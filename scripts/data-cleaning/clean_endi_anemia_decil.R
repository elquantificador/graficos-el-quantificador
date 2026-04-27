# ============================================================
# clean_endi_anemia_decil.R
# Carga y procesa los datos de la ENDI R2 para análisis de
# prevalencia de anemia en niñas/os de 6 a 23 meses por decil
# de ingreso per cápita del hogar.
#
# Los deciles se generan a partir del ingreso per cápita
# calculado como la suma de ingresos individuales del hogar
# (sección f1_s2) dividida por el número de miembros (f1_s1_1).
#
# Componentes del ingreso total individual:
#   Laboral:
#     - Salario neto          (f1_s2_12 − f1_s2_13)
#     - Ingreso neto negocio  (f1_s2_9 − f1_s2_11 + f1_s2_10_2)
#     - Especie laboral       (f1_s2_14_2)
#     - Ocupación secundaria  (f1_s2_15 + f1_s2_16_2)
#   No laboral:
#     - Intereses / arriendo  (f1_s2_17_2)
#     - Pensiones             (f1_s2_18_2)
#     - Donaciones nacionales (f1_s2_19_2)
#     - Remesas               (f1_s2_20_2)
#     - Bono Desarrollo Humano      (f1_s2_22)
#     - Bono Joaquín Gallegos Lara  (f1_s2_24)
#     - Bono de los 1000 días       (f1_s2_26)
#
# Nota: el código 999999 en variables de ingreso indica sin
# respuesta; se recodifica como NA y luego como 0 para la suma.
#
# Requiere: data/raw/endi_r2/BDD_ENDI_R2_f1_personas.rds
# Guarda:   data/processed/endi_r2_prev_anemia_decil.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_endi_anemia_decil.R
# ============================================================

library(haven)
library(dplyr)
library(tidyr)
library(survey)

path_rawdata_r2 <- "data/raw/endi_r2"
out_path        <- "data/processed/endi_r2_prev_anemia_decil.rds"

endi_r2_personas <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f1_personas.rds"))

# ---- 1. Calcular ingreso per cápita del hogar -------------------------

income_vars <- c(
  "f1_s2_9", "f1_s2_10_2", "f1_s2_11", "f1_s2_12", "f1_s2_13",
  "f1_s2_14_2", "f1_s2_15", "f1_s2_16_2", "f1_s2_17_2", "f1_s2_18_2",
  "f1_s2_19_2", "f1_s2_20_2", "f1_s2_22", "f1_s2_24", "f1_s2_26"
)

ingreso_personas <- endi_r2_personas %>%
  mutate(
    across(all_of(income_vars), ~ if_else(. >= 999999, NA_real_, as.numeric(.)))
  ) %>%
  mutate(
    ingreso_ind =
      # salario neto (bruto − descuentos IESS / impuestos)
      pmax(replace_na(f1_s2_12, 0) - replace_na(f1_s2_13, 0), 0) +
      # negocio / cuenta propia neto (ventas − gastos operativos + especie del negocio)
      pmax(replace_na(f1_s2_9,  0) - replace_na(f1_s2_11, 0), 0) +
      replace_na(f1_s2_10_2, 0) +
      # ingreso en especie laboral y ocupación secundaria
      replace_na(f1_s2_14_2, 0) +
      replace_na(f1_s2_15,   0) +
      replace_na(f1_s2_16_2, 0) +
      # ingresos no laborales
      replace_na(f1_s2_17_2, 0) +   # intereses / arriendo
      replace_na(f1_s2_18_2, 0) +   # pensiones
      replace_na(f1_s2_19_2, 0) +   # donaciones nacionales
      replace_na(f1_s2_20_2, 0) +   # remesas
      replace_na(f1_s2_22,   0) +   # BDH
      replace_na(f1_s2_24,   0) +   # Bono JGL
      replace_na(f1_s2_26,   0)     # Bono 1000 días
  )

# Sumar a nivel hogar y dividir por miembros
ingreso_hogar <- ingreso_personas %>%
  group_by(id_hogar) %>%
  summarise(
    ingreso_hogar = sum(ingreso_ind, na.rm = TRUE),
    n_miembros    = first(f1_s1_1),
    .groups = "drop"
  ) %>%
  mutate(ingreso_pc = ingreso_hogar / n_miembros)

endi_r2_personas <- endi_r2_personas %>%
  left_join(ingreso_hogar %>% select(id_hogar, ingreso_pc), by = "id_hogar")

# ---- 2. Filtrar subpoblación y crear deciles --------------------------

endi_r2_personas_valid <- endi_r2_personas %>%
  filter(!is.na(ane6_23_new), !is.na(ingreso_pc), ingreso_pc > 0) %>%
  mutate(
    decil = ntile(ingreso_pc, 10),
    decil = factor(decil, levels = 1:10, labels = paste0("Decil ", 1:10), ordered = TRUE)
  )

# ---- 3. Diseño muestral -----------------------------------------------

options(survey.lonely.psu = "adjust")

endi_r2_design <- svydesign(
  ids     = ~id_upm,
  strata  = ~estrato,
  weights = ~fexp,
  data    = endi_r2_personas_valid,
  nest    = TRUE
)

# ---- 4. Prevalencia de anemia por decil (con IC 95 %) -----------------

prev_anemia_decil <- svyby(
  ~ane6_23_new,
  ~decil,
  endi_r2_design,
  svymean,
  na.rm    = TRUE,
  vartype  = "ci"
) %>%
  as.data.frame() %>%
  transmute(
    decil          = decil,
    prev_anemia    = ane6_23_new,
    prev_anemia_low = ci_l,
    prev_anemia_upp = ci_u
  )

# ---- 5. Umbrales de ingreso y conteo de observaciones -----------------

income_thresholds <- endi_r2_personas_valid %>%
  group_by(decil) %>%
  summarise(
    ingreso_pc_min = min(ingreso_pc),
    ingreso_pc_max = max(ingreso_pc),
    n              = n(),
    .groups = "drop"
  )

# ---- 6. Tabla final ---------------------------------------------------

prev_anemia_decil <- prev_anemia_decil %>%
  left_join(income_thresholds, by = "decil")

print(prev_anemia_decil)

# ---- 7. Guardar -------------------------------------------------------

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(prev_anemia_decil, out_path)
message("Guardado: ", out_path)
