# ============================================================
# clean_endi_anemia_decil.R
# Carga y procesa los datos de la ENDI R2 para analisis de
# prevalencia de anemia en ninas/os de 6 a 23 meses por decil
# de ingreso per capita del hogar.
#
# El ingreso per capita se calcula siguiendo la metodologia
# oficial del INEC (R2_Income_Syntax.R):
#   Ingreso laboral:
#     - ind  = f1_s2_9 + f1_s2_10_2 - f1_s2_11  (negocio/cta. propia)
#     - asal = f1_s2_12 + f1_s2_13 + f1_s2_14_2 (asalariado)
#     - ila1 = ind + asal  (si ila1 < 0 -> ila = ila2)
#     - ila2 = f1_s2_15 + f1_s2_16_2             (actividad secundaria)
#   Ingreso no laboral:
#     - icap   = f1_s2_17_2  (intereses / arriendo)
#     - itrans = f1_s2_18_2 + f1_s2_19_2 + f1_s2_20_2 + f1_s2_22
#   Variable de control x:
#     - x = 1: ingreso principal desconocido -> ii = NA
#     - x = 2: secundaria desconocida pero hay ingreso no laboral -> ii = inla
#   Cero ingresos -> NA a nivel individual y de hogar.
#   Tamano del hogar = miembros observados en la base.
#   Deciles sobre log(ipcf) con wtd.quantile ponderado por fexp.
#
# Requiere: data/raw/endi_r2/BDD_ENDI_R2_f1_personas.rds
# Guarda:   data/processed/endi_r2_prev_anemia_decil.rds
# ============================================================
# Ejecutar desde la raiz del proyecto:
#   Rscript scripts/data-cleaning/clean_endi_anemia_decil.R
# ============================================================

library(haven)
library(dplyr)
library(Hmisc)
library(survey)

path_rawdata_r2 <- "data/raw/endi_r2"
out_path        <- "data/processed/endi_r2_prev_anemia_decil.rds"

endi_r2_personas <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f1_personas.rds"))

# ---- 1. Calcular ingreso per capita del hogar -------------------------

var <- c("f1_s2_9", "f1_s2_10_2", "f1_s2_11",
         "f1_s2_12", "f1_s2_13",  "f1_s2_14_2",
         "f1_s2_15", "f1_s2_16_2","f1_s2_17_2",
         "f1_s2_18_2", "f1_s2_19_2", "f1_s2_20_2",
         "f1_s2_22")

# Formato numerico
endi_r2_personas <- endi_r2_personas %>%
  mutate(across(all_of(var), as.numeric))

# Homologar variantes de 9s a 999999
for (i in var) {
  endi_r2_personas[[i]] <- case_when(
    endi_r2_personas[[i]] == 9999999  ~ 999999,
    endi_r2_personas[[i]] == 99999999 ~ 999999,
    TRUE ~ endi_r2_personas[[i]]
  )
}

# Variable de control x para incoherencias de ingreso
endi_r2_personas <- endi_r2_personas %>%
  mutate(x = case_when(
    (f1_s2_9 == 999999 | f1_s2_12 == 999999) |
      (f1_s2_15 == 999999 & f1_s2_16_2 == 999999 &
         is.na(f1_s2_17_2) & is.na(f1_s2_18_2) & is.na(f1_s2_19_2) &
         is.na(f1_s2_20_2) & is.na(f1_s2_22)) ~ 1,
    (f1_s2_15 == 999999 & f1_s2_16_2 == 999999) &
      (!is.na(f1_s2_17_2) | !is.na(f1_s2_18_2) | !is.na(f1_s2_19_2) |
         !is.na(f1_s2_20_2) | !is.na(f1_s2_22)) ~ 2,
    TRUE ~ 0
  ))

# 999999 -> NA
for (i in var) {
  endi_r2_personas[[i]] <- case_when(
    endi_r2_personas[[i]] == 999999 ~ NA_real_,
    TRUE ~ endi_r2_personas[[i]]
  )
}

# Marcar tambien 999 / 9999 / 99999 como incoherentes
for (i in var) {
  endi_r2_personas <- endi_r2_personas %>%
    mutate(x = case_when(
      (!!sym(i) == 999 | !!sym(i) == 9999 | !!sym(i) == 99999) ~ 1,
      TRUE ~ x
    ))
}

# Ingreso laboral -- actividad principal
endi_r2_personas <- endi_r2_personas %>%
  mutate(f1_s2_11 = -f1_s2_11)   # gastos de negocio se restan

endi_r2_personas <- endi_r2_personas %>%
  rowwise() %>%
  mutate(
    ind  = sum(c(f1_s2_9, f1_s2_10_2, f1_s2_11), na.rm = TRUE),  # negocio/cta. propia
    asal = sum(c(f1_s2_12, f1_s2_13, f1_s2_14_2), na.rm = TRUE)  # asalariado
  ) %>%
  mutate(ila1 = sum(c(ind, asal), na.rm = TRUE)) %>%
  ungroup()

# Ingreso laboral -- actividad secundaria
endi_r2_personas <- endi_r2_personas %>%
  rowwise() %>%
  mutate(ila2 = sum(c(f1_s2_15, f1_s2_16_2), na.rm = TRUE)) %>%
  ungroup()

# Ingreso laboral total; si ila1 < 0 (gastos > ventas) usar solo secundaria
endi_r2_personas <- endi_r2_personas %>%
  rowwise() %>%
  mutate(ila = sum(c(ila1, ila2), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ila = case_when(ila1 < 0 ~ ila2, TRUE ~ ila))

# Ingreso no laboral
endi_r2_personas <- endi_r2_personas %>%
  mutate(
    icap    = f1_s2_17_2,
    ipens   = f1_s2_18_2,
    ilocal  = f1_s2_19_2,
    iextr   = f1_s2_20_2,
    isocial = f1_s2_22
  ) %>%
  rowwise() %>%
  mutate(
    itrans = sum(c(ipens, ilocal, iextr, isocial), na.rm = TRUE),
    inla   = sum(c(icap, itrans), na.rm = TRUE)
  ) %>%
  ungroup()

# Poner a NA todas las fuentes si x == 1 (ingreso principal desconocido)
var1 <- c("ind", "asal", "ila", "icap", "ipens",
          "ilocal", "iextr", "isocial", "itrans", "inla")

for (i in var1) {
  endi_r2_personas <- endi_r2_personas %>%
    mutate(!!sym(i) := case_when(x == 1 ~ NA_real_, TRUE ~ !!sym(i)))
}

# Ingreso individual total
endi_r2_personas <- endi_r2_personas %>%
  rowwise() %>%
  mutate(ii = sum(c(ila, inla), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    ii = case_when(x == 1 ~ NA_real_, TRUE ~ ii),  # incoherente -> NA
    ii = case_when(x == 2 ~ inla,     TRUE ~ ii),  # sin secundaria, con no laboral
    ii = case_when(ii == 0 ~ NA_real_, TRUE ~ ii)  # cero -> NA
  )

# Ingreso familiar (suma de ii por hogar; cero -> NA)
endi_r2_personas <- endi_r2_personas %>%
  group_by(id_hogar) %>%
  mutate(ih = sum(ii, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ih = case_when(ih == 0 ~ NA_real_, TRUE ~ ih))

# Tamano del hogar (miembros observados) e ingreso per capita
endi_r2_personas <- endi_r2_personas %>%
  mutate(nump = 1) %>%
  group_by(id_hogar) %>%
  mutate(
    hsize = sum(nump),
    ipcf  = ih / hsize
  ) %>%
  ungroup() %>%
  mutate(lipcf = log(ipcf))

# ---- 2. Filtrar subpoblacion y crear deciles --------------------------

endi_r2_personas_valid <- endi_r2_personas %>%
  filter(!is.na(ane6_23_new), !is.na(lipcf))

# Deciles sobre log(ipcf) con cuantiles ponderados por fexp
decil_breaks <- wtd.quantile(
  endi_r2_personas_valid$lipcf,
  weights = endi_r2_personas_valid$fexp,
  probs   = seq(0, 1, length = 11),
  na.rm   = TRUE
)

endi_r2_personas_valid <- endi_r2_personas_valid %>%
  mutate(
    decil = as.numeric(cut(lipcf, breaks = decil_breaks, include.lowest = TRUE)),
    decil = factor(decil, levels = 1:10, labels = paste0("Decil ", 1:10), ordered = TRUE)
  )

# ---- 3. Diseno muestral -----------------------------------------------

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
  na.rm   = TRUE,
  vartype = "ci"
) %>%
  as.data.frame() %>%
  transmute(
    decil           = decil,
    prev_anemia     = ane6_23_new,
    prev_anemia_low = ci_l,
    prev_anemia_upp = ci_u
  )

# ---- 5. Umbrales de ingreso y conteo de observaciones -----------------

income_thresholds <- endi_r2_personas_valid %>%
  group_by(decil) %>%
  summarise(
    ingreso_pc_min = min(ipcf, na.rm = TRUE),
    ingreso_pc_max = max(ipcf, na.rm = TRUE),
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
