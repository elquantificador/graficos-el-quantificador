# ============================================================
# clean_escolaridad_ingreso.R
# Carga la ENEMDU enero 2026 y prepara los datos para el
# análisis de ingreso laboral por nivel educativo.
# Guarda: data/processed/escolaridad_ingreso.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_escolaridad_ingreso.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven"))

df_raw <- read_sav("data/raw/enemdu/enemdu_persona_2026_01.sav")

df_clean <- df_raw %>%
  mutate(
    p03   = as.numeric(p03),
    p10a  = as.numeric(p10a),
    ingrl = as.numeric(ingrl),
    ingrl = na_if(ingrl, 999999),
    ingrl = if_else(ingrl == -1, NA_real_, ingrl),
    escolaridad = case_when(
      p10a %in% c(1, 2, 3, 4, 5) ~ "Menos de\nsecundaria",
      p10a %in% c(6, 7)          ~ "Secundaria",
      p10a == 8                  ~ "Tecnología",
      p10a == 9                  ~ "Universidad",
      p10a == 10                 ~ "Posgrado",
      TRUE                       ~ NA_character_
    ),
    escolaridad = factor(
      escolaridad,
      levels = c("Menos de\nsecundaria", "Secundaria", "Tecnología", "Universidad", "Posgrado")
    )
  ) %>%
  filter(p03 >= 15, !is.na(escolaridad), !is.na(ingrl), ingrl >= 0)

dir.create("data/processed", showWarnings = FALSE)
saveRDS(df_clean, "data/processed/escolaridad_ingreso.rds")
message("Guardado: data/processed/escolaridad_ingreso.rds  (", nrow(df_clean), " filas)")
