# ============================================================
# endi_cleaning.R
# Carga y procesa los datos de la ENDI R2 para análisis de
# prevalencia de desnutrición crónica por etnia.
# Requiere: data/endi_r2/BDD_ENDI_R2_f1_personas.rds
# Guarda:   data/processed/endi_r2_prev_dcronica_etnia.rds
# ============================================================

library(haven)
library(tidyverse)
library(survey)
library(srvyr)

# Ruta
path_rawdata_r2 <- "data/endi_r2"

# Cargar datos
endi_r2_personas <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f1_personas.rds"))

# Crear variables demográficas ==========
endi_r2_personas <- endi_r2_personas %>%
  mutate(
    sexo = as_factor(f1_s1_2),
    etnia = as_factor(etnia)
  )

# Crear diseño de encuesta ==========
endi_r2_svy <- as_survey_design(
  endi_r2_personas,
  ids = id_upm,
  weights = fexp,
  strata = estrato,
  nest = TRUE
)

# Prevalencia de desnutrición ==========
r2_prev_dcronica_etnia <- endi_r2_svy %>%
  group_by(etnia) %>%
  summarize(
    prev_dcronica = survey_mean(dcronica_2, vartype = c("ci"), na.rm = TRUE)
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(r2_prev_dcronica_etnia, "data/processed/endi_r2_prev_dcronica_etnia.rds")
message("Guardado: data/processed/endi_r2_prev_dcronica_etnia.rds")
