# ============================================================
# clean_femicidios.R
# Carga y limpia los datos de femicidios de la Fiscalía
# General del Estado.
# Guarda: data/processed/femicidios.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_femicidios.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr"))

muertes_fem <- read.csv("data/raw/fiscalia/muertes_fem_fiscalia_2026.csv") %>%
  rename(año = year, cantidad = value, tipo = category) %>%
  filter(año < 2026) %>%
  mutate(tipo = case_when(
    tipo == "Femicidios" ~ "Femicidios",
    TRUE                 ~ "Otras muertes"
  ))

dir.create("data/processed", showWarnings = FALSE)
saveRDS(muertes_fem, "data/processed/femicidios.rds")
message("Guardado: data/processed/femicidios.rds  (", nrow(muertes_fem), " filas)")
