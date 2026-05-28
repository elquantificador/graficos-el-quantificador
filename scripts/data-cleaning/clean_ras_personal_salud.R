# ============================================================
# clean_ras_personal_salud.R
# Limpia la serie nacional del Registro de Actividades y
# Recursos de Salud (RAS) para graficar la evolución del
# personal del MSP en Ecuador.
# Requiere: data/raw/ras/msp_serie_nac.rds
# Guarda:   data/processed/ras_personal_salud_nacional.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" scripts/data-cleaning/clean_ras_personal_salud.R
# ============================================================

library(dplyr)
library(tidyr)

in_path <- "data/raw/ras/msp_serie_nac.rds"
out_path <- "data/processed/ras_personal_salud_nacional.rds"

ras_personal_salud <- readRDS(in_path) %>%
  pivot_longer(
    cols = c(tmedicos, tenf, tobst, ttaps),
    names_to = "ocupacion",
    values_to = "total"
  ) %>%
  mutate(
    total = na_if(total, 0),
    ocupacion = factor(
      ocupacion,
      levels = c("tmedicos", "tenf", "tobst", "ttaps"),
      labels = c("Medicos", "Enfermeros", "Obstetrices", "TAPS")
    )
  ) %>%
  arrange(ocupacion, anio)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(ras_personal_salud, out_path)
message("Guardado: ", out_path)
