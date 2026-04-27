# ============================================================
# clean_ef_epi_job.R
# Lee los datos de EF EPI Ecuador (2025) y prepara la tabla
# de puntajes por función laboral para el gráfico.
# Requiere: data/raw/ef_epi/ef_epi_ecuador_extracted.xlsx
# Guarda:   data/processed/ef_epi_job.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_ef_epi_job.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("readxl", "dplyr"))

df <- read_excel("data/raw/ef_epi/ef_epi_ecuador_extracted.xlsx", sheet = "Job_Functions_Exact") |>
  select(job_function, score, proficiency_band) |>
  mutate(
    proficiency_band = factor(
      proficiency_band,
      levels = c("Very low", "Low", "Moderate", "High"),
      ordered = TRUE
    )
  )

dir.create("data/processed", showWarnings = FALSE)
saveRDS(df, "data/processed/ef_epi_job.rds")
message("Guardado: data/processed/ef_epi_job.rds  (", nrow(df), " filas)")
