# ============================================================
# clean_bce_comercio_colombia.R
# Author: Daniel Sanchez
# Purpose: Combina importaciones y exportaciones mensuales de Colombia.
# Inputs:  BCE monthly importation and exportation CSV files
# Outputs: data/processed/bce_comercio_colombia.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_bce_comercio_colombia.R
# ============================================================

# 0. Setup ----

source("scripts/packages.R")
ensure_packages(c("dplyr", "janitor", "lubridate", "readr"))

importaciones_path <- "data/raw/importaciones_colombia/bce_colombia_importaciones_mensuales_2024_2026.csv"
exportaciones_path <- "data/raw/importaciones_colombia/bce_colombia_exportaciones_mensuales_2024_2026.csv"
out_path <- "data/processed/bce_comercio_colombia.rds"

# 1. Read inputs ----

importaciones_raw <- read_csv(
  importaciones_path,
  show_col_types = FALSE
) |>
  clean_names()

exportaciones_raw <- read_csv(
  exportaciones_path,
  show_col_types = FALSE
) |>
  clean_names()

# 2. Prepare monthly trade data ----

importaciones_mensuales <- importaciones_raw |>
  transmute(
    fecha = ymd(paste0(.data$periodo, "-01")),
    serie = "Importaciones",
    valor_millones_usd = .data$fob_millones_usd
  )

exportaciones_mensuales <- exportaciones_raw |>
  transmute(
    fecha = ymd(paste0(.data$periodo, "-01")),
    serie = "Exportaciones",
    valor_millones_usd = .data$fob_millones_usd
  )

comercio_colombia_mensual <- bind_rows(
  importaciones_mensuales,
  exportaciones_mensuales
) |>
  arrange(.data$fecha, .data$serie)

# 3. Write output ----

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(comercio_colombia_mensual, out_path)
message("Guardado: ", out_path)
