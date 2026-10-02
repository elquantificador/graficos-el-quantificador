# ============================================================
# clean_bce_importaciones_colombia.R
# Author: Daniel Sanchez
# Purpose: Prepara importaciones mensuales y comparaciones anuales.
# Inputs:  data/raw/importaciones_colombia/bce_colombia_importaciones_mensuales_2024_2026.csv
# Outputs: data/processed/bce_importaciones_colombia.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_bce_importaciones_colombia.R
# ============================================================

# 0. Setup ----

source("scripts/packages.R")
ensure_packages(c("dplyr", "janitor", "lubridate", "readr"))

input_path <- "data/raw/importaciones_colombia/bce_colombia_importaciones_mensuales_2024_2026.csv"
out_path <- "data/processed/bce_importaciones_colombia.rds"

# 1. Read inputs ----

importaciones_raw <- read_csv(
  input_path,
  show_col_types = FALSE
) |>
  clean_names()

# 2. Prepare monthly data ----

importaciones_mensuales <- importaciones_raw |>
  mutate(
    fecha = ymd(paste0(.data$periodo, "-01")),
    anio = year(.data$fecha),
    mes = month(.data$fecha)
  ) |>
  arrange(.data$fecha) |>
  mutate(
    fob_acumulado_anual_millones_usd = cumsum(.data$fob_millones_usd),
    cif_acumulado_anual_millones_usd = cumsum(.data$cif_millones_usd),
    .by = anio
  )

# 3. Calculate annual comparisons ----

valores_anio_anterior <- importaciones_mensuales |>
  transmute(
    fecha,
    fob_anio_anterior_millones_usd = .data$fob_millones_usd,
    cif_anio_anterior_millones_usd = .data$cif_millones_usd,
    fob_acumulado_anio_anterior_millones_usd =
      .data$fob_acumulado_anual_millones_usd,
    cif_acumulado_anio_anterior_millones_usd =
      .data$cif_acumulado_anual_millones_usd
  )

importaciones_comparadas <- importaciones_mensuales |>
  mutate(fecha_anio_anterior = .data$fecha - years(1)) |>
  left_join(
    valores_anio_anterior,
    by = join_by(fecha_anio_anterior == fecha),
    relationship = "many-to-one",
    multiple = "error",
    unmatched = "drop"
  ) |>
  mutate(
    fob_cambio_interanual_millones_usd =
      .data$fob_millones_usd - .data$fob_anio_anterior_millones_usd,
    cif_cambio_interanual_millones_usd =
      .data$cif_millones_usd - .data$cif_anio_anterior_millones_usd,
    fob_cambio_interanual_pct =
      (.data$fob_millones_usd / .data$fob_anio_anterior_millones_usd - 1) * 100,
    cif_cambio_interanual_pct =
      (.data$cif_millones_usd / .data$cif_anio_anterior_millones_usd - 1) * 100,
    fob_cambio_acumulado_interanual_millones_usd =
      .data$fob_acumulado_anual_millones_usd -
        .data$fob_acumulado_anio_anterior_millones_usd,
    cif_cambio_acumulado_interanual_millones_usd =
      .data$cif_acumulado_anual_millones_usd -
        .data$cif_acumulado_anio_anterior_millones_usd,
    fob_cambio_acumulado_interanual_pct =
      (.data$fob_acumulado_anual_millones_usd /
        .data$fob_acumulado_anio_anterior_millones_usd - 1) * 100,
    cif_cambio_acumulado_interanual_pct =
      (.data$cif_acumulado_anual_millones_usd /
        .data$cif_acumulado_anio_anterior_millones_usd - 1) * 100
  )

# 4. Write outputs ----

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(importaciones_comparadas, out_path)
message("Guardado: ", out_path)

sessionInfo()
