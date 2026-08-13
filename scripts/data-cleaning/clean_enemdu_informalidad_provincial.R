# ============================================================
# clean_enemdu_informalidad_provincial.R
# Author: Juan Diego Sotomayor Jiménez; adaptación de El Quantificador
# Purpose: Limpia y valida las tasas provinciales de informalidad y empleo no remunerado.
# Requiere: data/raw/enemdu/enemdu_anual_2025_provincial.csv
# Guarda:   data/processed/enemdu_informalidad_provincial.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_informalidad_provincial.R
# ============================================================

# 0. Setup ----
source("scripts/packages.R")
ensure_packages(c("dplyr", "readr"))
# set.seed(42)

input_path <- "data/raw/enemdu/enemdu_anual_2025_provincial.csv"
out_path <- "data/processed/enemdu_informalidad_provincial.rds"
expected_provinces <- 24L
national_informality_2025 <- 51.5
tolerance <- 0.05
expected_correlation_2025 <- 0.741
expected_correlation_2024 <- 0.717
correlation_tolerance <- 0.01

required_columns <- c(
  "provincia",
  "region_natural",
  "informalidad_2024",
  "informalidad_2025",
  "no_remunerado_2024",
  "no_remunerado_2025"
)

rate_columns <- c(
  "informalidad_2024",
  "informalidad_2025",
  "no_remunerado_2024",
  "no_remunerado_2025"
)

# 1. Load data ----
datos_crudos <- readr::read_csv(input_path, show_col_types = FALSE)

missing_columns <- setdiff(required_columns, names(datos_crudos))
if (length(missing_columns) > 0) {
  stop("Faltan columnas requeridas: ", paste(missing_columns, collapse = ", "))
}

if (nrow(datos_crudos) != expected_provinces) {
  stop("Se esperaban 24 provincias y se encontraron ", nrow(datos_crudos), ".")
}

if (anyDuplicated(datos_crudos$provincia) > 0) {
  stop("El archivo contiene provincias duplicadas.")
}

# 2. Clean and validate ----
datos_provinciales <- datos_crudos |>
  transmute(
    provincia = recode(
      provincia,
      "Bolivar" = "Bolívar",
      "Galapagos" = "Galápagos",
      "Los Rios" = "Los Ríos",
      "Manabi" = "Manabí",
      "Sucumbios" = "Sucumbíos"
    ),
    region_natural = recode(region_natural, "Amazonia" = "Amazonía"),
    across(all_of(rate_columns), as.numeric)
  )

invalid_rates <- datos_provinciales |>
  summarise(across(all_of(rate_columns), ~ any(is.na(.x) | .x < 0 | .x > 100))) |>
  unlist(use.names = FALSE) |>
  any()

if (invalid_rates) {
  stop("Las tasas deben ser numéricas, no faltantes y estar entre 0 y 100.")
}

expected_values <- tibble::tribble(
  ~provincia, ~informalidad_2025_esperada, ~no_remunerado_2025_esperado,
  "Morona Santiago", 87.4, 42.0,
  "Galápagos", 16.2, 3.2,
  "Guayas", 51.9, 2.9,
  "Azuay", 47.2, 9.9
)

anchor_check <- expected_values |>
  inner_join(
    datos_provinciales,
    by = join_by(provincia),
    multiple = "error",
    unmatched = c("error", "drop")
  )

anchors_are_valid <- anchor_check |>
  summarise(
    informalidad_ok = all(
      abs(informalidad_2025 - informalidad_2025_esperada) <= tolerance
    ),
    no_remunerado_ok = all(
      abs(no_remunerado_2025 - no_remunerado_2025_esperado) <= tolerance
    )
  ) |>
  unlist(use.names = FALSE) |>
  all()

if (!anchors_are_valid) {
  stop("Los valores de control no coinciden con las tablas 8 y 9 del boletín.")
}

correlation_2025 <- cor(
  datos_provinciales$informalidad_2025,
  datos_provinciales$no_remunerado_2025,
  use = "complete.obs"
)

correlation_2024 <- cor(
  datos_provinciales$informalidad_2024,
  datos_provinciales$no_remunerado_2024,
  use = "complete.obs"
)

if (
  abs(correlation_2025 - expected_correlation_2025) > correlation_tolerance ||
    abs(correlation_2024 - expected_correlation_2024) > correlation_tolerance
) {
  stop("Las correlaciones provinciales no coinciden con los valores verificados.")
}

chart_data <- list(
  provincias = datos_provinciales,
  metadata = list(
    source = "INEC, ENEMDU anual 2025, Boletín Técnico Nro. 03-2026",
    source_tables = c("Tabla 8", "Tabla 9"),
    national_informality_2025 = national_informality_2025,
    correlation_2024 = correlation_2024,
    correlation_2025 = correlation_2025,
    original_project = "jdsotomayorjimenez/elquantificador_informalidad_ecu"
  )
)

# 3. Export ----
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(chart_data, out_path)
message("Guardado: ", out_path)
invisible(sessionInfo())
