# ============================================================
# clean_inec_mercado_laboral_pobreza_2026.R
# Prepara los indicadores del primer gráfico del artículo sobre el INEC.
# Requiere: data/raw/enemdu/inec_mercado_laboral_pobreza_2026.csv
# Guarda:   data/processed/inec_mercado_laboral_pobreza_2026.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_inec_mercado_laboral_pobreza_2026.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr", "tibble"))

input_path <- "data/raw/enemdu/inec_mercado_laboral_pobreza_2026.csv"
out_path <- "data/processed/inec_mercado_laboral_pobreza_2026.rds"

required_columns <- c("indicador", "valor", "grupo", "periodo", "orden")
raw_data <- readr::read_csv(
  input_path,
  col_types = readr::cols(
    indicador = readr::col_character(),
    valor = readr::col_double(),
    grupo = readr::col_character(),
    periodo = readr::col_character(),
    orden = readr::col_integer()
  ),
  show_col_types = FALSE
)

missing_columns <- setdiff(required_columns, names(raw_data))
if (length(missing_columns) > 0) {
  stop("Faltan columnas requeridas: ", paste(missing_columns, collapse = ", "))
}

if (any(!is.finite(raw_data$valor)) || any(raw_data$valor < 0)) {
  stop("Los valores deben ser porcentajes finitos y no negativos.")
}

data <- raw_data |>
  dplyr::mutate(
    grupo = factor(
      .data$grupo,
      levels = c("Mercado laboral", "Pobreza por ingresos")
    ),
    indicador = factor(
      .data$indicador,
      levels = c(
        "Ocupados en el sector informal",
        "Empleo adecuado",
        "Subempleo",
        "Desempleo",
        "Pobreza rural",
        "Pobreza urbana",
        "Pobreza por ingresos",
        "Pobreza extrema"
      )
    )
  ) |>
  dplyr::arrange(.data$grupo, .data$orden)

metadata <- list(
  source = paste(
    "INEC, ENEMDU mayo de 2026 (mercado laboral) y diciembre de 2025",
    "(pobreza por ingresos)."
  ),
  source_url = "https://www.ecuadorencifras.gob.ec/enemdu-2026/",
  methodology = paste(
    "Los valores se transcriben del primer gráfico del artículo",
    "'Apagón estadístico: ¿qué está pasando en el INEC en 2026?'",
    "y no constituyen una reestimación independiente."
  )
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(data = data, metadata = metadata), out_path)
message("Guardado: ", out_path)
