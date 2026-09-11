# ============================================================
# clean_inec_canasta_ingreso.R
# Prepara la distribución de ingreso de hogares con dos adultos y dos hijos.
# Requiere: data/raw/enighur/enighur_2025_hogares_2adultos_2hijos.csv
# Guarda:   data/processed/inec_canasta_ingreso.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_inec_canasta_ingreso.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr", "tibble"))

income_path <- "data/raw/enighur/enighur_2025_hogares_2adultos_2hijos.csv"
canasta_path <- "data/raw/inec_canasta_ingreso/canasta_vs_ingreso_karel.csv"
out_path <- "data/processed/inec_canasta_ingreso.rds"

income_data <- readr::read_csv(
  income_path,
  col_types = readr::cols(
    ingreso_monetario_hogar = readr::col_double(),
    fexp = readr::col_double()
  ),
  show_col_types = FALSE
) |>
  dplyr::filter(
    is.finite(.data$ingreso_monetario_hogar),
    .data$ingreso_monetario_hogar >= 0,
    is.finite(.data$fexp),
    .data$fexp > 0
  )

canasta_2025 <- readr::read_csv(
  canasta_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    anio = as.integer(.data$anio),
    canasta_basica_usd = as.numeric(.data$canasta_basica_usd),
    salario_basico_usd = as.numeric(.data$salario_basico_usd),
    ingreso_familiar_usd = as.numeric(.data$ingreso_familiar_usd)
  ) |>
  dplyr::filter(.data$anio == 2025) |>
  dplyr::slice_head(n = 1)

if (nrow(canasta_2025) != 1) {
  stop("Debe existir exactamente una fila de umbrales para 2025.")
}

if (nrow(income_data) == 0) {
  stop("No hay hogares válidos para la distribución de ingreso.")
}

weighted_quantile <- function(x, w, probability) {
  ordering <- order(x)
  x <- x[ordering]
  w <- w[ordering]
  x[which(cumsum(w) >= probability * sum(w))[1]]
}

sbu_1_6 <- 1.6 * canasta_2025$salario_basico_usd[[1]]
ingreso_familiar <- canasta_2025$ingreso_familiar_usd[[1]]
canasta_value <- canasta_2025$canasta_basica_usd[[1]]
total_weight <- sum(income_data$fexp)
share_below_canasta <- sum(
  income_data$fexp[income_data$ingreso_monetario_hogar < canasta_value]
) / total_weight

summary <- tibble::tibble(
  anio = 2025L,
  salario_basico_usd = canasta_2025$salario_basico_usd[[1]],
  sbu_1_6_usd = sbu_1_6,
  ingreso_familiar_usd = ingreso_familiar,
  canasta_basica_usd = canasta_value,
  share_below_canasta = share_below_canasta,
  share_above_canasta = 1 - share_below_canasta,
  hogares_muestra = nrow(income_data),
  hogares_ponderados = total_weight,
  ingreso_p25 = weighted_quantile(
    income_data$ingreso_monetario_hogar,
    income_data$fexp,
    0.25
  ),
  ingreso_mediano = weighted_quantile(
    income_data$ingreso_monetario_hogar,
    income_data$fexp,
    0.50
  ),
  ingreso_p75 = weighted_quantile(
    income_data$ingreso_monetario_hogar,
    income_data$fexp,
    0.75
  ),
  ingreso_p95 = weighted_quantile(
    income_data$ingreso_monetario_hogar,
    income_data$fexp,
    0.95
  )
)

metadata <- list(
  source = paste(
    "ENIGHUR 2024-2025, INEC;",
    "INEC, Informe Ejecutivo de Canastas Analíticas, diciembre de 2025."
  ),
  methodology = paste(
    "Se seleccionan hogares de cuatro personas con dos adultos de 18 años o más,",
    "dos hijos menores de 18 años, un representante del hogar y un cónyuge o",
    "conviviente. El ingreso es el ingreso corriente monetario mensual del hogar",
    "(ing_mon_cor), ponderado con el factor de expansión Fexp."
  ),
  source_income_extract = income_path,
  canasta_source = canasta_path
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(data = income_data, summary = summary, metadata = metadata),
  out_path
)
message("Guardado: ", out_path)
