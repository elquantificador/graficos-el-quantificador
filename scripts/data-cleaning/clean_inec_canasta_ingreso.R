# ============================================================
# clean_inec_canasta_ingreso.R
# Prepara la serie de costo de canasta, ingreso familiar del hogar tipo y
# mediana observada de ingresos de hogares de referencia.
# Requiere: data/raw/inec_canasta_ingreso/canasta_vs_ingreso_karel.csv
# Guarda:   data/processed/inec_canasta_ingreso.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_inec_canasta_ingreso.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven", "readr", "purrr", "tibble"))

input_path <- "data/raw/inec_canasta_ingreso/canasta_vs_ingreso_karel.csv"
out_path <- "data/processed/inec_canasta_ingreso.rds"

required_columns <- c(
  "anio",
  "canasta_basica_usd",
  "ingreso_familiar_usd",
  "salario_basico_usd",
  "fuente_ingreso",
  "url_fuente"
)

raw_data <- readr::read_csv(
  input_path,
  col_types = readr::cols(
    anio = readr::col_integer(),
    canasta_basica_usd = readr::col_double(),
    ingreso_familiar_usd = readr::col_double(),
    salario_basico_usd = readr::col_double(),
    fuente_ingreso = readr::col_character(),
    url_fuente = readr::col_character()
  ),
  na = c("", "NA")
)

enemdu_specs <- tibble::tribble(
  ~anio, ~path, ~periodicidad,
  2018, "data/raw/enemdu/ENEMDU_PERSONAS_2018_12_hom.sav", "Diciembre 2018",
  2019, "data/raw/enemdu/enemdu_persona_201912.sav", "Diciembre 2019",
  2020, "data/raw/enemdu/enemdu_persona_2020_12.sav", "Diciembre 2020",
  2021, "data/raw/enemdu/enemdu_persona_2021_12.sav", "Diciembre 2021",
  2022, "data/raw/enemdu/enemdu_persona_2022_12.sav", "Diciembre 2022",
  2023, "data/raw/enemdu/enemdu_persona_2023_12.sav", "Diciembre 2023",
  2024, "data/raw/enemdu/enemdu_persona_2024_12.sav", "Diciembre 2024",
  2025, "data/raw/enemdu/enemdu_persona_2025_12.sav", "Diciembre 2025",
  2026, "data/raw/enemdu/enemdu_persona_2026_l_trimestre.sav", "I trimestre 2026"
)

missing_enemdu <- enemdu_specs |>
  dplyr::filter(!file.exists(.data$path))

if (nrow(missing_enemdu) > 0) {
  stop(
    "Faltan archivos ENEMDU para el ingreso per cápita observado: ",
    paste(missing_enemdu$path, collapse = ", ")
  )
}

weighted_median <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  x <- x[ok]
  w <- w[ok]
  ordering <- order(x)
  x <- x[ordering]
  w <- w[ordering]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

summarise_reference_income <- function(anio, path, periodicidad) {
  raw_enemdu <- haven::read_sav(path)
  id_hogar <- as.character(raw_enemdu$id_hogar)
  edad <- as.numeric(haven::zap_labels(raw_enemdu$p03))
  parentesco <- as.numeric(haven::zap_labels(raw_enemdu$p04))
  ingpc <- as.numeric(haven::zap_labels(raw_enemdu$ingpc))
  fexp <- as.numeric(haven::zap_labels(raw_enemdu$fexp))
  periodo <- as.character(raw_enemdu$periodo)

  reference_households <- tibble::tibble(
    id_hogar = id_hogar,
    edad = edad,
    parentesco = parentesco,
    ingpc = ingpc,
    fexp = fexp,
    periodo = periodo
  ) |>
    dplyr::filter(
      !is.na(.data$id_hogar),
      nzchar(.data$id_hogar),
      is.finite(.data$edad),
      is.finite(.data$parentesco),
      is.finite(.data$ingpc),
      .data$ingpc >= 0,
      is.finite(.data$fexp),
      .data$fexp > 0
    ) |>
    dplyr::group_by(.data$periodo, .data$id_hogar) |>
    dplyr::summarise(
      n_personas = dplyr::n(),
      n_adultos = sum(.data$edad >= 18),
      n_menores = sum(.data$edad < 18),
      n_jefes = sum(.data$parentesco == 1),
      n_conyuges = sum(.data$parentesco == 2),
      n_hijos = sum(.data$parentesco == 3 & .data$edad < 18),
      ingpc = dplyr::first(.data$ingpc),
      fexp = dplyr::first(.data$fexp),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$n_personas == 4,
      .data$n_adultos == 2,
      .data$n_menores == 2,
      .data$n_jefes == 1,
      .data$n_conyuges == 1,
      .data$n_hijos == 2
    )

  period_medians <- reference_households |>
    dplyr::group_by(.data$periodo) |>
    dplyr::summarise(
      ingreso_per_capita_mediano_usd = weighted_median(.data$ingpc, .data$fexp),
      hogares_referencia = dplyr::n(),
      .groups = "drop"
    )

  if (nrow(period_medians) == 0) {
    stop("No se encontraron hogares de referencia en ", path)
  }

  tibble::tibble(
    anio = anio,
    ingreso_per_capita_mediano_referencia_usd = mean(period_medians$ingreso_per_capita_mediano_usd),
    ingreso_familiar_mediano_referencia_equiv_usd = mean(period_medians$ingreso_per_capita_mediano_usd) * 4,
    hogares_referencia = sum(period_medians$hogares_referencia),
    periodos_enemdu = paste(period_medians$periodo, collapse = ", "),
    fuente_enemdu = periodicidad
  )
}

missing_columns <- setdiff(required_columns, names(raw_data))
if (length(missing_columns) > 0) {
  stop("Faltan columnas requeridas: ", paste(missing_columns, collapse = ", "))
}

ingpc_data <- purrr::pmap_dfr(
  enemdu_specs,
  summarise_reference_income
)

chart_data <- raw_data |>
  dplyr::select(dplyr::all_of(required_columns)) |>
  dplyr::left_join(ingpc_data, by = "anio") |>
  dplyr::mutate(
    cobertura = .data$ingreso_familiar_usd / .data$canasta_basica_usd,
    brecha_usd = .data$ingreso_familiar_usd - .data$canasta_basica_usd
  ) |>
  dplyr::arrange(.data$anio)

if (anyDuplicated(chart_data$anio) > 0) {
  stop("La serie contiene años duplicados.")
}

if (any(!is.finite(chart_data$cobertura)) || any(!is.finite(chart_data$brecha_usd))) {
  stop("La serie contiene valores no finitos en cobertura o brecha.")
}

if (any(!is.finite(chart_data$ingreso_per_capita_mediano_referencia_usd)) ||
    any(!is.finite(chart_data$ingreso_familiar_mediano_referencia_equiv_usd))) {
  stop("La serie ENEMDU contiene valores no finitos para los hogares de referencia.")
}

metadata <- list(
  source = paste(
    "INEC, Índice de Precios al Consumidor, boletines técnicos de enero de 2018 a 2026;",
    "ENEMDU, archivos de diciembre de 2018 a 2025 e I trimestre de 2026."
  ),
  original_author = "Karel Lázaro González Ruíz",
  original_repository = "https://github.com/karelgonzalezruiz/Concurso-Ecuador-Quantificado-2026-Participacion",
  methodology = paste(
    "Los valores del hogar tipo corresponden a cuatro miembros con 1,6 perceptores",
    "del salario básico. El ingreso observado es la mediana ponderada de la variable",
    "oficial ingpc de ENEMDU entre hogares de cuatro personas con dos adultos, dos",
    "hijos menores de 18 años, un jefe y un cónyuge. Se calcula por periodo y se",
    "promedia entre periodos disponibles. Para compararlo con la canasta y el hogar",
    "tipo, se multiplica por cuatro; 2026 usa enero-marzo."
  )
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(data = chart_data, metadata = metadata), out_path)
message("Guardado: ", out_path)
