# ============================================================
# clean_ipc_ciudades_leonor.R
# Limpia las series oficiales del IPC de nueve ciudades para la
# visualizacion sobre costo de vida.
# Requiere: data/raw/ipc_inec_2026_06/indices/.../*.csv
# Guarda:   data/processed/ipc_ciudades_inec_2026_06.rds
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "purrr", "readr", "stringr", "tibble", "tidyr"))

raw_dir <- "data/raw/ipc_inec_2026_06/indices/ipc_ind_nac_reg_ciud_06_2026"
out_path <- "data/processed/ipc_ciudades_inec_2026_06.rds"

city_files <- c(
  "4. Guayaquil.csv" = "Guayaquil",
  "5. Esmeraldas.csv" = "Esmeraldas",
  "6. Machala.csv" = "Machala",
  "7. Manta.csv" = "Manta",
  "8. Sto. Domingo.csv" = "Santo Domingo",
  "9. Quito.csv" = "Quito",
  "10. Loja.csv" = "Loja",
  "11. Cuenca.csv" = "Cuenca",
  "12. Ambato.csv" = "Ambato"
)

month_number <- c(
  ene = 1L, feb = 2L, mar = 3L, abr = 4L, may = 5L, jun = 6L,
  jul = 7L, ago = 8L, sep = 9L, oct = 10L, nov = 11L, dic = 12L
)

read_city <- function(file_name, city_name) {
  path <- file.path(raw_dir, file_name)
  if (!file.exists(path)) {
    stop("No existe el archivo crudo: ", path)
  }

  raw <- readr::read_csv(
    path,
    skip = 4,
    locale = readr::locale(encoding = "Latin1"),
    show_col_types = FALSE,
    name_repair = "minimal"
  )

  month_cols <- names(raw)[stringr::str_detect(
    names(raw),
    "^(ene|feb|mar|abr|may|jun|jul|ago|sep|oct|nov|dic)-[0-9]{2}$"
  )]

  if (length(month_cols) < 2) {
    stop("No se reconocieron columnas mensuales en: ", path)
  }

  selected <- raw |>
    dplyr::filter(
      .data[[names(raw)[1]]] == "General",
      .data[[names(raw)[4]]] == "GENERAL"
    )

  if (nrow(selected) != 1) {
    stop("La fila General no es unica en: ", path)
  }

  selected |>
    dplyr::select(dplyr::all_of(month_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "periodo",
      values_to = "ipc"
    ) |>
    dplyr::mutate(
      ciudad = city_name,
      mes = stringr::str_sub(.data$periodo, 1, 3),
      anio = 2000L + as.integer(stringr::str_sub(.data$periodo, 5, 6)),
      fecha = as.Date(sprintf(
        "%d-%02d-01",
        .data$anio,
        unname(month_number[.data$mes])
      )),
      ipc = readr::parse_number(
        as.character(.data$ipc),
        locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
      )
    ) |>
    dplyr::select(.data$ciudad, .data$fecha, .data$ipc)
}

ipc <- purrr::map2_dfr(names(city_files), unname(city_files), read_city) |>
  dplyr::filter(
    .data$fecha >= as.Date("2021-01-01"),
    .data$fecha <= as.Date("2026-06-01")
  ) |>
  dplyr::arrange(.data$ciudad, .data$fecha)

expected_dates <- seq.Date(as.Date("2021-01-01"), as.Date("2026-06-01"), by = "month")
validation <- ipc |>
  dplyr::group_by(.data$ciudad) |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    fecha_inicio = min(.data$fecha),
    fecha_fin = max(.data$fecha),
    .groups = "drop"
  )

if (nrow(ipc) != 9 * length(expected_dates) || any(!is.finite(ipc$ipc))) {
  stop("La tabla limpia no tiene 9 ciudades x 66 meses validos.")
}

if (any(validation$observaciones != length(expected_dates)) ||
    any(validation$fecha_inicio != min(expected_dates)) ||
    any(validation$fecha_fin != max(expected_dates))) {
    stop("La cobertura temporal no coincide con enero de 2021 a junio de 2026.")
}

ranking <- ipc |>
  dplyr::group_by(.data$ciudad) |>
  dplyr::arrange(.data$fecha, .by_group = TRUE) |>
  dplyr::summarise(
    ipc_enero_2021 = dplyr::first(.data$ipc),
    ipc_junio_2026 = dplyr::last(.data$ipc),
    variacion_acumulada_pct = (.data$ipc_junio_2026 / .data$ipc_enero_2021 - 1) * 100,
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(.data$variacion_acumulada_pct)) |>
  dplyr::mutate(puesto = dplyr::row_number()) |>
  dplyr::select(
    .data$puesto, .data$ciudad, .data$ipc_enero_2021,
    .data$ipc_junio_2026, .data$variacion_acumulada_pct
  )

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(
    ipc = ipc,
    ranking = ranking,
    validation = validation,
    source = "INEC, Indice de Precios al Consumidor, series IPC nacional/regional/ciudad, corte junio 2026",
    source_url = "https://www.ecuadorencifras.gob.ec/indice-de-precios-al-consumidor-2026/"
  ),
  out_path
)

message("Guardado: ", out_path)
message("Filas: ", nrow(ipc), "; ciudades: ", nrow(validation))
