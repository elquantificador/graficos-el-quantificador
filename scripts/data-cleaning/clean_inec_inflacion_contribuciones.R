# ==============================================================================
# Contribuciones a la inflación anual de Ecuador
# Fuente: INEC, IPC base 2014 y base julio 2025-junio 2026
# ==============================================================================

# 0. Setup ----

source("scripts/packages.R")
ensure_packages(c("dplyr", "lubridate", "readr", "stringr", "tidyr"))

input_zip <- file.path(
  "data/raw/ipc_inec_2026_08",
  "Tabulados_y_series_historicas_CSV_2026_08.zip"
)
old_incidence_zip <- file.path(
  "data/raw/ipc_inec_2026_06",
  "Series Incidencias.zip"
)
empalme_input_zip <- file.path(
  "data/raw/ipc_inec_2026_08",
  "Series_empalmadas_2026_07.zip"
)
out_path <- "data/processed/inec_inflacion_contribuciones.rds"
start_date <- lubridate::ymd("2022-01-01")
end_date <- lubridate::ymd("2026-08-01")

# 1. Read inputs ----

find_zip_member <- function(zip_path, pattern) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  matches <- members[stringr::str_detect(members, pattern)]

  if (length(matches) != 1) {
    stop("No se encontró un único archivo esperado en: ", zip_path)
  }

  matches
}

read_nested_csv <- function(outer_zip, inner_zip_pattern, csv_pattern) {
  outer_member <- find_zip_member(outer_zip, inner_zip_pattern)
  outer_dir <- tempfile("ipc_outer_")
  inner_dir <- tempfile("ipc_inner_")
  dir.create(outer_dir)
  dir.create(inner_dir)
  on.exit(unlink(c(outer_dir, inner_dir), recursive = TRUE), add = TRUE)

  utils::unzip(outer_zip, files = outer_member, exdir = outer_dir)
  inner_zip <- file.path(outer_dir, outer_member)
  extraction_status <- system2("tar", c("-xf", inner_zip, "-C", inner_dir))
  if (extraction_status != 0) {
    stop("No se pudo extraer: ", inner_zip)
  }

  extracted_files <- list.files(inner_dir, recursive = TRUE, full.names = TRUE)
  csv_paths <- extracted_files[stringr::str_detect(
    stringr::str_replace_all(extracted_files, "\\\\", "/"),
    csv_pattern
  )]
  if (length(csv_paths) != 1) {
    stop("No se encontró un único CSV esperado en: ", inner_zip)
  }

  readr::read_csv(
    csv_paths,
    skip = 6,
    col_names = FALSE,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )
}

parse_inec_month <- function(labels) {
  month_numbers <- c(
    ene = 1, feb = 2, mar = 3, abr = 4, may = 5, jun = 6,
    jul = 7, ago = 8, sep = 9, oct = 10, nov = 11, dic = 12
  )
  parts <- stringr::str_split_fixed(stringr::str_to_lower(labels), "-", 2)

  as.Date(sprintf(
    "20%s-%02d-01",
    parts[, 2],
    unname(month_numbers[parts[, 1]])
  ))
}

parse_index <- function(raw_data) {
  header_values <- raw_data[1, ] |>
    unlist(use.names = FALSE) |>
    as.character()
  last_data_column <- max(
    which(!is.na(header_values) & stringr::str_squish(header_values) != "")
  )
  raw_data <- raw_data[, seq_len(last_data_column)]

  month_names <- stringr::str_replace_all(
    header_values[5:last_data_column],
    "-",
    "_"
  )
  column_names <- if (stringr::str_detect(
    stringr::str_to_lower(header_values[2]),
    "ponder"
  )) {
    c("nivel", "ponderacion", "codigo_ccif", "division", month_names)
  } else {
    c("nivel", "codigo_ccif", "division", "ponderacion", month_names)
  }

  raw_data[-1, ] |>
    stats::setNames(column_names) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(month_names),
      names_to = "month_name",
      values_to = "indice"
    ) |>
    dplyr::transmute(
      nivel = stringr::str_squish(.data$nivel),
      codigo_ccif = stringr::str_squish(.data$codigo_ccif),
      division = stringr::str_squish(.data$division),
      ponderacion = readr::parse_number(
        .data$ponderacion,
        locale = readr::locale(decimal_mark = ".")
      ),
      fecha = parse_inec_month(stringr::str_replace_all(.data$month_name, "_", "-")),
      indice = readr::parse_number(
        .data$indice,
        locale = readr::locale(decimal_mark = ".")
      )
    ) |>
    dplyr::filter(!is.na(.data$fecha), !is.na(.data$indice))
}

read_old_annual_incidence <- function(zip_path) {
  inner_zip_name <- "Series Incidencias/ipc_incid_nac_div_06_2026.zip"
  annual_csv_name <- "ipc_incid_nac_div_06_2026/2.INCID. ANUAL.csv"
  extraction_dir <- tempfile("ipc_old_incidence_")
  dir.create(extraction_dir)
  on.exit(unlink(extraction_dir, recursive = TRUE), add = TRUE)

  utils::unzip(zip_path, files = inner_zip_name, exdir = extraction_dir)
  inner_zip <- file.path(extraction_dir, inner_zip_name)
  raw_data <- readr::read_csv(
    unz(inner_zip, annual_csv_name),
    skip = 4,
    col_names = FALSE,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "Latin1")
  )
  header_values <- raw_data[1, ] |>
    unlist(use.names = FALSE) |>
    as.character()
  last_data_column <- max(
    which(!is.na(header_values) & stringr::str_squish(header_values) != "")
  )
  month_names <- stringr::str_replace_all(
    header_values[5:last_data_column], "-", "_"
  )
  column_names <- c(
    "nivel", "ponderacion", "codigo", "division", month_names
  )

  data <- raw_data[-1, seq_len(last_data_column)] |>
    stats::setNames(column_names)
  month_index <- tibble::tibble(
    month_name = month_names,
    fecha = seq(
      from = lubridate::ymd("2016-01-01"),
      by = "month",
      length.out = length(month_names)
    )
  )

  list(
    incidencias = data |>
      dplyr::filter(
        .data$nivel == "División",
        stringr::str_detect(.data$codigo, "^[0-9]{2}$")
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(month_names),
        names_to = "month_name",
        values_to = "incidencia_anual"
      ) |>
      dplyr::mutate(
        incidencia_anual = readr::parse_number(.data$incidencia_anual),
        fecha = month_index$fecha[match(.data$month_name, month_index$month_name)]
      ) |>
      dplyr::filter(.data$fecha >= start_date, .data$fecha < lubridate::ymd("2026-07-01")) |>
      dplyr::select(.data$fecha, .data$codigo, .data$division, .data$incidencia_anual),
    inflacion_anual = data |>
      dplyr::filter(.data$division == "Variación Anual Nacional") |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(month_names),
        names_to = "month_name",
        values_to = "inflacion_anual"
      ) |>
      dplyr::mutate(
        inflacion_anual = readr::parse_number(.data$inflacion_anual),
        fecha = month_index$fecha[match(.data$month_name, month_index$month_name)]
      ) |>
      dplyr::filter(.data$fecha >= start_date, .data$fecha < lubridate::ymd("2026-07-01")) |>
      dplyr::select(.data$fecha, .data$inflacion_anual)
  )
}

calculate_contributions <- function(indices, weights, general_indices) {
  indices |>
    dplyr::left_join(weights, by = c("nivel", "codigo_ccif")) |>
    dplyr::left_join(general_indices, by = "fecha") |>
    dplyr::mutate(
      incidencia_anual = .data$ponderacion * (
        .data$indice - .data$indice_12_meses
      ) / .data$indice_general_12_meses * 100
    ) |>
    dplyr::filter(
      .data$fecha >= start_date,
      .data$fecha <= end_date,
      !is.na(.data$incidencia_anual)
    )
}

# 2. Prepare official series ----

# Desde julio de 2026 el INEC publica una nueva canasta. Las series empalmadas
# conservan la continuidad histórica; el archivo mensual aporta julio y agosto.
indices_empalmados_raw <- read_nested_csv(
  outer_zip = empalme_input_zip,
  inner_zip_pattern = "Tabulados_series_empalmadas_CSV\\.zip$",
  csv_pattern = "Nacional_CCIF_2026_07/1\\..*\\.csv$"
)
indices_nueva_base_raw <- read_nested_csv(
  outer_zip = input_zip,
  inner_zip_pattern = "DECON_02_IPC_ind_var_CCIF_dom_2026_08\\.zip$",
  csv_pattern = "Nacional_CCIF_2026_08/1\\..*\\.csv$"
)

indices_empalmados <- parse_index(indices_empalmados_raw)
indices_nueva_base <- parse_index(indices_nueva_base_raw)
old_annual <- read_old_annual_incidence(old_incidence_zip)

ponderaciones <- indices_nueva_base |>
  dplyr::filter(.data$fecha == max(.data$fecha)) |>
  dplyr::distinct(.data$nivel, .data$codigo_ccif, .data$ponderacion)

indices <- dplyr::bind_rows(
  indices_empalmados |>
    dplyr::select(.data$nivel, .data$codigo_ccif, .data$division, .data$fecha, .data$indice),
  indices_nueva_base |>
    dplyr::select(.data$nivel, .data$codigo_ccif, .data$division, .data$fecha, .data$indice)
) |>
  dplyr::arrange(.data$nivel, .data$codigo_ccif, .data$fecha) |>
  dplyr::distinct(.data$nivel, .data$codigo_ccif, .data$fecha, .keep_all = TRUE) |>
  dplyr::group_by(.data$nivel, .data$codigo_ccif) |>
  dplyr::mutate(indice_12_meses = dplyr::lag(.data$indice, 12)) |>
  dplyr::ungroup()

# 3. Calculate contributions ----

inflacion_calculada <- indices |>
  dplyr::filter(.data$nivel == "General", .data$codigo_ccif == "Total") |>
  dplyr::transmute(
    fecha = .data$fecha,
    inflacion_anual = (.data$indice / .data$indice_12_meses - 1) * 100,
    indice_general_12_meses = .data$indice_12_meses
  ) |>
  dplyr::filter(
    .data$fecha >= start_date,
    .data$fecha <= end_date,
    !is.na(.data$inflacion_anual)
  )

general_indices <- inflacion_calculada |>
  dplyr::select(.data$fecha, .data$indice_general_12_meses)

incidencias_calculadas <- indices |>
  dplyr::filter(
    .data$nivel == "División",
    stringr::str_detect(.data$codigo_ccif, "^[0-9]{2}$")
  ) |>
  calculate_contributions(
    weights = ponderaciones,
    general_indices = general_indices
  ) |>
  dplyr::transmute(
    fecha = .data$fecha,
    codigo = .data$codigo_ccif,
    division = .data$division,
    incidencia_anual = .data$incidencia_anual
  ) |>
  dplyr::filter(.data$fecha >= lubridate::ymd("2026-07-01")) |>
  dplyr::group_by(.data$fecha) |>
  dplyr::mutate(
    incidencia_anual = .data$incidencia_anual *
      inflacion_calculada$inflacion_anual[
        match(.data$fecha, inflacion_calculada$fecha)
      ] / sum(.data$incidencia_anual)
  ) |>
  dplyr::ungroup()

incidencias <- dplyr::bind_rows(
  old_annual$incidencias,
  incidencias_calculadas
)

inflacion_anual <- dplyr::bind_rows(
  old_annual$inflacion_anual,
  inflacion_calculada |>
    dplyr::filter(.data$fecha >= lubridate::ymd("2026-07-01")) |>
    dplyr::select(.data$fecha, .data$inflacion_anual)
) |>
  dplyr::arrange(.data$fecha)

# La clase 0722 incluye combustibles y lubricantes para equipo de transporte
# personal. Sustituye la estimación previa de gasolina por una categoría INEC.
combustibles <- indices |>
  dplyr::filter(.data$nivel == "Clase", .data$codigo_ccif == "0722") |>
  calculate_contributions(
    weights = ponderaciones,
    general_indices = general_indices
  ) |>
  dplyr::transmute(
    fecha = .data$fecha,
    incidencia_anual = .data$incidencia_anual
  ) |>
  dplyr::filter(.data$fecha >= lubridate::ymd("2026-07-01"))

# 4. Write outputs ----

processed <- list(
  incidencias = incidencias,
  inflacion_anual = inflacion_anual |>
    dplyr::select(.data$fecha, .data$inflacion_anual),
  combustibles = combustibles,
  metadata = list(
    source = "INEC, IPC: series empalmadas y tabulados de agosto de 2026",
    source_period = "enero de 2022 a agosto de 2026",
    base_change_date = lubridate::ymd("2026-07-01"),
    method = paste(
      "Contribuciones anuales calculadas con índices empalmados y",
      "ponderaciones de la canasta vigente desde julio de 2026,",
      "normalizadas para reconciliar con la inflación general publicada.",
      "La clase 0722 se muestra solo desde julio de 2026."
    )
  )
)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(processed, out_path)
message("Guardado: ", out_path)
