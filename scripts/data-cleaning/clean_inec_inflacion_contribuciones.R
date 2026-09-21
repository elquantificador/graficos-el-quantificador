# ============================================================
# Contribuciones a la inflación anual de Ecuador
# Author: Daniel Sanchez
# Purpose: Prepara siete componentes de la incidencia anual del IPC nacional.
# Inputs: paquetes del IPC del INEC con base 2014 y con la nueva base 2026
# Outputs: data/processed/inec_inflacion_contribuciones.rds
# ============================================================

# 0. Setup ----

source("scripts/packages.R")
ensure_packages(c("dplyr", "lubridate", "readr", "stringr", "tidyr"))

input_zip <- "data/raw/ipc_inec_2026_06/Series Incidencias.zip"
index_input_zip <- paste0(
  "data/raw/ipc_inec_2026_06/",
  "Tabulados_y_series_historicas_CSV_2026_06.zip"
)
new_base_input_zip <- paste0(
  "data/raw/ipc_inec_2026_08/",
  "Tabulados_y_series_historicas_CSV_2026_08.zip"
)
empalme_input_zip <- paste0(
  "data/raw/ipc_inec_2026_08/",
  "Series_empalmadas_2026_07.zip"
)
out_path <- "data/processed/inec_inflacion_contribuciones.rds"
annual_csv_path <- "ipc_incid_nac_div_06_2026/2.INCID. ANUAL.csv"
inner_zip_name <- "Series Incidencias/ipc_incid_nac_div_06_2026.zip"
series_ipc_zip_name <- "Series IPC.zip"
index_inner_zip_name <- "Series IPC/ipc_ind_nac_reg_ciud_06_2026.zip"
index_csv_path <- "ipc_ind_nac_reg_ciud_06_2026/1. NACIONAL.csv"

find_zip_member <- function(zip_path, pattern) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  match <- members[stringr::str_detect(members, pattern)][1]
  if (is.na(match)) {
    stop("No se encontró un archivo en ", zip_path, " que coincida con ", pattern)
  }
  match
}

# 1. Read inputs ----

utils::unzip(
  input_zip,
  files = inner_zip_name,
  exdir = tempdir(),
  overwrite = TRUE
)
inner_zip_path <- file.path(tempdir(), inner_zip_name)

raw_annual <- readr::read_csv(
  unz(inner_zip_path, annual_csv_path),
  skip = 4,
  col_names = FALSE,
  locale = readr::locale(encoding = "Latin1"),
  na = c("", "NA"),
  show_col_types = FALSE
)

utils::unzip(
  index_input_zip,
  files = series_ipc_zip_name,
  exdir = tempdir(),
  overwrite = TRUE
)
series_ipc_zip_path <- file.path(tempdir(), series_ipc_zip_name)
utils::unzip(
  series_ipc_zip_path,
  files = index_inner_zip_name,
  exdir = tempdir(),
  overwrite = TRUE
)
index_inner_zip_path <- file.path(tempdir(), index_inner_zip_name)

raw_index <- readr::read_csv(
  unz(index_inner_zip_path, index_csv_path),
  skip = 4,
  col_names = FALSE,
  locale = readr::locale(encoding = "Latin1"),
  na = c("", "NA"),
  show_col_types = FALSE
)

new_base_index_zip_name <- find_zip_member(
  new_base_input_zip,
  "^DECON_02_IPC_ind_var_CCIF_dom_2026_08\\.zip$"
)
utils::unzip(
  new_base_input_zip,
  files = new_base_index_zip_name,
  exdir = tempdir(),
  overwrite = TRUE
)
new_base_index_zip_path <- file.path(
  tempdir(),
  basename(new_base_index_zip_name)
)
new_base_index_csv_path <- find_zip_member(
  new_base_index_zip_path,
  "Nacional_CCIF_2026_08/1\\."
)
raw_new_base_index <- readr::read_csv(
  unz(new_base_index_zip_path, new_base_index_csv_path),
  skip = 6,
  col_names = FALSE,
  locale = readr::locale(encoding = "UTF-8"),
  na = c("", "NA"),
  show_col_types = FALSE
)

empalme_csv_zip_name <- find_zip_member(
  empalme_input_zip,
  "Tabulados_series_empalmadas_CSV\\.zip$"
)
utils::unzip(
  empalme_input_zip,
  files = empalme_csv_zip_name,
  exdir = tempdir(),
  overwrite = TRUE
)
empalme_csv_zip_path <- file.path(
  tempdir(),
  basename(empalme_csv_zip_name)
)
empalme_index_csv_path <- find_zip_member(
  empalme_csv_zip_path,
  "Nacional_CCIF_2026_07/1\\."
)
raw_empalme_index <- readr::read_csv(
  unz(empalme_csv_zip_path, empalme_index_csv_path),
  skip = 6,
  col_names = FALSE,
  locale = readr::locale(encoding = "UTF-8"),
  na = c("", "NA"),
  show_col_types = FALSE
)

# 2. Prepare data ----

header_values <- raw_annual[1, ] |>
  unlist(use.names = FALSE) |>
  as.character()
last_data_column <- max(
  which(!is.na(header_values) & stringr::str_squish(header_values) != "")
)
raw_annual <- raw_annual[, seq_len(last_data_column)]

month_labels <- header_values[5:last_data_column]
month_names <- stringr::str_replace_all(month_labels, "-", "_")
column_names <- c(
  "nivel",
  "ponderacion",
  "codigo_ccif",
  "division",
  month_names
)

annual_data <- raw_annual[-1, ] |>
  stats::setNames(column_names)

month_index <- tibble::tibble(
  month_name = month_names,
  fecha = seq(
    from = lubridate::ymd("2016-01-01"),
    by = "month",
    length.out = length(month_names)
  )
)

# 3. Calculate estimates ----

incidencias <- annual_data |>
  dplyr::filter(
    .data$nivel == "División",
    stringr::str_detect(.data$codigo_ccif, "^\\d{2}$")
  ) |>
  dplyr::select(
    ponderacion,
    codigo_ccif,
    division,
    dplyr::all_of(month_names)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(month_names),
    names_to = "month_name",
    values_to = "incidencia_anual"
  ) |>
  dplyr::mutate(
    ponderacion = readr::parse_number(
      .data$ponderacion,
      locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    ),
    incidencia_anual = readr::parse_number(
      .data$incidencia_anual,
      locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    )
  ) |>
  dplyr::left_join(
    month_index,
    by = dplyr::join_by(month_name),
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
  dplyr::select(
    fecha,
    codigo_ccif,
    division,
    ponderacion,
    incidencia_anual
  ) |>
  dplyr::arrange(.data$fecha, .data$codigo_ccif)

inflacion_anual <- annual_data |>
  dplyr::filter(.data$division == "Variación Anual Nacional") |>
  dplyr::select(dplyr::all_of(month_names)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(month_names),
    names_to = "month_name",
    values_to = "inflacion_anual"
  ) |>
  dplyr::mutate(
    inflacion_anual = readr::parse_number(
      .data$inflacion_anual,
      locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    )
  ) |>
  dplyr::left_join(
    month_index,
    by = dplyr::join_by(month_name),
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
  dplyr::select(fecha, inflacion_anual) |>
  dplyr::arrange(.data$fecha)

index_header_values <- raw_index[1, ] |>
  unlist(use.names = FALSE) |>
  as.character()
index_last_data_column <- max(
  which(
    !is.na(index_header_values) &
      stringr::str_squish(index_header_values) != ""
  )
)
raw_index <- raw_index[, seq_len(index_last_data_column)]

index_month_labels <- index_header_values[5:index_last_data_column]
index_month_names <- stringr::str_replace_all(index_month_labels, "-", "_")
index_column_names <- c(
  "nivel",
  "ponderacion",
  "codigo_ccif",
  "producto",
  index_month_names
)

index_data <- raw_index[-1, ] |>
  stats::setNames(index_column_names)

index_month <- tibble::tibble(
  month_name = index_month_names,
  fecha = seq(
    from = lubridate::ymd("2015-01-01"),
    by = "month",
    length.out = length(index_month_names)
  )
)

parse_inec_month <- function(labels) {
  month_numbers <- c(
    ene = 1,
    feb = 2,
    mar = 3,
    abr = 4,
    may = 5,
    jun = 6,
    jul = 7,
    ago = 8,
    sep = 9,
    oct = 10,
    nov = 11,
    dic = 12
  )
  parts <- stringr::str_split_fixed(
    stringr::str_to_lower(labels),
    "-",
    2
  )
  as.Date(sprintf(
    "20%s-%02d-01",
    parts[, 2],
    unname(month_numbers[parts[, 1]])
  ))
}

parse_new_base_index <- function(raw_data) {
  header_values <- raw_data[1, ] |>
    unlist(use.names = FALSE) |>
    as.character()
  last_data_column <- max(
    which(
      !is.na(header_values) &
        stringr::str_squish(header_values) != ""
    )
  )
  raw_data <- raw_data[, seq_len(last_data_column)]
  month_names <- stringr::str_replace_all(
    header_values[5:last_data_column],
    "-",
    "_"
  )
  column_names <- c(
    "nivel",
    "codigo_ccif",
    "division",
    "ponderacion",
    month_names
  )
  raw_data[-1, ] |>
    stats::setNames(column_names) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(month_names),
      names_to = "month_name",
      values_to = "indice"
    ) |>
    dplyr::mutate(
      fecha = parse_inec_month(
        stringr::str_replace_all(.data$month_name, "_", "-")
      ),
      ponderacion = readr::parse_number(
        .data$ponderacion,
        locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
      ),
      indice = readr::parse_number(
        .data$indice,
        locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
      )
    ) |>
    dplyr::select(
      nivel,
      codigo_ccif,
      division,
      ponderacion,
      fecha,
      indice
    )
}

new_base_index <- parse_new_base_index(raw_new_base_index)
empalme_index <- parse_new_base_index(raw_empalme_index)

gasoline_codes <- c("07221248", "07221249", "07221250")
gasolina <- index_data |>
  dplyr::filter(
    .data$nivel == "Producto",
    .data$codigo_ccif %in% gasoline_codes
  ) |>
  dplyr::select(
    codigo_ccif,
    ponderacion,
    dplyr::all_of(index_month_names)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(index_month_names),
    names_to = "month_name",
    values_to = "indice"
  ) |>
  dplyr::mutate(
    ponderacion = as.numeric(.data$ponderacion),
    indice = as.numeric(.data$indice)
  ) |>
  dplyr::left_join(
    index_month,
    by = dplyr::join_by(month_name),
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
  dplyr::arrange(.data$codigo_ccif, .data$fecha) |>
  dplyr::group_by(.data$codigo_ccif) |>
  dplyr::mutate(
    indice_12_meses = dplyr::lag(.data$indice, 12),
    incidencia_producto = .data$ponderacion * (
      .data$indice / .data$indice_12_meses - 1
    ) * 100
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(.data$fecha >= lubridate::ymd("2016-01-01")) |>
  dplyr::group_by(.data$fecha) |>
  dplyr::summarise(
    incidencia_gasolina = sum(
      .data$incidencia_producto,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$fecha)

new_base_start <- lubridate::ymd("2026-07-01")
new_base_index_target <- new_base_index |>
  dplyr::filter(.data$fecha >= new_base_start)
new_target_dates <- new_base_index_target |>
  dplyr::distinct(.data$fecha)
empalme_index_lag <- empalme_index |>
  dplyr::mutate(fecha = .data$fecha %m+% lubridate::years(1)) |>
  dplyr::semi_join(new_target_dates, by = "fecha")

new_incidencias <- new_base_index_target |>
  dplyr::filter(.data$nivel == "División") |>
  dplyr::left_join(
    empalme_index_lag |>
      dplyr::filter(.data$nivel == "División") |>
      dplyr::select(
        nivel,
        codigo_ccif,
        fecha,
        indice_lag = indice
      ),
    by = dplyr::join_by(nivel, codigo_ccif, fecha),
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
  dplyr::mutate(
    incidencia_anual = .data$ponderacion * (
      .data$indice / .data$indice_lag - 1
    ) * 100
  ) |>
  dplyr::select(
    fecha,
    codigo_ccif,
    division,
    ponderacion,
    incidencia_anual
  ) |>
  dplyr::arrange(.data$fecha, .data$codigo_ccif)

new_gasoline_weights <- new_base_index_target |>
  dplyr::filter(
    .data$nivel == "Producto",
    .data$codigo_ccif %in% c("0722201", "0722202")
  ) |>
  dplyr::group_by(.data$fecha) |>
  dplyr::summarise(
    peso_gasolina = sum(.data$ponderacion, na.rm = TRUE),
    .groups = "drop"
  )

new_class_transport_fuels <- new_base_index_target |>
  dplyr::filter(
    .data$nivel == "Clase",
    .data$codigo_ccif == "0722"
  ) |>
  dplyr::left_join(
    empalme_index_lag |>
      dplyr::filter(
        .data$nivel == "Clase",
        .data$codigo_ccif == "0722"
      ) |>
      dplyr::select(fecha, indice_lag = indice),
    by = dplyr::join_by(fecha),
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
  dplyr::left_join(
    new_gasoline_weights,
    by = dplyr::join_by(fecha),
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
  dplyr::transmute(
    fecha = .data$fecha,
    incidencia_gasolina = .data$ponderacion * (
      .data$indice / .data$indice_lag - 1
    ) * 100 * (.data$peso_gasolina / .data$ponderacion)
  ) |>
  dplyr::arrange(.data$fecha)

new_inflacion_anual <- new_base_index_target |>
  dplyr::filter(
    .data$nivel == "General",
    .data$codigo_ccif == "Total"
  ) |>
  dplyr::left_join(
    empalme_index_lag |>
      dplyr::filter(
        .data$nivel == "General",
        .data$codigo_ccif == "Total"
      ) |>
      dplyr::select(fecha, indice_lag = indice),
    by = dplyr::join_by(fecha),
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
  dplyr::transmute(
    fecha = .data$fecha,
    inflacion_anual = (
      .data$indice / .data$indice_lag - 1
    ) * 100
  ) |>
  dplyr::arrange(.data$fecha)

inflacion_anual_total <- dplyr::bind_rows(
  inflacion_anual,
  new_inflacion_anual
) |>
  dplyr::arrange(.data$fecha)

processed <- list(
  incidencias = incidencias,
  incidencias_nueva = new_incidencias,
  inflacion_anual = inflacion_anual_total,
  gasolina = gasolina,
  gasolina_nueva = new_class_transport_fuels,
  metadata = list(
    source = "INEC, Índice de Precios al Consumidor",
    source_file = c(
      input_zip,
      index_input_zip,
      new_base_input_zip,
      empalme_input_zip
    ),
    source_period = "enero de 2016 a agosto de 2026",
    base = paste(
      "IPC base 2014 = 100 hasta junio de 2026;",
      "IPC base julio 2025 - junio 2026 = 100 desde julio de 2026"
    ),
    method = paste(
      "Incidencia anual por división; gasolina calculada con las",
      "ponderaciones e índices de los productos 07221248, 07221249 y",
      "07221250 hasta junio de 2026. Desde julio de 2026, la gasolina",
      "se aproxima distribuyendo la incidencia de la clase 0722 según el",
      "peso de los productos 0722201 y 0722202, porque la serie empalmada",
      "del INEC no publica índices históricos a nivel de producto."
    )
  )
)

# 4. Write outputs ----

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(processed, out_path)
message("Guardado: ", out_path)
