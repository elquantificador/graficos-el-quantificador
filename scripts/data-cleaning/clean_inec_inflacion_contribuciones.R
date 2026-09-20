# ============================================================
# Contribuciones a la inflación anual de Ecuador
# Author: Daniel Sanchez
# Purpose: Prepara cinco componentes de la incidencia anual del IPC nacional.
# Inputs: data/raw/ipc_inec_2026_06/Series Incidencias.zip
#         data/raw/ipc_inec_2026_06/Tabulados_y_series_historicas_CSV_2026_06.zip
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
out_path <- "data/processed/inec_inflacion_contribuciones.rds"
annual_csv_path <- "ipc_incid_nac_div_06_2026/2.INCID. ANUAL.csv"
inner_zip_name <- "Series Incidencias/ipc_incid_nac_div_06_2026.zip"
series_ipc_zip_name <- "Series IPC.zip"
index_inner_zip_name <- "Series IPC/ipc_ind_nac_reg_ciud_06_2026.zip"
index_csv_path <- "ipc_ind_nac_reg_ciud_06_2026/1. NACIONAL.csv"

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

processed <- list(
  incidencias = incidencias,
  inflacion_anual = inflacion_anual,
  gasolina = gasolina,
  metadata = list(
    source = "INEC, Índice de Precios al Consumidor",
    source_file = input_zip,
    source_period = "enero de 2016 a junio de 2026",
    base = "IPC base 2014 = 100",
    method = paste(
      "Incidencia anual por división; gasolina calculada con las",
      "ponderaciones e índices de los productos 07221248, 07221249 y",
      "07221250"
    )
  )
)

# 4. Write outputs ----

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(processed, out_path)
message("Guardado: ", out_path)
