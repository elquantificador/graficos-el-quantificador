# ============================================================
# clean_salario_ipc.R
# Lee las series mensuales de sueldo e IPC nacional
# y prepara la tabla procesada para el gráfico de sueldo
# público/privado, real vs. nominal.
# Requiere:
#   - data/REESS Indicadores Laborales_Empleo_01_2026.xlsx (hoja 4_2_3)
#   - data/ipc_ind_nac_reg_ciud_03_2026.xlsx (hoja 1. NACIONAL)
# Guarda:
#   - data/processed/salario_ipc_series.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_salario_ipc.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "stringr", "lubridate", "readr", "readxl"))

parse_month_col <- function(x) {
  x |>
    stringr::str_replace_all("\\.", "") |>
    stringr::str_replace_all(c(
      "ene" = "jan",
      "abr" = "apr",
      "ago" = "aug",
      "dic" = "dec"
    )) |>
    (\(value) lubridate::dmy(paste0("01-", value)))()
}

repair_blank_names <- function(df) {
  blank_idx <- which(is.na(names(df)) | names(df) == "")
  if (length(blank_idx) > 0) {
    names(df)[blank_idx] <- paste0("extra_", seq_along(blank_idx))
  }
  df
}

salarios_sheet <- readxl::read_excel(
  "data/REESS Indicadores Laborales_Empleo_01_2026.xlsx",
  sheet = "4_2_3",
  col_names = FALSE,
  .name_repair = "minimal"
)

salary_header <- salarios_sheet |>
  (\(x) x[5, , drop = FALSE])() |>
  unlist(use.names = FALSE) |>
  as.character()

salary_header[1] <- "extra_1"
salary_header[2] <- "Desagregaciones"

salarios_raw <- salarios_sheet |>
  (\(x) x[6:nrow(x), , drop = FALSE])() |>
  stats::setNames(salary_header) |>
  repair_blank_names()

ipc_sheet <- readxl::read_excel(
  "data/ipc_ind_nac_reg_ciud_03_2026.xlsx",
  sheet = "1. NACIONAL",
  col_names = FALSE,
  .name_repair = "minimal"
)

ipc_header <- ipc_sheet |>
  (\(x) x[3, , drop = FALSE])() |>
  unlist(use.names = FALSE) |>
  as.character()

ipc_raw <- ipc_sheet |>
  (\(x) x[4:nrow(x), , drop = FALSE])() |>
  stats::setNames(ipc_header) |>
  repair_blank_names()

salarios_sector <- salarios_raw |>
  dplyr::filter(Desagregaciones %in% c("1_Privado", "2_Público")) |>
  tidyr::pivot_longer(
    cols = tidyselect::matches("^[a-z]{3}\\.-\\d{2}$"),
    names_to = "fecha",
    values_to = "sueldo_nominal"
  ) |>
  dplyr::mutate(
    fecha = parse_month_col(fecha),
    sueldo_nominal = readr::parse_number(
      as.character(sueldo_nominal),
      locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    ),
    sector = dplyr::recode(
      Desagregaciones,
      "1_Privado" = "Empleo privado",
      "2_Público" = "Empleo público"
    )
  ) |>
  tidyr::drop_na(fecha, sueldo_nominal) |>
  dplyr::select(fecha, sector, sueldo_nominal) |>
  dplyr::arrange(sector, fecha)

ipc_general <- ipc_raw |>
  dplyr::filter(Nivel == "General") |>
  dplyr::select(Nivel, `Descripción CCIF`, Ponderación, tidyselect::matches("^[a-z]{3}-\\d{2}$")) |>
  tidyr::pivot_longer(
    cols = tidyselect::matches("^[a-z]{3}-\\d{2}$"),
    names_to = "fecha",
    values_to = "ipc"
  ) |>
  dplyr::mutate(
    fecha = parse_month_col(fecha),
    ipc = readr::parse_number(as.character(ipc), locale = readr::locale(decimal_mark = ".", grouping_mark = ",")),
    categoria = "IPC general"
  ) |>
  tidyr::drop_na(fecha) |>
  dplyr::select(fecha, categoria, ipc, Ponderación) |>
  dplyr::arrange(fecha)

salario_sector_real <- salarios_sector |>
  dplyr::left_join(
    ipc_general |>
      dplyr::select(fecha, ipc),
    by = "fecha"
  ) |>
  dplyr::filter(lubridate::year(fecha) >= 2019) |>
  dplyr::mutate(
    sueldo_real = sueldo_nominal / ipc * 100
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(
    salario_sector_real = salario_sector_real
  ),
  "data/processed/salario_ipc_series.rds"
)

message("Guardado: data/processed/salario_ipc_series.rds")
