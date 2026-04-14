# ============================================================
# clean_salario_ipc.R
# Lee las series mensuales de sueldo promedio e IPC nacional
# y prepara las tablas procesadas para los gráficos de sueldo
# real vs. nominal y sueldo vs. costo de vida.
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

salarios_prom <- salarios_raw |>
  dplyr::filter(Desagregaciones == "Total sueldo corriente medio") |>
  tidyr::pivot_longer(
    cols = tidyselect::matches("^[a-z]{3}\\.-\\d{2}$"),
    names_to = "fecha",
    values_to = "sueldo"
  ) |>
  dplyr::mutate(
    fecha = parse_month_col(fecha),
    sueldo = readr::parse_number(as.character(sueldo), locale = readr::locale(decimal_mark = ".", grouping_mark = ","))
  ) |>
  tidyr::drop_na(fecha) |>
  dplyr::select(fecha, sueldo) |>
  dplyr::arrange(fecha)

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

ipc_key_series <- ipc_raw |>
  dplyr::filter(
    (Nivel == "General") |
      (Nivel == "División" & `Descripción CCIF` %in% c(
        "Alimentos y bebidas no alcohólicas",
        "Transporte"
      )) |
      (Nivel == "Grupo" & `Descripción CCIF` %in% c(
        "Alquileres efectivos del alojamiento"
      )) |
      (Nivel == "Subclase" & `Descripción CCIF` %in% c(
        "Combustibles y lubricantes para equipo de transporte personal"
      ))
  ) |>
  dplyr::select(Nivel, `Descripción CCIF`, Ponderación, tidyselect::matches("^[a-z]{3}-\\d{2}$")) |>
  tidyr::pivot_longer(
    cols = tidyselect::matches("^[a-z]{3}-\\d{2}$"),
    names_to = "fecha",
    values_to = "ipc"
  ) |>
  dplyr::mutate(
    fecha = parse_month_col(fecha),
    ipc = readr::parse_number(as.character(ipc), locale = readr::locale(decimal_mark = ".", grouping_mark = ",")),
    categoria = dplyr::case_when(
      Nivel == "General" ~ "IPC general",
      `Descripción CCIF` == "Alimentos y bebidas no alcohólicas" ~ "Alimentos y bebidas",
      `Descripción CCIF` == "Transporte" ~ "Transporte",
      `Descripción CCIF` == "Alquileres efectivos del alojamiento" ~ "Alojamiento",
      `Descripción CCIF` == "Combustibles y lubricantes para equipo de transporte personal" ~ "Gasolina y lubricantes"
    )
  ) |>
  tidyr::drop_na(fecha) |>
  dplyr::select(fecha, categoria, ipc, Ponderación) |>
  dplyr::arrange(categoria, fecha)

salario_real <- salarios_prom |>
  dplyr::left_join(ipc_general, by = "fecha") |>
  dplyr::filter(lubridate::year(fecha) >= 2019) |>
  dplyr::mutate(sueldo_real = sueldo / ipc * 100)

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

ipc_yoy <- ipc_key_series |>
  dplyr::group_by(categoria) |>
  dplyr::arrange(fecha, .by_group = TRUE) |>
  dplyr::mutate(
    valor = ((ipc / dplyr::lag(ipc, 12)) - 1) * 100
  ) |>
  dplyr::ungroup() |>
  dplyr::select(fecha, categoria, valor)

sueldo_yoy <- salarios_prom |>
  dplyr::arrange(fecha) |>
  dplyr::mutate(
    valor = ((sueldo / dplyr::lag(sueldo, 12)) - 1) * 100,
    categoria = "Sueldo promedio"
  ) |>
  dplyr::select(fecha, categoria, valor)

ipc_index_plot <- ipc_key_series |>
  dplyr::filter(lubridate::year(fecha) >= 2019) |>
  dplyr::group_by(categoria) |>
  dplyr::arrange(fecha, .by_group = TRUE) |>
  dplyr::mutate(
    valor = ipc / dplyr::first(ipc) * 100
  ) |>
  dplyr::ungroup() |>
  dplyr::select(fecha, categoria, valor)

sueldo_index_plot <- salarios_prom |>
  dplyr::filter(lubridate::year(fecha) >= 2019) |>
  dplyr::arrange(fecha) |>
  dplyr::mutate(
    categoria = "Sueldo promedio",
    valor = sueldo / dplyr::first(sueldo) * 100
  ) |>
  dplyr::select(fecha, categoria, valor)

salario_vs_ipc_index <- dplyr::bind_rows(ipc_index_plot, sueldo_index_plot) |>
  dplyr::filter(
    fecha <= max(salarios_prom$fecha, na.rm = TRUE)
  ) |>
  tidyr::drop_na(valor)

salario_vs_ipc <- dplyr::bind_rows(ipc_yoy, sueldo_yoy) |>
  dplyr::filter(
    lubridate::year(fecha) >= 2019,
    fecha <= max(salarios_prom$fecha, na.rm = TRUE)
  ) |>
  tidyr::drop_na(valor)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(
    salario_real = salario_real,
    salario_sector_real = salario_sector_real,
    salario_vs_ipc = salario_vs_ipc,
    salario_vs_ipc_index = salario_vs_ipc_index
  ),
  "data/processed/salario_ipc_series.rds"
)

message("Guardado: data/processed/salario_ipc_series.rds")
