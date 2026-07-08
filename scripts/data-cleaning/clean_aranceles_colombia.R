# ============================================================
# clean_aranceles_colombia.R
# Prepara la comparación de arancel anterior y arancel de Noboa
# para las cinco subpartidas con mayor arancel aplicado.
# Requiere: data/raw/aranceles_colombia/COL_completo_con_arancel.xlsx
#           data/raw/aranceles_colombia/lista-de-ecuador.xlsx
# Guarda:   data/processed/aranceles_colombia.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_aranceles_colombia.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readxl", "stringr"))

imports_path <- "data/raw/aranceles_colombia/COL_completo_con_arancel.xlsx"
tariff_path <- "data/raw/aranceles_colombia/lista-de-ecuador.xlsx"
out_path <- "data/processed/aranceles_colombia.rds"

imports_df <- readxl::read_xlsx(imports_path)
tariff_df <- readxl::read_xlsx(tariff_path)

chart_df <- imports_df |>
  dplyr::mutate(
    anio = stringr::str_extract(.data[["Período"]], "^\\d{4}"),
    mes = stringr::str_extract(.data[["Período"]], "(?<=/)\\s*\\d{1,2}") |>
      stringr::str_trim(),
    fecha = as.Date(paste(anio, mes, "01", sep = "-"))
  ) |>
  dplyr::filter(mes == "03") |>
  dplyr::left_join(tariff_df, by = "codigo_subpartida") |>
  dplyr::filter(
    !is.na(Arancel),
    !is.na(Arancel_Base),
    !is.na(.data[["Descripción"]])
  ) |>
  dplyr::transmute(
    periodo = .data[["Período"]],
    fecha,
    codigo_subpartida,
    descripcion = stringr::str_squish(.data[["Descripción"]]),
    arancel_base = as.numeric(Arancel_Base),
    arancel_nuevo = as.numeric(Arancel) * 100,
    aumento_puntos = arancel_nuevo - arancel_base
  ) |>
  dplyr::arrange(dplyr::desc(arancel_nuevo)) |>
  dplyr::slice_head(n = 5)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(chart_df, out_path)
message("Guardado: ", out_path)
