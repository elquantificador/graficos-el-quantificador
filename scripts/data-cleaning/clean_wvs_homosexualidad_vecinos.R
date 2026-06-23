# ============================================================
# clean_wvs_homosexualidad_vecinos.R
# Calcula el porcentaje de personas que preferirían no tener
# a un homosexual como vecino, por sexo y año de encuesta.
# Requiere: data/raw/wvs/WVSEcuador.dta
# Guarda:   data/processed/wvs_homosexualidad_vecinos.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_wvs_homosexualidad_vecinos.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven"))

input_path <- "data/raw/wvs/WVSEcuador.dta"
out_path <- "data/processed/wvs_homosexualidad_vecinos.rds"

wvs <- haven::read_dta(input_path) |>
  dplyr::mutate(
    anio = haven::as_factor(S020),
    sexo = haven::as_factor(X001),
    sexo = dplyr::recode(as.character(sexo), Male = "Hombre", Female = "Mujer"),
    peso_muestral = as.numeric(S017)
  )

vecinos <- wvs |>
  dplyr::filter(!is.na(sexo), !is.na(anio), !is.na(peso_muestral), peso_muestral > 0) |>
  dplyr::group_by(anio, sexo) |>
  dplyr::summarise(
    porcentaje = stats::weighted.mean(A124_09 == 1, w = peso_muestral, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    anio = factor(as.character(anio), levels = c("2013", "2018")),
    sexo = factor(sexo, levels = c("Hombre", "Mujer"))
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(vecinos, out_path)
message("Guardado: ", out_path)
