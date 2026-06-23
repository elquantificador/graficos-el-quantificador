# ============================================================
# clean_wvs_homosexualidad_padres.R
# Calcula la distribución de respuestas a la afirmación sobre
# si las parejas homosexuales son tan buenos padres.
# Requiere: data/raw/wvs/WVSEcuador.dta
# Guarda:   data/processed/wvs_homosexualidad_padres.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_wvs_homosexualidad_padres.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven"))

input_path <- "data/raw/wvs/WVSEcuador.dta"
out_path <- "data/processed/wvs_homosexualidad_padres.rds"

wvs <- haven::read_dta(input_path) |>
  dplyr::mutate(
    anio = haven::as_factor(S020),
    respuesta = haven::as_factor(D081),
    peso_muestral = as.numeric(S017)
  )

respuesta_levels <- c(
  "No sabe",
  "Totalmente de acuerdo",
  "De acuerdo",
  "Indiferente",
  "En desacuerdo",
  "Totalmente en desacuerdo"
)

respuesta_labels_raw <- c(
  "Missing; Not available",
  "Not asked",
  "Not applicable",
  "No answer"
)

padres <- wvs |>
  dplyr::filter(anio == "2018", !is.na(respuesta), !is.na(peso_muestral), peso_muestral > 0) |>
  dplyr::filter(!as.character(respuesta) %in% respuesta_labels_raw) |>
  dplyr::mutate(
    respuesta = factor(
      as.character(respuesta),
      levels = c(
        "Don't know",
        "Agree strongly",
        "Agree",
        "Neither agree nor disagree",
        "Disagree",
        "Disagree strongly"
      ),
      labels = respuesta_levels
    )
  ) |>
  dplyr::group_by(respuesta) |>
  dplyr::summarise(
    n = sum(peso_muestral, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    porcentaje = n / sum(n),
    respuesta = factor(respuesta, levels = respuesta_levels)
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(padres, out_path)
message("Guardado: ", out_path)
