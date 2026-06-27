# ============================================================
# clean_lgbti_aceptacion_orientacion_identidad.R
# Calcula la aceptación de la orientación sexual o identidad
# de género entre personas cercanas usando la ENCV LGBTI+ 2025.
# Requiere: data/raw/lgbti/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx
# Guarda:   data/processed/lgbti_aceptacion_orientacion_identidad_2025.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_lgbti_aceptacion_orientacion_identidad.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readxl", "srvyr"))

input_path <- "data/raw/lgbti/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx"
out_path <- "data/processed/lgbti_aceptacion_orientacion_identidad_2025.rds"

base_lgbti <- readxl::read_excel(input_path)

base_lgbti_fexp <- base_lgbti |>
  srvyr::as_survey_design(weights = fexp)

tab <- function(var, grupo) {
  base_lgbti_fexp |>
    dplyr::filter({{ var }} != "No aplica") |>
    dplyr::filter({{ var }} != "No sabe") |>
    dplyr::group_by(respuesta = {{ var }}) |>
    srvyr::summarise(proportion = srvyr::survey_prop(vartype = NULL)) |>
    dplyr::mutate(
      grupo = grupo,
      porcentaje = proportion
    ) |>
    dplyr::select(grupo, respuesta, porcentaje)
}

plot_df <- dplyr::bind_rows(
  tab(s08_p01_1_1a, "Madre"),
  tab(s08_p01_2_1a, "Padre"),
  tab(s08_p01_5_1a, "Hermanas/os"),
  tab(s08_p01_7_1a, "Amigas/os"),
  tab(s08_p01_8_1a, "Compañeras/os de estudio/trabajo")
)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(plot_df, out_path)
message("Guardado: ", out_path)
