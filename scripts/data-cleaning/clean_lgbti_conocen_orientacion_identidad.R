# ============================================================
# clean_lgbti_conocen_orientacion_identidad.R
# Author: Daniel Sánchez Pazmiño
# Purpose: Calcula el porcentaje de personas cercanas que conoce la
#          orientación sexual o identidad de género de la persona
#          encuestada, por tipo de relación, ENCV LGBTI+ 2025.
# Inputs:  data/raw/lgbti/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx
# Outputs: data/processed/lgbti_conocen_orientacion_identidad_2025.rds
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "readxl", "srvyr"))

# 0. Setup ----
input_path <- "data/raw/lgbti/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx"
out_path   <- "data/processed/lgbti_conocen_orientacion_identidad_2025.rds"

# 1. Load Data ----
base_lgbti <- readxl::read_excel(input_path)

# 2. Clean / Transform ----
plot_df <- base_lgbti |>
  dplyr::select(
    fexp,
    Madre                              = s08_p01_1,
    Padre                              = s08_p01_2,
    `Hermanas/os`                      = s08_p01_5,
    `Amigas/os`                        = s08_p01_7,
    `Compañeras/os de estudio/trabajo` = s08_p01_8
  ) |>
  tidyr::pivot_longer(-fexp, names_to = "grupo", values_to = "respuesta") |>
  dplyr::filter(respuesta != "No aplica") |>
  srvyr::as_survey_design(weights = fexp) |>
  dplyr::group_by(grupo) |>
  srvyr::summarise(porcentaje = srvyr::survey_mean(respuesta == "sí", vartype = NULL))

# 5. Export ----
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(plot_df, out_path)
message("Guardado: ", out_path)

sessionInfo()
