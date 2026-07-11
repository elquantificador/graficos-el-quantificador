# ============================================================
# clean_enighur_gasolina_share_quintiles_years.R
# Prepara datos para comparar el peso de la gasolina dentro del
# gasto monetario del hogar por quintil y encuesta.
# Requiere: data/raw/enighur/enighur_gasolina_share_quintiles_2012_2025.csv
# Guarda:   data/processed/enighur_gasolina_share_quintiles_years.rds
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr"))

input_path <- "data/raw/enighur/enighur_gasolina_share_quintiles_2012_2025.csv"
out_path <- "data/processed/enighur_gasolina_share_quintiles_years.rds"

plot_df <- readr::read_csv(input_path, show_col_types = FALSE) |>
  dplyr::mutate(
    quintil_ingreso = factor(quintil_ingreso, levels = c("Q1", "Q2", "Q3", "Q4", "Q5")),
    encuesta = factor(encuesta, levels = c("ENIGHUR 2011-2012", "ENIGHUR 2024-2025"))
  ) |>
  dplyr::arrange(.data$quintil_ingreso, .data$encuesta)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(summary = plot_df, source = input_path), out_path)

message("Guardado: ", out_path)
