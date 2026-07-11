# ============================================================
# clean_enighur_gasolina_transporte_quintiles.R
# Prepara datos para comparar gasolina y transporte publico por
# quintil de ingreso del hogar (ENIGHUR 2024-2025).
# Requiere: data/raw/enighur/enighur_gasolina_transporte_publico_quintiles_2025.csv
# Guarda:   data/processed/enighur_gasolina_transporte_quintiles_2025.rds
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr"))

input_path <- "data/raw/enighur/enighur_gasolina_transporte_publico_quintiles_2025.csv"
out_path <- "data/processed/enighur_gasolina_transporte_quintiles_2025.rds"

raw_df <- readr::read_csv(input_path, show_col_types = FALSE)

plot_df <- dplyr::bind_rows(
  raw_df |>
    dplyr::transmute(
      quintil_ingreso,
      rubro = "Gasolina",
      gasto_promedio = gasolina_promedio,
      share_gasto_monetario = gasolina_share
    ),
  raw_df |>
    dplyr::transmute(
      quintil_ingreso,
      rubro = "Transporte publico",
      gasto_promedio = transporte_publico_promedio,
      share_gasto_monetario = transporte_publico_share
    )
) |>
  dplyr::mutate(
    quintil_ingreso = factor(quintil_ingreso, levels = c("Q1", "Q2", "Q3", "Q4", "Q5")),
    rubro = factor(rubro, levels = c("Gasolina", "Transporte publico"))
  ) |>
  dplyr::arrange(.data$quintil_ingreso, .data$rubro)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(summary = plot_df, source = input_path), out_path)

message("Guardado: ", out_path)
