# ============================================================
# clean_fbref_ecuatorianos_big5.R
# Prepara la tabla de minutos jugados por ecuatorianos en las
# cinco grandes ligas de Europa por temporada.
# Requiere: data/raw/fbref/ecuatorianos_big5_minutos_2019_2026.csv
# Guarda:   data/processed/fbref_ecuatorianos_big5.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_fbref_ecuatorianos_big5.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("readr", "dplyr"))

out_path <- "data/processed/fbref_ecuatorianos_big5.rds"

df <- readr::read_csv(
  "data/raw/fbref/ecuatorianos_big5_minutos_2019_2026.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(
    temporada = factor(
      temporada,
      levels = c("19/20", "20/21", "21/22", "22/23", "23/24", "24/25", "25/26")
    )
  ) |>
  dplyr::arrange(temporada, dplyr::desc(minutos), jugador)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(df, out_path)
message("Guardado: ", out_path)
