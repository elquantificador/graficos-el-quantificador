# ============================================================
# clean_pisa_ecuador_puntajes.R
# Prepara los puntajes promedio de Ecuador para PISA-D 2017 y PISA 2025.
# Requiere: data/raw/pisa_ecuador/pisa_ecuador_mean_scores.csv
# Guarda:   data/processed/pisa_ecuador_puntajes.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_pisa_ecuador_puntajes.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr"))

input_path <- "data/raw/pisa_ecuador/pisa_ecuador_mean_scores.csv"
out_path <- "data/processed/pisa_ecuador_puntajes.rds"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

scores <- readr::read_csv(input_path, show_col_types = FALSE) |>
  dplyr::mutate(
    subject = dplyr::recode(
      subject,
      science = "Ciencias",
      reading = "Lectura",
      mathematics = "Matem\u00e1ticas"
    ),
    subject = factor(subject, levels = c("Ciencias", "Lectura", "Matem\u00e1ticas")),
    year = factor(year, levels = c(2017, 2025), labels = c("2017", "2025")),
    score_label = format(round(mean_score, 1), nsmall = 1, decimal.mark = ",")
  ) |>
  dplyr::arrange(subject, year)

result <- list(
  summary = scores,
  source = "OECD PISA 2025 Results Volume I, tables I.B1.2a.36-38",
  note = "2017 corresponde a PISA for Development; la tabla de tendencia del OECD lo presenta bajo la columna PISA 2018."
)

saveRDS(result, out_path)
message("Guardado: ", out_path)
