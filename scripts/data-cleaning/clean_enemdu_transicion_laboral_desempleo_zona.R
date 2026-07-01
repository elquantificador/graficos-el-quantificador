# ============================================================
# clean_enemdu_transicion_laboral_desempleo_zona.R
# Extrae la transición laboral nacional de personas
# desempleadas desde la matriz ENEMDU IV 2022 - IV 2023.
# Requiere: data/raw/enemdu/Trimestre_IV_2022_2023_tabulados_matriz.xlsx
# Guarda:   data/processed/enemdu_transicion_laboral_desempleo_zona_2022_2023.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_transicion_laboral_desempleo_zona.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("readxl", "dplyr", "stringr"))

input_path <- "data/raw/enemdu/Trimestre_IV_2022_2023_tabulados_matriz.xlsx"
out_path <- "data/processed/enemdu_transicion_laboral_desempleo_zona_2022_2023.rds"

extract_unemployed_row <- function(sheet_name) {
  raw_df <- readxl::read_excel(
    input_path,
    sheet = sheet_name,
    col_names = FALSE
  )

  row_df <- raw_df %>%
    filter(
      .data$...2 == "Desempleado",
      !is.na(.data$...3),
      !is.na(.data$...6)
    ) %>%
    slice(1) %>%
    transmute(
      empleado_2023 = as.numeric(.data$...3),
      desempleado_2023 = as.numeric(.data$...4),
      pei_2023 = as.numeric(.data$...5),
      total_2022 = as.numeric(.data$...6)
    )

  if (nrow(row_df) != 1) {
    stop("No se pudo extraer la fila de desempleo para la hoja: ", sheet_name)
  }

  row_df
}

nacional_row <- extract_unemployed_row("1.1. MTL - Nacional")

flows_df <- dplyr::bind_rows(
  tibble::tibble(
    origen = "Desempleados 2022",
    destino = "Obtuvo empleo en 2023",
    count = nacional_row$empleado_2023
  ),
  tibble::tibble(
    origen = "Desempleados 2022",
    destino = "Continuó desempleado en 2023",
    count = nacional_row$desempleado_2023
  ),
  tibble::tibble(
    origen = "Desempleados 2022",
    destino = "Ya no busca trabajo en 2023",
    count = nacional_row$pei_2023
  )
) %>%
  mutate(
    share_total = .data$count / nacional_row$total_2022
  )

outcome_df <- flows_df %>%
  dplyr::select("destino", "count", "share_total")

chart_data <- list(
  flows = flows_df,
  root = tibble::tibble(
    origen = "Desempleados 2022",
    count = nacional_row$total_2022,
    share_total = 1
  ),
  outcomes = outcome_df,
  metadata = list(
    period = "Trimestre IV 2022 - Trimestre IV 2023",
    source_file = input_path,
    sheets = c("1.1. MTL - Nacional")
  )
)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(chart_data, out_path)
message("Guardado: ", out_path)
