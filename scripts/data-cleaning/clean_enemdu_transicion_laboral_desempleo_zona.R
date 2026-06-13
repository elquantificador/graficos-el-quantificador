# ============================================================
# clean_enemdu_transicion_laboral_desempleo_zona.R
# Extrae la transición laboral de personas desempleadas por
# zona de residencia desde la matriz ENEMDU IV 2022 - IV 2023.
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
urbano_row <- extract_unemployed_row("1.2. MTL - Urbano")
rural_row <- extract_unemployed_row("1.3. MTL - Rural")

flows_df <- dplyr::bind_rows(
  tibble::tibble(
    origen = "Desempleados 2022",
    zona = "Urbano",
    destino = "Obtuvo empleo en 2023",
    count = urbano_row$empleado_2023
  ),
  tibble::tibble(
    origen = "Desempleados 2022",
    zona = "Urbano",
    destino = "Continuó desempleado en 2023",
    count = urbano_row$desempleado_2023
  ),
  tibble::tibble(
    origen = "Desempleados 2022",
    zona = "Urbano",
    destino = "Salió de la fuerza laboral en 2023",
    count = urbano_row$pei_2023
  ),
  tibble::tibble(
    origen = "Desempleados 2022",
    zona = "Rural",
    destino = "Obtuvo empleo en 2023",
    count = rural_row$empleado_2023
  ),
  tibble::tibble(
    origen = "Desempleados 2022",
    zona = "Rural",
    destino = "Continuó desempleado en 2023",
    count = rural_row$desempleado_2023
  ),
  tibble::tibble(
    origen = "Desempleados 2022",
    zona = "Rural",
    destino = "Salió de la fuerza laboral en 2023",
    count = rural_row$pei_2023
  )
) %>%
  mutate(
    share_total = .data$count / nacional_row$total_2022
  )

zone_df <- tibble::tibble(
  zona = c("Urbano", "Rural"),
  count = c(urbano_row$total_2022, rural_row$total_2022)
) %>%
  mutate(
    share_total = .data$count / nacional_row$total_2022
  )

outcome_df <- flows_df %>%
  group_by(.data$destino) %>%
  summarise(
    count = sum(.data$count),
    .groups = "drop"
  ) %>%
  mutate(
    share_total = .data$count / nacional_row$total_2022
  )

chart_data <- list(
  flows = flows_df,
  root = tibble::tibble(
    origen = "Desempleados 2022",
    count = nacional_row$total_2022,
    share_total = 1
  ),
  zones = zone_df,
  outcomes = outcome_df,
  metadata = list(
    period = "Trimestre IV 2022 - Trimestre IV 2023",
    source_file = input_path,
    sheets = c("1.1. MTL - Nacional", "1.2. MTL - Urbano", "1.3. MTL - Rural")
  )
)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(chart_data, out_path)
message("Guardado: ", out_path)
