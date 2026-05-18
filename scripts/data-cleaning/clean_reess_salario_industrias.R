# ============================================================
# clean_reess_salario_industrias.R
# Extrae salario promedio y empleo registrado por industria
# (CIIU Rev. 4.1 nivel 1) desde REESS y prepara la comparación
# febrero 2025 vs febrero 2026.
# Requiere: data/raw/reess/Indicadores Laborales_Empleo_02_2026.xlsx
# Guarda:   data/processed/reess_salario_industrias_feb_2025_2026.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_reess_salario_industrias.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readxl", "stringr", "tidyr"))

input_path <- "data/raw/reess/Indicadores Laborales_Empleo_02_2026.xlsx"
out_path <- "data/processed/reess_salario_industrias_feb_2025_2026.rds"

temp_input <- file.path(tempdir(), basename(input_path))
file.copy(input_path, temp_input, overwrite = TRUE)

header_row <- 5
data_start_row <- 6

excluded_codes <- c(
  "Z0_No clasificado_CIIU",
  "Z1_Doméstico",
  "Z2_Campesino",
  "Z3_Semicontribuyente",
  "Z4_Voluntario"
)

extract_reess_series <- function(sheet_name, total_label, value_prefix) {
  raw_df <- read_excel(temp_input, sheet = sheet_name, col_names = FALSE)
  col2_name <- names(raw_df)[2]
  col3_name <- names(raw_df)[3]

  data_end_row <- raw_df %>%
    mutate(row_id = row_number()) %>%
    filter(.data[[col2_name]] == total_label) %>%
    pull(row_id) %>%
    first() - 1

  month_names <- raw_df[header_row, -c(1:3)] %>%
    unlist(use.names = FALSE) %>%
    as.character()

  feb_2025_idx <- match("feb.-25", month_names)
  feb_2026_idx <- match("feb.-26", month_names)

  series_values <- raw_df[data_start_row:data_end_row, -(1:3)]
  colnames(series_values) <- month_names

  bind_cols(
    raw_df[data_start_row:data_end_row, ] %>%
      transmute(
        codigo = as.character(.data[[col2_name]]),
        industria = as.character(.data[[col3_name]])
      ),
    series_values
  ) %>%
    transmute(
      codigo = codigo,
      industria = stringr::str_remove(industria, "\\.$"),
      !!paste0(value_prefix, "_feb_2025") := as.numeric(series_values[[feb_2025_idx]]),
      !!paste0(value_prefix, "_feb_2026") := as.numeric(series_values[[feb_2026_idx]])
    ) %>%
    filter(
      !is.na(industria),
      !codigo %in% excluded_codes
    )
}

salary_df <- extract_reess_series(
  sheet_name = "4_2_1",
  total_label = "Total sueldo corriente medio",
  value_prefix = "salario"
)

employment_df <- extract_reess_series(
  sheet_name = "1_1_1",
  total_label = "Total Empleo registrado",
  value_prefix = "empleo"
)

output_df <- salary_df %>%
  left_join(employment_df, by = c("codigo", "industria"))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(output_df, out_path)

message("Guardado: ", out_path, "  (", nrow(output_df), " filas)")
