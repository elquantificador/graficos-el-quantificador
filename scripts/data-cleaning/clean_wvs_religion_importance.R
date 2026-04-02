# ============================================================
# clean_wvs_religion_importance.R
# Lee la tabulación del World Values Survey sobre la importancia
# de la religión y prepara una tabla larga para graficar.
# Requiere: data/wvs_importance_of_religion_in_life.xls
# Guarda:   data/processed/wvs_religion_importance.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_wvs_religion_importance.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("readxl", "dplyr", "tidyr", "stringr"))

file_path <- "data/wvs_importance_of_religion_in_life.xls"
out_path <- "data/processed/wvs_religion_importance.rds"

response_levels <- c(
  "Very important",
  "Rather important",
  "Not very important",
  "Not at all important",
  "Don't know",
  "No answer"
)

parse_total_percent <- function(x) {
  x |>
    str_extract("^\\s*\\d+(?:\\.\\d+)?") |>
    as.numeric() / 100
}

parse_total_n <- function(x) {
  x |>
    str_extract("\\(([0-9,]+)\\)") |>
    str_remove_all("[(),]") |>
    as.numeric()
}

raw <- read_excel(file_path, sheet = 1, col_names = FALSE, .name_repair = "minimal")

geographies <- c("Total", raw[5, 3:ncol(raw)] |> unlist() |> as.character())

tab <- raw[6:12, 1:ncol(raw)]
names(tab) <- c("response", geographies)

sample_sizes <- tab |>
  filter(response == "(N)") |>
  select(-response) |>
  pivot_longer(
    cols = everything(),
    names_to = "geography",
    values_to = "sample_size"
  ) |>
  mutate(sample_size = suppressWarnings(as.numeric(sample_size)))

df <- tab |>
  filter(response != "(N)") |>
  pivot_longer(
    cols = -response,
    names_to = "geography",
    values_to = "raw_value"
  ) |>
  mutate(
    response = factor(response, levels = response_levels, ordered = TRUE),
    share = case_when(
      geography == "Total" ~ parse_total_percent(raw_value),
      TRUE ~ suppressWarnings(as.numeric(raw_value) / 100)
    ),
    n = case_when(
      geography == "Total" ~ parse_total_n(raw_value),
      TRUE ~ NA_real_
    )
  ) |>
  left_join(sample_sizes, by = "geography") |>
  mutate(
    estimated_n = round(share * sample_size),
    geography = factor(geography, levels = geographies)
  ) |>
  select(geography, response, share, n, sample_size, estimated_n)

dir.create("data/processed", showWarnings = FALSE)
saveRDS(df, out_path)
message("Guardado: ", out_path, "  (", nrow(df), " filas)")
