# ============================================================
# clean_padres_hijos_censo.R
# Lee las tabulaciones REDATAM del censo 2010 y 2022 y prepara
# los datos para el gráfico de convivencia con padres/abuelos.
# Guarda: data/processed/padres_hijos_censo.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/clean_padres_hijos_censo.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("readxl", "dplyr", "tidyr", "stringr"))

file_2010 <- "data/censo_padres_hijos_2010.xlsx"
file_2022 <- "data/censo_padres_hijos_2022.xlsx"
out_path <- "data/processed/padres_hijos_censo.rds"

age_keep <- c("10 - 19 años", "20 - 29 años", "30 - 39 años", "40 - 49 años")
num_2010 <- c("Hijo o hija", "Nieto o nieta")
num_2022 <- c("Hija o hijo", "Hijastra o hijastro", "Nieta o nieto")

read_redatam_tab <- function(path, year) {
  raw <- read_excel(path, sheet = "Output", col_names = FALSE)
  age_groups <- raw[12, 3:ncol(raw)] |> unlist() |> as.character()
  dat <- raw[13:nrow(raw), 2:ncol(raw)]
  names(dat) <- c("relationship", age_groups)

  dat |>
    mutate(relationship = str_squish(as.character(relationship))) |>
    filter(!is.na(relationship), relationship != "", relationship != "Total") |>
    pivot_longer(
      cols = -relationship,
      names_to = "age_group",
      values_to = "n"
    ) |>
    mutate(
      n = ifelse(n == "-", 0, n),
      n = suppressWarnings(as.numeric(n)),
      year = year
    ) |>
    filter(!is.na(n))
}

plot_df <- bind_rows(
  read_redatam_tab(file_2010, 2010),
  read_redatam_tab(file_2022, 2022)
) |>
  filter(age_group %in% age_keep) |>
  mutate(
    in_numerator = case_when(
      year == 2010 & relationship %in% num_2010 ~ TRUE,
      year == 2022 & relationship %in% num_2022 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  group_by(year, age_group) |>
  summarise(
    numerator = sum(n[in_numerator], na.rm = TRUE),
    denominator = sum(n, na.rm = TRUE),
    share = numerator / denominator,
    .groups = "drop"
  ) |>
  mutate(age_group = factor(age_group, levels = age_keep))

dir.create("data/processed", showWarnings = FALSE)
saveRDS(plot_df, out_path)
message("Guardado: ", out_path, "  (", nrow(plot_df), " filas)")
