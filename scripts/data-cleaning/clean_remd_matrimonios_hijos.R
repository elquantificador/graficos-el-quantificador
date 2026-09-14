# ============================================================
# clean_remd_matrimonios_hijos.R
# Prepara la cohorte 2020 para las curvas por hijos reconocidos.
# Requiere: data/raw/remd_matrimonios/cohort_2020_survival_input.csv
# Guarda:   data/processed/remd_matrimonios_hijos.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_remd_matrimonios_hijos.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr"))

input_path <- "data/raw/remd_matrimonios/cohort_2020_survival_input.csv"
out_path <- "data/processed/remd_matrimonios_hijos.rds"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

children_levels <- c(
  "0 hijos",
  "1 hijo",
  "2 hijos",
  "3 hijos",
  "4+ hijos"
)

analysis_data <- readr::read_csv(input_path, show_col_types = FALSE) |>
  dplyr::mutate(
    primary_link_eligible = as.logical(primary_link_eligible),
    event_registered_divorce = as.logical(event_registered_divorce),
    followup_years = as.numeric(followup_years),
    children_group = dplyr::case_when(
      children_group == "0 hijos" ~ "0 hijos",
      children_group == "1 hijo" ~ "1 hijo",
      children_group == "2 hijos" ~ "2 hijos",
      children_group == "3 hijos" ~ "3 hijos",
      grepl("^4 o m", children_group) ~ "4+ hijos",
      TRUE ~ NA_character_
    ),
    children_group = factor(children_group, levels = children_levels)
  ) |>
  dplyr::filter(
    primary_link_eligible,
    !is.na(children_group),
    !is.na(followup_years),
    followup_years >= 0,
    followup_years <= 5,
    !is.na(event_registered_divorce)
  )

result <- list(
  data = analysis_data,
  source = "REMD 2020-2025, INEC; validated analysis cohort from matrimonios-divorcios-salas commit cde4c13.",
  note = "This compact input keeps only the fields needed for the children-group survival curves and omits linkage identifiers."
)

saveRDS(result, out_path)
message("Guardado: ", out_path)
