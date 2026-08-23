# clean_crimen_desapariciones.R
# Prepara la serie anual de muertes intencionales, desapariciones y presupuesto policial.
# Requiere archivos en data/raw/crimen/.
# Guarda: data/processed/crimen_desapariciones.rds

source("scripts/packages.R")
ensure_packages(c("dplyr", "readxl", "lubridate"))

death_path <- "data/raw/crimen/mdi_homicidiosintencionales_pm_2014_2026.xlsx"
missing_path <- "data/raw/crimen/mdi_personasdesaparecidas_pm_2017_2026.xlsx"
budget_path <- "data/raw/crimen/Gasto_proforma.xlsx"
out_path <- "data/processed/crimen_desapariciones.rds"

death_types <- c("ASESINATO", "HOMICIDIO", "FEMICIDIO", "SICARIATO")

deaths <- readxl::read_excel(death_path) %>%
  mutate(anio = lubridate::year(fecha_infraccion)) %>%
  filter(anio >= 2017, anio <= 2025, tipo_muerte %in% death_types) %>%
  count(anio, name = "muertes_intencionales")

missing <- readxl::read_excel(missing_path) %>%
  mutate(anio = lubridate::year(fecha_desaparicion)) %>%
  filter(anio >= 2017, anio <= 2025) %>%
  count(anio, name = "desapariciones")

budget_raw <- readxl::read_excel(budget_path)
budget_year_col <- names(budget_raw)[1]

budget <- budget_raw %>%
  filter(
    grepl("052", Entidad, fixed = TRUE),
    .data[[budget_year_col]] >= 2017,
    .data[[budget_year_col]] <= 2025
  ) %>%
  transmute(
    anio = as.integer(.data[[budget_year_col]]),
    presupuesto_policial = as.numeric(pre_liquidado)
  )

annual <- deaths %>%
  full_join(missing, by = "anio") %>%
  full_join(budget, by = "anio") %>%
  arrange(anio)

metadata <- list(
  period = c("2017-01-01", "2025-12-31"),
  death_types = death_types,
  death_records = sum(annual$muertes_intencionales),
  disappearance_records = sum(annual$desapariciones),
  source_death = "angel-cloud976/LIDE-Grafico_Crimen",
  source_disappearance = "angel-cloud976/LIDE-Grafico_Crimen",
  source_budget = "angel-cloud976/LIDE-Grafico_Crimen"
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(data = annual, metadata = metadata), out_path)
message(
  "Guardado: ", out_path,
  " (", nrow(annual), " años; ", metadata$death_records,
  " muertes intencionales; ", metadata$disappearance_records,
  " desapariciones)"
)
