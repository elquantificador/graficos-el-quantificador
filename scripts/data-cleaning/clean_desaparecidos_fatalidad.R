# clean_desaparecidos_fatalidad.R
# Prepara la proporción anual de denuncias que permanecen desaparecidas o terminan en fallecimiento.
# Requiere: data/raw/desapariciones/mdi_personasdesaparecidas_pm_2017_2025.xlsx
# Guarda: data/processed/desaparecidos_fatalidad.rds

source("scripts/packages.R")
ensure_packages(c("dplyr", "readxl", "lubridate"))

input_path <- "data/raw/desapariciones/mdi_personasdesaparecidas_pm_2017_2025.xlsx"
out_path <- "data/processed/desaparecidos_fatalidad.rds"

raw <- readxl::read_excel(input_path, sheet = "1")

annual <- raw %>%
  mutate(anio = lubridate::year(fecha_desaparicion)) %>%
  filter(anio >= 2017, anio <= 2025, !is.na(situacion_actual)) %>%
  count(anio, situacion_actual, name = "casos") %>%
  group_by(anio) %>%
  mutate(
    total_denuncias = sum(casos),
    porcentaje = casos / total_denuncias
  ) %>%
  ungroup() %>%
  filter(situacion_actual %in% c("DESAPARECIDO", "FALLECIDO")) %>%
  arrange(anio, situacion_actual)

metadata <- list(
  period = c("2017-01-01", "2025-12-31"),
  statuses = c("DESAPARECIDO", "FALLECIDO"),
  total_denuncias = sum(unique(annual[c("anio", "total_denuncias")])$total_denuncias),
  source = "EDDIETOMALAFIGUEROA/Github-Eddie-Tomala"
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(data = annual, metadata = metadata), out_path)
message("Guardado: ", out_path, " (", nrow(annual), " filas; ", length(unique(annual$anio)), " años)")
