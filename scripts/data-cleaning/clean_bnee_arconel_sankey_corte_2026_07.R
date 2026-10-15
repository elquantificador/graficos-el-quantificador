# ============================================================
# clean_bnee_arconel_sankey_corte_2026_07.R
# Extrae las fuentes y destinos del BNEE de ARCONEL.
# Requiere: data/raw/BNEE_julio_2026.xls
# Guarda:   data/processed/bnee_arconel_sankey_corte_2026_07.csv
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_bnee_arconel_sankey_corte_2026_07.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readxl", "readr"))

input_path <- "data/raw/BNEE_julio_2026.xls"
out_path <- "data/processed/bnee_arconel_sankey_corte_2026_07.csv"

bnee <- read_excel(
  input_path,
  sheet = "BNEE julio 2026",
  col_names = FALSE,
  .name_repair = "minimal"
)

cell_gwh <- function(row, column) {
  as.numeric(bnee[[column]][[row]])
}

total_supply_gwh <- cell_gwh(6, 9)
energy_delivered_gwh <- cell_gwh(4, 21)

flow_data <- tibble(
  origen = c(
    "Hidráulica",
    "No renovable",
    "Otras renovables",
    "Importaciones",
    rep("Producción e importaciones", 6)
  ),
  destino = c(
    rep("Producción e importaciones", 4),
    "No disponible para servicio público",
    "Pérdidas en transmisión",
    "Exportación",
    "Pérdidas técnicas en distribución",
    "Pérdidas no técnicas en distribución",
    "Consumo total"
  ),
  energia_gwh = c(
    cell_gwh(9, 9),
    cell_gwh(14, 9),
    sum(vapply(10:13, function(row) cell_gwh(row, 9), numeric(1))),
    cell_gwh(18, 9),
    total_supply_gwh - energy_delivered_gwh,
    cell_gwh(9, 21),
    cell_gwh(13, 21),
    cell_gwh(19, 21),
    cell_gwh(20, 21),
    cell_gwh(17, 21)
  )
)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
write_csv(flow_data, out_path)

message("Guardado: ", out_path)
