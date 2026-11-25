# ============================================================
# clean_inec_ipc_arroz.R
# Prepara las incidencias por producto de octubre de 2026 y el índice
# nacional del arroz y del IPC general (base 2014) hasta junio de 2026.
# Requiere: data/raw/ipc_arroz/ipc_incidencias_productos_octubre_2026.csv
#           data/raw/ipc_arroz/ipc_arroz_general_nacional_2015_2026_06.csv
# Guarda:   data/processed/inec_ipc_arroz.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_inec_ipc_arroz.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr"))

incidencias_path <- "data/raw/ipc_arroz/ipc_incidencias_productos_octubre_2026.csv"
indice_path <- "data/raw/ipc_arroz/ipc_arroz_general_nacional_2015_2026_06.csv"
out_path <- "data/processed/inec_ipc_arroz.rds"

incidencias <- readr::read_csv(incidencias_path, show_col_types = FALSE)

indice <- readr::read_csv(indice_path, show_col_types = FALSE) |>
  dplyr::mutate(
    fecha = as.Date(paste0(periodo, "-01")),
    serie = ifelse(serie == "General", "IPC general", serie)
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(list(incidencias = incidencias, indice = indice), out_path)
message("Guardado: ", out_path)
