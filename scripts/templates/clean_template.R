# ============================================================
# clean_template.R
# One-line description of what this script does.
# Requiere: data/raw/source/file.ext
# Guarda:   data/processed/source_topic.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_source_topic.R
# ============================================================

source('scripts/packages.R')
ensure_packages(c('dplyr', 'readr'))

input_path <- 'data/raw/source/file.ext'
out_path <- 'data/processed/source_topic.rds'

dir.create('data/processed', recursive = TRUE, showWarnings = FALSE)

# Reemplaza este bloque con tu lógica de limpieza.
df <- readr::read_csv(input_path, show_col_types = FALSE)

saveRDS(df, out_path)
message('Guardado: ', out_path)
