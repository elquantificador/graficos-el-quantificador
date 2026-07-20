# ============================================================
# clean_enemdu_horas_sector.R
# Prepara el promedio de horas trabajadas por sector y sexo,
# 2007-2026, para comparar la jornada laboral formal e informal.
# Requiere: data/raw/enemdu/enemdu_horas_sector_2007_2026.csv
# Guarda:   data/processed/enemdu_horas_sector_2007_2026.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_horas_sector.R
# ============================================================
# Nota: el insumo es el agregado por año/sexo/sector calculado por
# Eddie Tomalá a partir de los microdatos trimestrales de la ENEMDU
# (personas de 15 años o más, p24 = horas trabajadas). Los microdatos
# crudos 2007-2018 no forman parte de este repositorio; el agregado
# aquí tratado como insumo "raw" es la salida reproducible de ese
# procesamiento externo.

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr"))

input_path <- "data/raw/enemdu/enemdu_horas_sector_2007_2026.csv"
out_path <- "data/processed/enemdu_horas_sector_2007_2026.rds"

datos_consolidados <- readr::read_csv(input_path, show_col_types = FALSE) |>
  mutate(
    sexo = factor(sexo, levels = c("Mujeres", "Hombres")),
    sector_desc = factor(
      sector_desc,
      levels = c("Sector Formal", "Sector Informal", "Empleo Doméstico", "No clasificados por sector")
    )
  )

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(datos_consolidados, out_path)
message("Guardado: ", out_path)
