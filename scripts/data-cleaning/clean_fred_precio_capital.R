# ============================================================
# clean_fred_precio_capital.R
# Salario básico unificado (SBU) y precios productor de Estados Unidos de
# maquinaria, computadoras y software, 2007 a 2026, en índice con 2007 = 100.
# Requiere: data/raw/fred/WPU11.csv
#           data/raw/fred/PCU334111334111.csv
#           data/raw/fred/PCU511210511210.csv
#           data/raw/mdt/sbu_ecuador_2007_2027.csv
# Guarda:   data/processed/fred_precio_capital.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_fred_precio_capital.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "purrr", "readr", "lubridate"))

out_path <- "data/processed/fred_precio_capital.rds"

# Índices de precios productor de Estados Unidos (BLS vía FRED), mensuales.
precios <- c(
  "Maquinaria industrial" = "data/raw/fred/WPU11.csv",
  "Computadoras" = "data/raw/fred/PCU334111334111.csv",
  "Software" = "data/raw/fred/PCU511210511210.csv"
) |>
  purrr::map(readr::read_csv, show_col_types = FALSE) |>
  purrr::list_rbind(names_to = "serie")

sbu <- readr::read_csv(
  "data/raw/mdt/sbu_ecuador_2007_2027.csv",
  show_col_types = FALSE
)

# 2026 es el promedio de enero a octubre, los meses publicados.
precios_anuales <- precios |>
  dplyr::mutate(anio = lubridate::year(fecha)) |>
  dplyr::summarise(valor = mean(valor, na.rm = FALSE), .by = c(serie, anio))

sbu_anual <- sbu |>
  dplyr::filter(anio <= 2026) |>
  dplyr::transmute(serie = "Salario básico", anio, valor = sbu)

precio_capital <- dplyr::bind_rows(sbu_anual, precios_anuales) |>
  dplyr::mutate(indice = 100 * valor / valor[anio == 2007], .by = serie)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(precio_capital, out_path)
message("Guardado: ", out_path)
