# ============================================================
# clean_sipa_piladoras_arroz.R
# Promedio mensual de los precios de arroz grano largo reportados por
# piladoras: compra de arroz en cáscara (saca de 200 libras) y venta de arroz
# pilado (quintal de 100 libras).
# Requiere: data/raw/sipa_piladoras/sipa_precios_piladoras_2012_2026_10.csv
# Guarda:   data/processed/sipa_piladoras_arroz.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_sipa_piladoras_arroz.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr", "stringr"))

input_path <- "data/raw/sipa_piladoras/sipa_precios_piladoras_2012_2026_10.csv"
out_path <- "data/processed/sipa_piladoras_arroz.rds"

piladoras <- readr::read_csv(input_path, show_col_types = FALSE) |>
  dplyr::mutate(
    presentacion = stringr::str_to_lower(presentacion),
    presentacion = stringr::str_replace(presentacion, "libras$", "libra")
  )

series <- piladoras |>
  dplyr::mutate(
    serie = dplyr::case_when(
      producto == "Arroz en Cáscara Grano Largo" & tipo == "Compra" &
        presentacion == "saca de 200,00 libra" ~ "Compra de arroz en cáscara (saca de 200 lb)",
      producto == "Arroz Pilado Natural Grano Largo" & tipo == "Venta" &
        presentacion == "quintal de 100,00 libra" ~ "Venta de arroz pilado (quintal de 100 lb)",
      .default = NA_character_
    )
  ) |>
  dplyr::filter(!is.na(serie)) |>
  dplyr::group_by(serie, anio, mes) |>
  dplyr::summarise(
    precio_promedio_usd = mean(precio_promedio_usd),
    n_cantones = dplyr::n_distinct(canton),
    .groups = "drop"
  ) |>
  dplyr::mutate(fecha = as.Date(sprintf("%d-%02d-01", anio, mes)))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(series, out_path)
message("Guardado: ", out_path)
