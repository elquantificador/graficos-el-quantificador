# ============================================================
# clean_exportaciones_eeuu.R
# Prepara la tabla de exportaciones de Ecuador hacia Estados
# Unidos para el gráfico comparativo 2024 vs 2025.
# Requiere:
#   - data/05. Export. por Producto Principal y País.xlsx
# Guarda:
#   - data/processed/exportaciones_eeuu_2024_2025.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_exportaciones_eeuu.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("readxl", "dplyr", "stringr", "tidyr"))

raw_data <- readxl::read_excel(
  "data/05. Export. por Producto Principal y País.xlsx",
  skip = 5
)

base_limpia <- raw_data |>
  dplyr::rename(
    periodo = `Período`,
    codigopp = `Código PP`,
    producto = `PP`,
    pais_destino = `País Destino`,
    tm_peso_neto = `TM (Peso Neto)`,
    fob_miles_usd = `FOB`
  ) |>
  dplyr::mutate(
    producto = stringr::str_squish(as.character(producto)),
    pais_destino = stringr::str_squish(as.character(pais_destino)),
    codigopp = as.character(codigopp),
    periodo = as.integer(periodo),
    tm_peso_neto = as.numeric(tm_peso_neto),
    fob_miles_usd = as.numeric(fob_miles_usd),
    fob_millones_usd = fob_miles_usd / 1000
  ) |>
  dplyr::filter(
    pais_destino == "ESTADOS UNIDOS",
    periodo %in% c(2024, 2025)
  )

base_agrupada <- base_limpia |>
  dplyr::mutate(
    producto_agrupado = dplyr::case_when(
      producto %in% c("PETRÓLEO CRUDO", "DERIVADOS DE PETRÓLEO") ~ "PETRÓLEO CRUDO Y DERIVADOS",
      producto %in% c("ENLATADOS DE PESCADO", "PESCADO") ~ "PESCADOS Y ENLATADOS",
      TRUE ~ producto
    )
  )

resumen_2024_2025 <- base_agrupada |>
  dplyr::group_by(producto_agrupado, periodo) |>
  dplyr::summarise(
    fob_total_miles_usd = sum(fob_miles_usd, na.rm = TRUE),
    fob_total_millones_usd = sum(fob_millones_usd, na.rm = TRUE),
    tm_total = sum(tm_peso_neto, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = periodo,
    values_from = c(fob_total_miles_usd, fob_total_millones_usd, tm_total),
    names_glue = "{.value}_{periodo}",
    values_fill = 0
  ) |>
  dplyr::mutate(
    valor_total_miles_usd = fob_total_miles_usd_2024 + fob_total_miles_usd_2025,
    valor_total_millones_usd = fob_total_millones_usd_2024 + fob_total_millones_usd_2025
  ) |>
  dplyr::arrange(dplyr::desc(valor_total_millones_usd))

top10 <- resumen_2024_2025 |>
  dplyr::slice_max(order_by = valor_total_millones_usd, n = 10, with_ties = FALSE) |>
  dplyr::arrange(dplyr::desc(valor_total_millones_usd))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(
    base_limpia = base_limpia,
    resumen_2024_2025 = resumen_2024_2025,
    top10 = top10
  ),
  "data/processed/exportaciones_eeuu_2024_2025.rds"
)

message("Guardado: data/processed/exportaciones_eeuu_2024_2025.rds")
