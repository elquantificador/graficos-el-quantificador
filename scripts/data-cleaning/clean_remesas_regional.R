# ============================================================
# clean_remesas_regional.R
# Descarga y verifica remesas recibidas para Ecuador, Colombia y Perú, y el
# PIB nominal de Ecuador para calcular el peso de las remesas en la economía.
#
# Fuentes:
#   - Banco Mundial, indicadores BX.TRF.PWKR.CD.DT y NY.GDP.MKTP.CD.
#   - Banco Central del Ecuador, Boletín Analítico de la Evolución Anual
#     de Remesas 2025 y base histórica publicada por el BCE.
#
# Guarda:
#   - data/raw/remesas_regional/      descargas reproducibles
#   - data/processed/remesas_regional_world_bank_bce.rds
#   - outputs/tables/remesas_ecuador_bce_world_bank.csv
#   - outputs/tables/remesas_regional_growth_world_bank.csv
#   - outputs/tables/remesas_origen_bce_2025.csv
#
# Ejecutar desde la raíz del repositorio:
#   Rscript scripts/data-cleaning/clean_remesas_regional.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("WDI", "readxl", "dplyr", "tidyr", "readr"))

raw_dir <- "data/raw/remesas_regional"
processed_dir <- "data/processed"
tables_dir <- "outputs/tables"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

wb_indicator <- "BX.TRF.PWKR.CD.DT"
gdp_indicator <- "NY.GDP.MKTP.CD"
bce_pdf_url <- paste0(
  "https://contenido.bce.fin.ec/documentos/Estadisticas/",
  "SectorExterno/BalanzaPagos/Remesas/eren2025anual.pdf"
)
bce_workbook_url <- paste0(
  "https://contenido.bce.fin.ec/documentos/Estadisticas/",
  "SectorExterno/BalanzaPagos/Remesas/RemesasIntegradoWEB_PUB.xlsx"
)

download_if_missing <- function(url, path) {
  if (!file.exists(path)) {
    download.file(url, path, mode = "wb", quiet = TRUE)
  }
  if (!file.exists(path) || file.info(path)$size == 0) {
    stop("La descarga no produjo un archivo válido: ", path)
  }
  path
}

fetch_wdi_indicator <- function(country_code, indicator) {
  data <- WDI::WDI(
    country = country_code,
    indicator = indicator,
    start = 2000,
    end = 2025,
    extra = FALSE
  )
  if (nrow(data) == 0 || !indicator %in% names(data)) {
    stop("El paquete WDI no devolvió observaciones para ", country_code,
         " e indicador ", indicator)
  }

  data |>
    dplyr::transmute(
      pais = country_code,
      anio = as.integer(year),
      valor_usd = as.numeric(.data[[indicator]]),
      fuente = "Banco Mundial (paquete WDI)",
      indicador = indicator,
      fecha_descarga = as.character(Sys.Date())
    ) |>
    dplyr::filter(!is.na(valor_usd)) |>
    dplyr::arrange(pais, anio)
}

fetch_world_bank <- function(country_code) {
  fetch_wdi_indicator(country_code, wb_indicator) |>
    dplyr::mutate(
      remesas_usd = valor_usd,
      remesas_millones_usd = valor_usd / 1e6
    ) |>
    dplyr::select(
      pais, anio, remesas_usd, remesas_millones_usd,
      fuente, indicador, fecha_descarga
    )
}

message("Descargando series del Banco Mundial...")
world_bank <- dplyr::bind_rows(
  fetch_world_bank("ECU"),
  fetch_world_bank("COL"),
  fetch_world_bank("PER")
) |>
  dplyr::mutate(
    # Se usa ASCII en los archivos técnicos para evitar corrupción de tildes
    # bajo la configuración regional de R en Windows. El rótulo público puede
    # mapearse a "Perú" al producir el gráfico.
    pais = dplyr::recode(pais, ECU = "Ecuador", COL = "Colombia", PER = "Peru")
  )

readr::write_csv(
  world_bank,
  file.path(raw_dir, "world_bank_remesas_wdi.csv")
)

message("Descargando PIB nominal de Ecuador del Banco Mundial...")
ecuador_gdp <- fetch_wdi_indicator("ECU", gdp_indicator) |>
  dplyr::mutate(
    pais = "Ecuador",
    gdp_usd = valor_usd,
    gdp_millones_usd = valor_usd / 1e6
  ) |>
  dplyr::select(
    pais, anio, gdp_usd, gdp_millones_usd,
    fuente, indicador, fecha_descarga
  )

readr::write_csv(
  ecuador_gdp,
  file.path(raw_dir, "world_bank_ecuador_gdp_wdi.csv")
)

ecuador_remesas_pib <- world_bank |>
  dplyr::filter(pais == "Ecuador") |>
  dplyr::select(anio, remesas_millones_usd) |>
  dplyr::inner_join(
    ecuador_gdp |>
      dplyr::select(anio, gdp_millones_usd),
    by = "anio"
  ) |>
  dplyr::mutate(
    remesas_pct_pib = 100 * remesas_millones_usd / gdp_millones_usd
  ) |>
  dplyr::arrange(anio)

quality_world_bank <- world_bank |>
  dplyr::group_by(pais) |>
  dplyr::summarise(
    filas = dplyr::n(),
    anios_distintos = dplyr::n_distinct(anio),
    anio_min = min(anio),
    anio_max = max(anio),
    faltantes = sum(is.na(remesas_millones_usd)),
    duplicados_anio = filas - anios_distintos,
    valores_negativos = sum(remesas_millones_usd < 0, na.rm = TRUE),
    .groups = "drop"
  )

if (any(quality_world_bank$faltantes > 0 |
        quality_world_bank$duplicados_anio > 0 |
        quality_world_bank$valores_negativos > 0)) {
  stop("Falló el chequeo de calidad de la serie del Banco Mundial")
}

if (nrow(ecuador_gdp) == 0 ||
    any(is.na(ecuador_gdp$gdp_millones_usd)) ||
    any(ecuador_gdp$gdp_millones_usd <= 0) ||
    anyDuplicated(ecuador_gdp$anio) > 0 ||
    any(is.na(ecuador_remesas_pib$remesas_pct_pib))) {
  stop("Falló el chequeo de calidad del PIB o del porcentaje de remesas sobre PIB")
}

message("Descargando publicaciones oficiales del BCE...")
bce_pdf_path <- download_if_missing(
  bce_pdf_url,
  file.path(raw_dir, "bce_eren2025anual.pdf")
)
bce_workbook_path <- download_if_missing(
  bce_workbook_url,
  file.path(raw_dir, "bce_RemesasIntegradoWEB_PUB.xlsx")
)

# La figura anual del boletín 2025 publica estos valores para 2016-2025.
# Se mantienen en una tabla explícita porque el PDF presenta la serie como
# etiquetas de gráfico, no como una tabla extraíble. El PDF descargado arriba
# es el respaldo primario de cada observación.
bce_annual <- tibble::tibble(
  pais = "Ecuador",
  anio = 2016:2025,
  remesas_millones_usd = c(
    2602.0, 2840.2, 3030.6, 3234.6, 3337.8,
    4362.4, 4743.5, 5447.5, 6539.8, 7729.5
  ),
  fuente = "Banco Central del Ecuador",
  documento = bce_pdf_url
)

# La Figura 3 del boletín anual publica la composición por país de origen
# para 2025. Los montos del texto del BCE suman el total anual; el porcentaje
# se recalcula desde esos montos y se redondea solo al presentar el gráfico.
bce_origin_2025 <- tibble::tibble(
  pais_procedencia = c(
    "Estados Unidos",
    "España",
    "Italia",
    "Resto del mundo"
  ),
  monto_millones_usd = c(6010.1, 1088.0, 152.6, 478.8),
  fuente = "Banco Central del Ecuador",
  documento = bce_pdf_url
) |>
  dplyr::mutate(
    anio = 2025L,
    porcentaje = 100 * monto_millones_usd /
      sum(monto_millones_usd)
  ) |>
  dplyr::select(
    anio, pais_procedencia, monto_millones_usd, porcentaje,
    fuente, documento
  )

total_bce_2025 <- bce_annual |>
  dplyr::filter(anio == 2025) |>
  dplyr::pull(remesas_millones_usd)

if (nrow(bce_origin_2025) != 4 ||
    abs(sum(bce_origin_2025$monto_millones_usd) - total_bce_2025) > 0.1 ||
    abs(bce_origin_2025$porcentaje[bce_origin_2025$pais_procedencia == "Estados Unidos"] - 77.8) > 0.1) {
  stop("Falló el chequeo de composición de remesas por país de origen en 2025")
}

# La hoja ConsultaSectorMonto de la base histórica del BCE contiene los
# totales anuales por sector. En este archivo, los años están en la fila 9 y
# "Total general" en la fila 12; las columnas 3 en adelante son años.
bce_workbook_raw <- readxl::read_excel(
  bce_workbook_path,
  sheet = "ConsultaSectorMonto",
  col_names = FALSE
)
bce_workbook_years <- suppressWarnings(
  as.integer(as.character(unlist(bce_workbook_raw[9, 3:ncol(bce_workbook_raw)])))
)
bce_workbook_totals <- suppressWarnings(
  as.numeric(as.character(unlist(bce_workbook_raw[12, 3:ncol(bce_workbook_raw)])))
)
bce_workbook_annual <- tibble::tibble(
  anio = bce_workbook_years,
  # La hoja identifica los montos como miles de USD.
  bce_workbook_millones_usd = bce_workbook_totals / 1e3
) |>
  dplyr::filter(!is.na(anio), !is.na(bce_workbook_millones_usd))

bce_crosscheck <- bce_workbook_annual |>
  dplyr::inner_join(
    bce_annual |>
      dplyr::select(anio, bce_boletin_millones_usd = remesas_millones_usd),
    by = "anio"
  ) |>
  dplyr::mutate(
    diferencia_millones_usd = bce_workbook_millones_usd - bce_boletin_millones_usd,
    diferencia_pct_boletin = 100 * diferencia_millones_usd / bce_boletin_millones_usd
  )

if (nrow(bce_crosscheck) == 0 ||
    any(abs(bce_crosscheck$diferencia_pct_boletin) > 0.01)) {
  stop("Falló el cruce entre la base histórica y el boletín anual del BCE")
}

ecuador_compare <- world_bank |>
  dplyr::filter(pais == "Ecuador", anio %in% bce_annual$anio) |>
  dplyr::select(anio, wb_millones_usd = remesas_millones_usd) |>
  dplyr::full_join(
    bce_annual |>
      dplyr::select(anio, bce_millones_usd = remesas_millones_usd),
    by = "anio"
  ) |>
    dplyr::mutate(
    diferencia_millones_usd = wb_millones_usd - bce_millones_usd,
    diferencia_pct_bce = 100 * diferencia_millones_usd / bce_millones_usd,
    dentro_tolerancia_0_5_pct = abs(diferencia_pct_bce) <= 0.5
  ) |>
  dplyr::arrange(anio)

if (any(is.na(ecuador_compare$wb_millones_usd)) ||
    any(is.na(ecuador_compare$bce_millones_usd)) ||
    any(!ecuador_compare$dentro_tolerancia_0_5_pct)) {
  stop("Falló el chequeo de consistencia Ecuador: BCE vs Banco Mundial")
}

regional_growth <- world_bank |>
  dplyr::filter(anio %in% c(2020, 2024)) |>
  dplyr::select(pais, anio, remesas_millones_usd) |>
  tidyr::pivot_wider(
    names_from = anio,
    values_from = remesas_millones_usd,
    names_prefix = "remesas_"
  ) |>
  dplyr::mutate(
    crecimiento_2020_2024_pct = 100 * (remesas_2024 / remesas_2020 - 1)
  ) |>
  dplyr::arrange(dplyr::desc(crecimiento_2020_2024_pct))

readr::write_csv(
  ecuador_compare,
  file.path(tables_dir, "remesas_ecuador_bce_world_bank.csv")
)
readr::write_csv(
  regional_growth,
  file.path(tables_dir, "remesas_regional_growth_world_bank.csv")
)
readr::write_csv(
  bce_crosscheck,
  file.path(tables_dir, "remesas_bce_workbook_boletin_check.csv")
)
readr::write_csv(
  bce_origin_2025,
  file.path(tables_dir, "remesas_origen_bce_2025.csv")
)

saveRDS(
  list(
    world_bank = world_bank,
    bce_annual = bce_annual,
    bce_origin_2025 = bce_origin_2025,
    bce_workbook_annual = bce_workbook_annual,
    bce_crosscheck = bce_crosscheck,
    ecuador_compare = ecuador_compare,
    ecuador_gdp = ecuador_gdp,
    ecuador_remesas_pib = ecuador_remesas_pib,
    regional_growth = regional_growth,
    sources = list(
      world_bank_indicator = wb_indicator,
      world_bank_gdp_indicator = gdp_indicator,
      bce_pdf = bce_pdf_url,
      bce_workbook = bce_workbook_url
    )
  ),
  file.path(processed_dir, "remesas_regional_world_bank_bce.rds")
)

message("\nComparación Ecuador: Banco Mundial vs BCE")
print(as.data.frame(ecuador_compare), row.names = FALSE)
message("\nCrecimiento regional 2020-2024 según Banco Mundial")
print(as.data.frame(regional_growth), row.names = FALSE)
message("\nChequeo de calidad Banco Mundial")
print(as.data.frame(quality_world_bank), row.names = FALSE)
message("\nChequeo base histórica BCE vs boletín BCE")
print(as.data.frame(bce_crosscheck), row.names = FALSE)
message("\nGuardado: ", file.path(processed_dir, "remesas_regional_world_bank_bce.rds"))
