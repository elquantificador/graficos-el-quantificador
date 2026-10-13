# ============================================================
# clean_rural_income_electricity.R
# Construye una serie de ingreso laboral rural de ENEMDU y una
# serie nacional de recaudación del servicio eléctrico de ARCONEL.
#
# La ventana común es 2007-2025. Los dos indicadores se expresan
# en dólares de diciembre de 2025 y después se indexan a 2007 = 100.
# Guarda:
#   data/processed/rural_income_electricity_annual.rds
#   outputs/tables/draft_rural_income_electricity_annual.csv
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_rural_income_electricity.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven", "readr", "readxl", "tibble"))

start_year <- 2007L
end_year <- 2025L

income_reference_path <- "data/raw/enemdu/rural_income_reference.csv"
cpi_path <- "data/raw/ipc/ipc_december_indices.csv"
arconel_dir <- "data/raw/arconel/facturacion_total"

income_reference <- readr::read_csv(
  income_reference_path,
  show_col_types = FALSE
)

cpi <- readr::read_csv(cpi_path, show_col_types = FALSE)
target_cpi <- cpi |>
  dplyr::filter(year == end_year) |>
  dplyr::pull(december_index)

if (length(target_cpi) != 1L) {
  stop("La serie del IPC debe tener un único valor para diciembre de 2025.")
}

income_from_december_sav <- function(year, path) {
  if (!file.exists(path)) {
    stop("No se encontró el microdato ENEMDU: ", path)
  }

  data <- haven::read_sav(path) |>
    dplyr::transmute(
      area = as.numeric(area),
      empleo = as.numeric(empleo),
      ingrl = as.numeric(haven::zap_labels(ingrl)),
      fexp = as.numeric(haven::zap_labels(fexp))
    ) |>
    dplyr::filter(
      area == 2,
      empleo == 1,
      ingrl > 0,
      ingrl < 999999,
      is.finite(ingrl),
      is.finite(fexp),
      fexp > 0
    )

  if (nrow(data) == 0L) {
    stop("No quedaron personas con empleo e ingreso válido para ", year, ".")
  }

  tibble::tibble(
    year = year,
    value = stats::weighted.mean(data$ingrl, data$fexp, na.rm = TRUE),
    base_year = year,
    basis = "diciembre",
    source_note = paste0("ENEMDU diciembre ", year, ", microdatos; cálculo ponderado")
  )
}

# La fuente histórica del INEC llega a 2016. Para 2017-2020 se usa
# diciembre porque no hay un agregado anual comparable para 2020.
income_december <- dplyr::bind_rows(
  income_from_december_sav(2017, "data/raw/enemdu/enemdu_persona_2017_12.sav"),
  income_from_december_sav(2018, "data/raw/enemdu/ENEMDU_PERSONAS_2018_12_hom.sav"),
  income_from_december_sav(2019, "data/raw/enemdu/enemdu_persona_201912.sav"),
  income_from_december_sav(2020, "data/raw/enemdu/enemdu_persona_2020_12.sav")
)

income <- dplyr::bind_rows(income_reference, income_december) |>
  dplyr::filter(year >= start_year, year <= end_year) |>
  dplyr::arrange(year) |>
  dplyr::left_join(
    cpi |>
      dplyr::rename(base_year = year, ipc_base = december_index),
    by = "base_year"
  ) |>
  dplyr::mutate(
    ipc_target = target_cpi,
    income_dec_2025_usd = value * ipc_target / ipc_base,
    income_basis = dplyr::if_else(basis == "anual", "ENEMDU anual", "ENEMDU diciembre")
  )

if (anyDuplicated(income$year) > 0L) {
  stop("La serie de ingreso tiene años duplicados.")
}

arconel_headers <- c(
  "id_empresa",
  "empresa",
  "year_reported",
  "month_id",
  "month",
  "grupo_consumo",
  "tipo_cliente",
  "clientes",
  "energia_mwh",
  "facturacion_usd",
  "recaudacion_usd",
  "subsidio_usd"
)

read_arconel_facturacion <- function(year) {
  path <- file.path(arconel_dir, paste0("facturacion_total_", year, ".xlsx"))
  if (!file.exists(path)) {
    stop("No se encontró el reporte ARCONEL para ", year, ": ", path)
  }

  raw <- readxl::read_excel(
    path,
    sheet = 1,
    skip = 1,
    col_names = FALSE,
    .name_repair = "minimal"
  )

  if (ncol(raw) != length(arconel_headers)) {
    stop(
      "El reporte ARCONEL de ", year,
      " no tiene las 12 columnas esperadas. Tiene ", ncol(raw), "."
    )
  }

  names(raw) <- arconel_headers

  cleaned <- raw |>
    dplyr::transmute(
      year = year,
      year_reported = as.integer(readr::parse_number(as.character(year_reported))),
      id_empresa = readr::parse_number(as.character(id_empresa)),
      empresa = as.character(empresa),
      month_id = readr::parse_number(as.character(month_id)),
      month = as.character(month),
      grupo_consumo = as.character(grupo_consumo),
      tipo_cliente = as.character(tipo_cliente),
      clientes = readr::parse_number(as.character(clientes)),
      energia_mwh = readr::parse_number(as.character(energia_mwh)),
      facturacion_usd = readr::parse_number(as.character(facturacion_usd)),
      recaudacion_usd = readr::parse_number(as.character(recaudacion_usd)),
      subsidio_usd = readr::parse_number(as.character(subsidio_usd))
    ) |>
    dplyr::filter(
      !is.na(year_reported),
      !is.na(recaudacion_usd)
    )

  if (nrow(cleaned) == 0L) {
    stop("El reporte ARCONEL de ", year, " no contiene filas válidas.")
  }

  reported_years <- unique(cleaned$year_reported)
  if (length(reported_years) != 1L || reported_years != year) {
    stop(
      "El reporte ARCONEL ", basename(path),
      " declara años distintos de ", year, "."
    )
  }

  cleaned
}

arconel_rows <- lapply(start_year:end_year, read_arconel_facturacion) |>
  dplyr::bind_rows()

collection <- arconel_rows |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    arconel_rows = dplyr::n(),
    arconel_companies = dplyr::n_distinct(id_empresa),
    clients = sum(clientes, na.rm = TRUE),
    energy_mwh = sum(energia_mwh, na.rm = TRUE),
    facturacion_usd_nominal = sum(facturacion_usd, na.rm = TRUE),
    recaudacion_usd_nominal = sum(recaudacion_usd, na.rm = TRUE),
    subsidio_usd = sum(subsidio_usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    cpi |>
      dplyr::rename(year = year, ipc_base = december_index),
    by = "year"
  ) |>
  dplyr::mutate(
    recaudacion_dec_2025_usd = recaudacion_usd_nominal * target_cpi / ipc_base,
    arconel_source_note = paste0(
      "ARCONEL, reporte Facturación Total ", year,
      "; suma de las filas del año y del campo Recaudación Servicio Eléctrico (USD)"
    )
  ) |>
  dplyr::select(
    year,
    arconel_rows,
    arconel_companies,
    clients,
    energy_mwh,
    facturacion_usd_nominal,
    recaudacion_usd_nominal,
    recaudacion_dec_2025_usd,
    subsidio_usd,
    arconel_source_note
  )

expected_years <- start_year:end_year
if (!all(collection$year == expected_years)) {
  stop("La serie de recaudación no tiene exactamente los años 2007-2025.")
}

annual <- tibble::tibble(year = expected_years) |>
  dplyr::left_join(income, by = "year") |>
  dplyr::left_join(collection, by = "year") |>
  dplyr::mutate(
    income_index_2007 = income_dec_2025_usd /
      income_dec_2025_usd[year == start_year] * 100,
    recaudacion_index_2007 = recaudacion_dec_2025_usd /
      recaudacion_dec_2025_usd[year == start_year] * 100
  )

if (!all(annual$year == expected_years)) {
  stop("La tabla final no está ordenada por año.")
}

required_numeric <- c(
  "income_dec_2025_usd",
  "recaudacion_dec_2025_usd",
  "income_index_2007",
  "recaudacion_index_2007"
)
if (any(!vapply(annual[required_numeric], function(x) all(is.finite(x)), logical(1)))) {
  stop("La tabla final contiene valores no finitos.")
}

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

saveRDS(annual, "data/processed/rural_income_electricity_annual.rds")
readr::write_csv(
  annual,
  "outputs/tables/draft_rural_income_electricity_annual.csv",
  na = ""
)

message("Años procesados: ", paste(range(annual$year), collapse = "-"))
message("Filas ARCONEL: ", sum(annual$arconel_rows))
message("Ingreso rural disponible: ", sum(!is.na(annual$income_dec_2025_usd)), " años")
message("Recaudación eléctrica disponible: ", sum(!is.na(annual$recaudacion_dec_2025_usd)), " años")
message("Guardado: data/processed/rural_income_electricity_annual.rds")
message("Guardado: outputs/tables/draft_rural_income_electricity_annual.csv")
