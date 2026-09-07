# ============================================================
# clean_reem_antiguedad_empresas_activas.R
# Tabula la antigüedad de las empresas activas del REEM 2025.
# Requiere: data/raw/reem/DATOS_ABIERTOS_REEM_2025.zip (no versionado)
# Guarda:   data/raw/reem/reem_2025_antiguedad_empresas_activas.csv
#           data/processed/reem_antiguedad_empresas_activas.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_reem_antiguedad_empresas_activas.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr", "tibble"))

input_path <- "data/raw/reem/DATOS_ABIERTOS_REEM_2025.zip"
tabulado_path <- "data/raw/reem/reem_2025_antiguedad_empresas_activas.csv"
out_path <- "data/processed/reem_antiguedad_empresas_activas.rds"
outer_member <- "DATOS_ABIERTOS_REEM_2025/BDD_DATOS_ABIERTOS_EMPRESAS.zip"
company_member <- "BDD_DATOS_ABIERTOS_EMPRESAS/EMPRESAS_periodo_2025.zip"
csv_member <- "EMPRESAS_periodo_2025.csv"
reference_year <- 2025L

if (!file.exists(input_path)) {
  stop(
    "No se encontró el microdato local REEM 2025: ", input_path,
    ". Descárgalo desde la URL registrada en data/sources/",
    "reem_antiguedad_empresas_activas.md. Este ZIP no se versiona."
  )
}

temp_dir <- tempfile("reem_2025_")
dir.create(temp_dir)
on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

unzip(input_path, files = outer_member, exdir = temp_dir)
outer_path <- file.path(temp_dir, outer_member)

if (!file.exists(outer_path)) {
  stop("El ZIP REEM no contiene ", outer_member, ".")
}

unzip(outer_path, files = company_member, exdir = temp_dir)
company_path <- file.path(temp_dir, company_member)

if (!file.exists(company_path)) {
  stop("El archivo intermedio no contiene ", company_member, ".")
}

unzip(company_path, files = csv_member, exdir = temp_dir)
csv_path <- file.path(temp_dir, csv_member)

if (!file.exists(csv_path)) {
  stop("El archivo de empresas no contiene ", csv_member, ".")
}

datos <- readr::read_delim(
  csv_path,
  delim = ";",
  locale = readr::locale(encoding = "UTF-16LE"),
  col_select = "fecha_inicio_actividad",
  col_types = readr::cols(fecha_inicio_actividad = readr::col_character()),
  na = c("", "-1"),
  show_col_types = FALSE,
  progress = FALSE
)

fecha_inicio <- as.Date(
  datos$fecha_inicio_actividad,
  format = "%Y%m%d"
)

validas <- !is.na(fecha_inicio) &
  fecha_inicio >= as.Date("1900-01-01") &
  fecha_inicio <= as.Date("2025-12-31")

antiguedad <- reference_year - as.integer(format(fecha_inicio[validas], "%Y"))

if (length(antiguedad) != 1204165L) {
  stop("El número de empresas con fecha de inicio válida no coincide con 1.204.165.")
}

tabulado <- tibble::tibble(antiguedad_anios = 0:60) |>
  dplyr::left_join(
    tibble::tibble(antiguedad_anios = pmin(antiguedad, 60L)) |>
      dplyr::count(antiguedad_anios, name = "empresas_activas"),
    by = "antiguedad_anios"
  ) |>
  dplyr::mutate(
    empresas_activas = dplyr::coalesce(empresas_activas, 0L),
    antiguedad_etiqueta = dplyr::if_else(
      antiguedad_anios == 60L,
      "60 o más",
      as.character(antiguedad_anios)
    ),
    porcentaje_empresas = empresas_activas / sum(empresas_activas),
    anio_referencia = reference_year
  )

if (median(antiguedad) != 11L || abs(mean(antiguedad) - 12.89) > 0.01) {
  stop("La distribución de antigüedad no reproduce los estadísticos esperados.")
}

dir.create(dirname(tabulado_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

readr::write_csv(tabulado, tabulado_path)
saveRDS(
  list(
    antiguedad = tabulado,
    empresas_total = sum(tabulado$empresas_activas),
    mediana_anios = median(antiguedad),
    promedio_anios = mean(antiguedad),
    empresas_100_mas = sum(antiguedad >= 100L),
    porcentaje_empresas_100_mas = mean(antiguedad >= 100L),
    empresas_excluidas = nrow(datos) - length(antiguedad),
    reference_year = reference_year
  ),
  out_path
)

message("Guardado: ", tabulado_path)
message("Guardado: ", out_path)
