# ============================================================
# clean_homicidios_hora.R
# Cuenta asesinatos por hora del día en Ecuador.
# Requiere: data/raw/homicidios/mdi_homicidiosintencionales_pm_2014_2026.xlsx
# Guarda:   data/processed/homicidios_hora.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_homicidios_hora.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "lubridate", "readxl", "tidyr"))

input_path <- "data/raw/homicidios/mdi_homicidiosintencionales_pm_2014_2026.xlsx"
out_path <- "data/processed/homicidios_hora.rds"

period_start <- as.Date("2017-01-01")
period_end <- as.Date("2025-12-31")

datos <- readxl::read_excel(input_path)

required_columns <- c("tipo_muerte", "fecha_infraccion", "hora_infraccion")
missing_columns <- setdiff(required_columns, names(datos))
if (length(missing_columns) > 0) {
  stop("Faltan columnas requeridas: ", paste(missing_columns, collapse = ", "))
}

fecha_min <- as.Date(min(datos$fecha_infraccion, na.rm = TRUE))
fecha_max <- as.Date(max(datos$fecha_infraccion, na.rm = TRUE))

hora_raw <- trimws(as.character(datos$hora_infraccion))
hora_raw[hora_raw %in% c("", "SIN_DATO")] <- NA_character_

datos_limpios <- datos %>%
  mutate(
    fecha = as.Date(fecha_infraccion),
    hora = lubridate::hour(lubridate::hms(hora_raw, quiet = TRUE))
  )

asesinatos <- datos_limpios %>%
  filter(
    tipo_muerte == "ASESINATO",
    fecha >= period_start,
    fecha <= period_end,
    !is.na(hora),
    hora >= 0,
    hora <= 23
  )

if (nrow(asesinatos) == 0) {
  stop("No se encontraron asesinatos con hora válida en el periodo seleccionado.")
}

por_hora <- asesinatos %>%
  count(hora, name = "asesinatos") %>%
  tidyr::complete(hora = 0:23, fill = list(asesinatos = 0L)) %>%
  arrange(hora) %>%
  mutate(
    participacion = asesinatos / sum(asesinatos),
    etiqueta_hora = sprintf("%02d:00", hora)
  )

metadata <- list(
  source = "https://github.com/angel-cloud976/LIDE-Grafico_Horas/blob/cb2b375/Homicidios/mdi_homicidiosintencionales_pm_2014_2026.xlsx",
  source_coverage = c(as.character(fecha_min), as.character(fecha_max)),
  period = c(as.character(period_start), as.character(period_end)),
  filter = "tipo_muerte == 'ASESINATO'",
  excluded_missing_hour = sum(
    datos_limpios$tipo_muerte == "ASESINATO" &
      datos_limpios$fecha >= period_start &
      datos_limpios$fecha <= period_end &
      is.na(datos_limpios$hora),
    na.rm = TRUE
  ),
  n_records = nrow(asesinatos),
  peak_hour = por_hora$hora[which.max(por_hora$asesinatos)]
)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(data = por_hora, metadata = metadata), out_path)
message("Guardado: ", out_path, " (", nrow(por_hora), " horas; ", metadata$n_records, " asesinatos)")
