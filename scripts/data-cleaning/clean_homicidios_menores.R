# ============================================================
# clean_homicidios_menores.R
# Cuenta víctimas menores de 18 años de muertes violentas intencionales,
# por año (enero a septiembre) y por provincia (2020-2026).
# Requiere: data/raw/homicidios/mdi_homicidiosintencionales_2014_sep2026.xlsx
# Guarda:   data/processed/homicidios_menores.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_homicidios_menores.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "lubridate", "readxl"))

input_path <- "data/raw/homicidios/mdi_homicidiosintencionales_2014_sep2026.xlsx"
out_path <- "data/processed/homicidios_menores.rds"

anio_ini <- 2014
anio_fin <- 2026
mes_max <- 9            # período comparable: enero a septiembre de cada año
anio_provincias <- 2020 # el gráfico provincial acumula 2020 a 2026

datos <- readxl::read_excel(input_path)

required_columns <- c("tipo_muerte", "fecha_infraccion", "edad", "medida_edad", "provincia")
missing_columns <- setdiff(required_columns, names(datos))
if (length(missing_columns) > 0) {
  stop("Faltan columnas requeridas: ", paste(missing_columns, collapse = ", "))
}

unidad <- toupper(trimws(as.character(datos$medida_edad)))
edad_num <- suppressWarnings(as.numeric(gsub(",", ".", as.character(datos$edad))))

# Edad en años; meses y días se convierten a fracción de año
edad_anios <- dplyr::case_when(
  unidad == "A" ~ edad_num,
  unidad == "M" ~ edad_num / 12,
  unidad == "D" ~ edad_num / 365.25,
  TRUE ~ NA_real_
)

limpiar_provincia <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "SIN_DATO", "SIN DATO", "NA", "N/A", "NO DETERMINADO")] <- NA_character_
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == "STO DGO DE LOS TSÁCHILAS" ~ "Santo Domingo",
    TRUE ~ tools::toTitleCase(tolower(x))
  )
}

base <- datos |>
  dplyr::mutate(
    fecha = as.Date(fecha_infraccion),
    anio = lubridate::year(fecha),
    mes = lubridate::month(fecha),
    edad_anios = edad_anios,
    menor = !is.na(edad_anios) & edad_anios >= 0 & edad_anios < 18,
    provincia_limpia = limpiar_provincia(provincia)
  ) |>
  dplyr::filter(anio >= anio_ini, anio <= anio_fin, mes >= 1, mes <= mes_max)

anual <- base |>
  dplyr::group_by(anio) |>
  dplyr::summarise(
    total = dplyr::n(),
    menores = sum(menor),
    edad_sin_dato = sum(is.na(edad_anios)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    pct_edad_sin_dato = edad_sin_dato / total,
    grupo = dplyr::case_when(
      anio <= 2021 ~ "2014-2021",
      anio <= 2025 ~ "2022-2025",
      TRUE ~ "2026"
    )
  )

provincias <- base |>
  dplyr::filter(menor, anio >= anio_provincias, !is.na(provincia_limpia)) |>
  dplyr::count(provincia = provincia_limpia, name = "victimas", sort = TRUE) |>
  dplyr::mutate(porcentaje = victimas / sum(victimas))

stopifnot(nrow(anual) == anio_fin - anio_ini + 1, nrow(provincias) > 0)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(list(anual = anual, provincias = provincias, mes_max = mes_max,
             anio_provincias = anio_provincias), out_path)
message("Guardado: ", out_path)
