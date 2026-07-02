# ============================================================
# clean_enemdu_actividad_fisica_insuficiente.R
# Prepara la prevalencia 2024 de actividad física insuficiente
# por área de residencia y grupo etario usando el módulo de
# actividad física de la ENEMDU.
# Requiere: data/raw/actividad_fisica_joan/2024_12/2_BDD_DATOS_ABIERTOS_ACTIVIDAD_FISICA_2024_12_CSV.csv
# Guarda:   data/processed/enemdu_actividad_fisica_insuficiente.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_actividad_fisica_insuficiente.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("data.table"))

input_path <- "data/raw/actividad_fisica_joan/2024_12/2_BDD_DATOS_ABIERTOS_ACTIVIDAD_FISICA_2024_12_CSV.csv"
year_num <- 2024L

enemdu_df <- fread(
  input_path,
  sep = ";",
  dec = ",",
  na.strings = ""
)

enemdu_df[, area_residencia := fifelse(area == 1, "Entorno urbano", "Entorno rural")]

ninos_df <- enemdu_df[
  p03 >= 8 & p03 <= 17 & !is.na(fexp),
  .(
    prevalencia = 100 * weighted.mean(af101 < 7, fexp, na.rm = TRUE)
  ),
  by = .(area_residencia)
]
ninos_df[, `:=`(anio = year_num, grupo_edad = "8-17 años")]

adultos_df <- copy(enemdu_df[p03 >= 18 & p03 <= 69 & !is.na(fexp)])
adultos_df[
  ,
  minutos_vigorosa := fifelse(
    af201cod == 1 & !is.na(af201d) & !is.na(af201h) & !is.na(af201m),
    af201d * (af201h * 60 + af201m),
    0
  )
]
adultos_df[
  ,
  minutos_moderada := fifelse(
    af202cod == 1 & !is.na(af202d) & !is.na(af202h) & !is.na(af202m),
    af202d * (af202h * 60 + af202m),
    0
  )
]
adultos_df[
  ,
  minutos_caminata := fifelse(
    af203cod == 1 & !is.na(af203d) & !is.na(af203h) & !is.na(af203m),
    af203d * (af203h * 60 + af203m),
    0
  )
]
adultos_df[
  ,
  actividad_insuficiente := minutos_moderada + minutos_caminata + 2 * minutos_vigorosa < 150
]

adultos_resumen <- adultos_df[
  ,
  .(
    prevalencia = 100 * weighted.mean(actividad_insuficiente, fexp, na.rm = TRUE)
  ),
  by = .(area_residencia)
]
adultos_resumen[, `:=`(anio = year_num, grupo_edad = "18-69 años")]

activity_series <- rbindlist(list(ninos_df, adultos_resumen), use.names = TRUE)

setcolorder(activity_series, c("anio", "grupo_edad", "area_residencia", "prevalencia"))
setorder(activity_series, grupo_edad, area_residencia, anio)
activity_series[, prevalencia := round(prevalencia, 1)]

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
out_path <- "data/processed/enemdu_actividad_fisica_insuficiente.rds"
saveRDS(activity_series, out_path)
message("Guardado: ", out_path)
