# ============================================================
# clean_uso_tiempo.R
# Carga las bases 2019 de uso del tiempo y personas, prepara
# el dataset para graficar tiempo dedicado a cocinar.
# Guarda: data/processed/S51P2_UT2019_clean.rds
# ============================================================
# Ejecutar desde la raiz del proyecto:
#   Rscript scripts/data-cleaning/clean_uso_tiempo.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "forcats", "readr", "tibble"))

uso_tiempo_2019_path <- "data/raw/uso_tiempo/201912_multibdd_uso_del_tiempo.sav.csv"
personas_2019_path <- "data/raw/uso_tiempo/201912_multibdd_personas.sav.csv"
output_path <- "data/processed/S51P2_UT2019_clean.rds"

if (!file.exists(uso_tiempo_2019_path) || !file.exists(personas_2019_path)) {
  stop(
    "No se encontraron las bases 2019 requeridas: ",
    uso_tiempo_2019_path,
    " y ",
    personas_2019_path,
    call. = FALSE
  )
}

data_uso_tiempo_2019 <- read_csv2(uso_tiempo_2019_path, show_col_types = FALSE)
data_personas_2019 <- read_csv2(personas_2019_path, show_col_types = FALSE)

df_info_personas_2019 <- tibble(
  id_per = data_personas_2019$id_per,
  s1p2 = data_personas_2019$s1p2,
  s1p3 = data_personas_2019$s1p3
)

df_analisis_uso_tiempo_2019 <- merge(df_info_personas_2019, data_uso_tiempo_2019, by = "id_per")

df_clean <- df_analisis_uso_tiempo_2019 %>%
  transmute(
    id_per = id_per,
    ciudad = ciudad,
    sexo = fct_recode(factor(s1p2), "Hombre" = "1", "Mujer" = "2"),
    edad = s1p3,
    s51p1 = fct_recode(factor(s51p1), "Si" = "1", "No" = "2"),
    s51p2 = fct_recode(factor(s51p2), "Si" = "1", "No" = "2"),
    s51p2a = coalesce(s51p2a, 0),
    s51p2b = coalesce(s51p2b, 0),
    s51p2c = coalesce(s51p2c, 0),
    s51p2d = coalesce(s51p2d, 0),
    conglomerado = conglomerado,
    estrato = estrato,
    upm = upm,
    fexp = fexp
  ) %>%
  mutate(
    t_horas_cocina = s51p2a + (s51p2b / 60) + s51p2c + (s51p2d / 60),
    edad_rango = case_when(
      edad <= 11 ~ "Edad entre 0 y 11 anos",
      between(edad, 12, 19) ~ "Edad entre 12 y 19 anos",
      between(edad, 20, 29) ~ "Edad entre 20 y 29 anos",
      between(edad, 30, 39) ~ "Edad entre 30 y 39 anos",
      between(edad, 40, 49) ~ "Edad entre 40 y 49 anos",
      between(edad, 50, 59) ~ "Edad entre 50 y 59 anos",
      between(edad, 60, 69) ~ "Edad entre 60 y 69 anos",
      edad >= 70 ~ "Edad mayor a 70 anos",
      edad == 99 ~ "No informa"
    ),
    edad_rango = factor(edad_rango),
    prov = factor(substr(ciudad, start = 1, stop = 2))
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(df_clean, output_path)
message("Guardado: ", output_path, "  (", nrow(df_clean), " filas)")
