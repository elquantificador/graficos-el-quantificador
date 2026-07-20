# ============================================================
# clean_inec_proyecciones_edades.R
# Lee las proyecciones poblacionales del INEC por edad simple y
# arma la composición porcentual por grandes grupos etarios (1950-2050).
# Requiere: data/raw/inec/tabul_nac_edad_sim_1950-2050.xlsx
# Guarda:   data/processed/inec_proyecciones_edades.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_inec_proyecciones_edades.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "readxl"))

input_path <- "data/raw/inec/tabul_nac_edad_sim_1950-2050.xlsx"
out_path <- "data/processed/inec_proyecciones_edades.rds"

poblacion_ancha <- readxl::read_excel(
  input_path,
  sheet = "población_ambos_sexos",
  range = "B18:CY118",
  col_names = FALSE
)
names(poblacion_ancha) <- c("edad", 1950:2050)

datos_composicion <- poblacion_ancha |>
  pivot_longer(cols = -edad, names_to = "anio", values_to = "poblacion") |>
  mutate(
    anio = as.numeric(anio),
    edad = as.numeric(edad)
  ) |>
  mutate(
    grupo_edad = case_when(
      edad < 15 ~ "0-14 años",
      edad >= 15 & edad <= 24 ~ "15-24 años",
      edad >= 25 & edad <= 54 ~ "25-54 años",
      edad >= 55 & edad <= 64 ~ "55-64 años",
      edad >= 65 ~ "65 años y más"
    )
  ) |>
  group_by(anio, grupo_edad) |>
  summarise(pob_grupo = sum(poblacion, na.rm = TRUE), .groups = "drop_last") |>
  mutate(porcentaje = pob_grupo / sum(pob_grupo)) |>
  ungroup() |>
  mutate(
    grupo_edad = factor(
      grupo_edad,
      levels = c("0-14 años", "15-24 años", "25-54 años", "55-64 años", "65 años y más")
    )
  )

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(datos_composicion, out_path)
message("Guardado: ", out_path)
