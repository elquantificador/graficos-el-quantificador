# ============================================================
# clean_ecuatorianos_altos.R
# Carga ENSANUT 2018 y prepara los datos para el gráfico de
# estatura e ingresos laborales por sexo.
# Guarda: data/processed/ecuatorianos_altos_ensanut_2018.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_ecuatorianos_altos.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven"))

zip_path <- "data/raw/ensanut/1_BDD_ENS2018_f1_personas.dta.zip"
dta_name <- "1_BDD_ENS2018_f1_personas.dta"
tmp_dir <- tempfile("ens2018_")
dir.create(tmp_dir)
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

utils::unzip(zip_path, files = dta_name, exdir = tmp_dir)
df_raw <- read_dta(file.path(tmp_dir, dta_name))

df_clean <- df_raw %>%
  mutate(
    sexo = as_factor(sexo),
    sexo = recode(sexo, hombre = "Hombre", mujer = "Mujer"),
    etnia = as_factor(etnia),
    edadanios = as.numeric(edadanios),
    f1_s3_18 = as.numeric(f1_s3_18),
    f1_s3_18 = na_if(f1_s3_18, 999999),
    f1_s3_18 = if_else(f1_s3_18 < 0, NA_real_, f1_s3_18),
    estatura = rowMeans(cbind(f1_s7_6_1, f1_s7_6_2, f1_s7_6_3), na.rm = TRUE),
    estatura = if_else(is.nan(estatura), NA_real_, estatura),
    linc = log(f1_s3_18)
  ) %>%
  filter(
    etnia == "Mestizo",
    between(edadanios, 40, 50),
    is.finite(estatura),
    is.finite(linc)
  ) %>%
  select(sexo, estatura, linc, edadanios, etnia, fexp, upm, estrato)

dir.create("data/processed", showWarnings = FALSE)
saveRDS(df_clean, "data/processed/ecuatorianos_altos_ensanut_2018.rds")
message("Guardado: data/processed/ecuatorianos_altos_ensanut_2018.rds  (", nrow(df_clean), " filas)")
