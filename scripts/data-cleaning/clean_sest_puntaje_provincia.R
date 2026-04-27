# ============================================================
# clean_sest_puntaje_provincia.R
# Carga y procesa los microdatos de Ser Estudiante 2024-2025
# para obtener el puntaje global promedio por provincia.
# Requiere: data/raw/sest/SEST25_micro_50578_20251215_SAV.sav
# Guarda:   data/processed/sest_puntaje_provincia.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_sest_puntaje_provincia.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven"))

infile <- "data/raw/sest/SEST25_micro_50578_20251215_SAV.sav"
outfile <- "data/processed/sest_puntaje_provincia.rds"

province_drop <- c("Zona No Delimitada", "En el Exterior")

df_raw <- read_sav(infile)

df_clean <- df_raw %>%
  mutate(
    provincia = as_factor(id_prov),
    region = as_factor(nm_regi),
    grado_label = as_factor(grado),
    estado_eval_label = as_factor(estado_eval)
  ) %>%
  filter(
    estado_eval_label == "Evaluado",
    !is.na(inev),
    !is.na(fex_inev),
    !is.na(provincia),
    !provincia %in% province_drop
  )

puntaje_provincia <- df_clean %>%
  group_by(provincia) %>%
  summarise(
    puntaje_global = weighted.mean(inev, w = fex_inev, na.rm = TRUE),
    n = n(),
    peso_total = sum(fex_inev, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(puntaje_global))

puntaje_provincia_grado <- df_clean %>%
  group_by(provincia, grado_label) %>%
  summarise(
    puntaje_global = weighted.mean(inev, w = fex_inev, na.rm = TRUE),
    n = n(),
    peso_total = sum(fex_inev, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(grado_label, desc(puntaje_global))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  list(
    province_scores = puntaje_provincia,
    province_grade_scores = puntaje_provincia_grado,
    student_level = df_clean %>%
      select(
        provincia,
        region,
        grado = grado_label,
        sexo = tp_sexo,
        area = tp_area,
        etnia = etnibee,
        financiamiento,
        sostenimiento,
        inev,
        fex_inev
      )
  ),
  outfile
)

message("Guardado: ", outfile)
