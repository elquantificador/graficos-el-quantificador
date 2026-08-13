# ============================================================
# clean_enemdu_nini_razones_sexo.R
# Author: Valeria Lizeth Marcayata Ojeda; adaptación de El Quantificador
# Purpose: Prepara las razones para no estudiar ni trabajar entre jóvenes NINI, por sexo y nivel educativo.
# Inputs:  data/raw/enemdu/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip
# Outputs: data/processed/enemdu_nini_razones_sexo.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_nini_razones_sexo.R
# ============================================================

# 0. Setup ----
source("scripts/packages.R")
ensure_packages(c("dplyr", "readr", "stringr", "tibble"))
# set.seed(42)

input_path <- file.path(
  "data",
  "raw",
  "enemdu",
  "2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip"
)
input_member <- "BDDenemdu_personas_2025_anual.csv"
out_path <- file.path(
  "data",
  "processed",
  "enemdu_nini_razones_sexo.rds"
)

required_columns <- c(
  "p03",
  "p02",
  "fexp",
  "p07",
  "empleo",
  "nnivins",
  "p09",
  "p34"
)

integer_columns <- c(
  "p03",
  "p02",
  "p07",
  "empleo",
  "nnivins",
  "p09",
  "p34"
)

expected_total_nini <- 865199
expected_women_share <- 77.3
total_tolerance <- 2
share_tolerance <- 0.1

# 1. Load data ----
if (!file.exists(input_path)) {
  stop("No se encontró la base oficial: ", input_path)
}

zip_members <- unzip(input_path, list = TRUE)$Name
if (!input_member %in% zip_members) {
  stop("El ZIP oficial no contiene ", input_member, ".")
}

input_connection <- unz(input_path, input_member, open = "rb")
datos_crudos <- read_delim(
  input_connection,
  delim = ";",
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  col_select = all_of(required_columns),
  show_col_types = FALSE,
  trim_ws = TRUE
)
close(input_connection)

missing_columns <- setdiff(required_columns, names(datos_crudos))
if (length(missing_columns) > 0) {
  stop(
    "Faltan columnas requeridas: ",
    str_c(missing_columns, collapse = ", ")
  )
}

# 2. Define the NINI population ----
datos <- datos_crudos |>
  mutate(
    across(all_of(integer_columns), as.integer),
    fexp = as.numeric(fexp)
  )

invalid_p09 <- datos |>
  filter(!is.na(p09), !p09 %in% 1:17) |>
  nrow()

invalid_p34 <- datos |>
  filter(!is.na(p34), !p34 %in% 1:12) |>
  nrow()

if (invalid_p09 > 0 || invalid_p34 > 0) {
  stop("Se encontraron códigos no documentados en p09 o p34.")
}

# Códigos verificados contra las etiquetas del archivo SPSS oficial de la
# ENEMDU anual 2025. p09 = 16 corresponde a recursos tecnológicos; p09 = 17,
# a otra razón. La entrega original tenía esos dos códigos intercambiados.
personas_nini <- datos |>
  filter(
    p03 >= 15L,
    p03 <= 29L,
    p02 %in% c(1L, 2L),
    !is.na(fexp),
    is.na(empleo) | empleo != 1L,
    is.na(p07) | p07 != 1L
  ) |>
  mutate(
    sexo = factor(
      p02,
      levels = c(1L, 2L),
      labels = c("Hombres", "Mujeres")
    ),
    nivel_educativo = case_when(
      nnivins %in% c(1L, 2L) ~ "Ninguno / Alfabetización",
      nnivins == 3L ~ "Educación básica",
      nnivins == 4L ~ "Bachillerato",
      nnivins == 5L ~ "Superior",
      TRUE ~ NA_character_
    ),
    nivel_educativo = factor(
      nivel_educativo,
      levels = c(
        "Ninguno / Alfabetización",
        "Educación básica",
        "Bachillerato",
        "Superior"
      )
    )
  ) |>
  filter(!is.na(nivel_educativo))

resumen_sexo <- personas_nini |>
  summarise(
    poblacion = sum(fexp, na.rm = TRUE),
    .by = sexo
  ) |>
  mutate(porcentaje = poblacion / sum(poblacion) * 100)

total_nini <- sum(resumen_sexo$poblacion, na.rm = TRUE)
women_share <- resumen_sexo |>
  filter(sexo == "Mujeres") |>
  pull(porcentaje)

if (abs(total_nini - expected_total_nini) > total_tolerance) {
  stop("El total ponderado de jóvenes NINI no coincide con la entrega.")
}

if (abs(women_share - expected_women_share) > share_tolerance) {
  stop("La proporción de mujeres no coincide con la entrega.")
}

# 3. Aggregate reasons for not studying ----
razones_estudio <- personas_nini |>
  mutate(
    razon = case_when(
      p09 == 3L ~ "Económico",
      p09 == 11L ~ "No le interesa",
      p09 == 15L ~ "Cuidado de hijos",
      p09 == 16L ~ "Recursos tecnológicos",
      p09 == 7L ~ "Enfermedad o discapacidad",
      p09 == 8L ~ "Quehaceres del hogar",
      p09 == 12L ~ "Embarazo",
      p09 %in% c(1L, 2L, 4L, 5L, 6L, 9L, 10L, 13L, 14L, 17L) ~
        "Otra razón",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(razon)) |>
  summarise(
    poblacion = sum(fexp, na.rm = TRUE),
    .by = c(sexo, nivel_educativo, razon)
  ) |>
  mutate(
    porcentaje = poblacion / sum(poblacion, na.rm = TRUE) * 100,
    .by = sexo
  )

# 4. Aggregate reasons for not working ----
# p34 solo se pregunta a quienes no buscaron trabajo. Un valor faltante dentro
# del universo NINI identifica a quienes sí buscaron trabajo.
razones_trabajo <- personas_nini |>
  mutate(
    razon = case_when(
      is.na(p34) ~ "Sí busca trabajo",
      p34 == 9L ~ "No tiene tiempo",
      p34 == 8L ~ "Sin deseos o necesidad",
      p34 == 11L ~ "Enfermedad o incapacidad",
      TRUE ~ "Otra razón"
    )
  ) |>
  summarise(
    poblacion = sum(fexp, na.rm = TRUE),
    .by = c(sexo, nivel_educativo, razon)
  ) |>
  mutate(
    porcentaje = poblacion / sum(poblacion, na.rm = TRUE) * 100,
    .by = sexo
  )

sum_check <- bind_rows(
  razones_estudio |> mutate(seccion = "Estudio"),
  razones_trabajo |> mutate(seccion = "Trabajo")
) |>
  summarise(
    porcentaje = sum(porcentaje, na.rm = TRUE),
    .by = c(seccion, sexo)
  )

if (any(abs(sum_check$porcentaje - 100) > 1e-8)) {
  stop("Las distribuciones porcentuales no suman 100%.")
}

# 5. Export ----
chart_data <- list(
  razones_estudio = razones_estudio,
  razones_trabajo = razones_trabajo,
  resumen_sexo = resumen_sexo,
  metadata = list(
    total_nini = total_nini,
    women_share = women_share,
    age_range = c(15L, 29L),
    year = 2025L,
    source = "INEC, ENEMDU anual 2025",
    source_url = paste0(
      "https://www.ecuadorencifras.gob.ec/documentos/web-inec/",
      "EMPLEO/2025/anual/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip"
    ),
    coding_note = paste(
      "La adaptación corrige p09: 16 = recursos tecnológicos y",
      "17 = otra razón, según las etiquetas oficiales."
    )
  )
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(chart_data, out_path)
message("Guardado: ", out_path)
invisible(sessionInfo())
