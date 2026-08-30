# ============================================================
# clean_enemdu_juventud_empleo_2025.R
# Author: Cristhian Guamán Saca; adaptación de El Quantificador
# Purpose: Calcula empleo adecuado y desempleo juvenil por sexo y provincia.
# Requiere: data/raw/enemdu/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip
# Guarda:   data/processed/enemdu_juventud_empleo_2025.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_juventud_empleo_2025.R
# ============================================================

# 0. Setup ----
source("scripts/packages.R")
ensure_packages(c("dplyr", "readr", "tibble"))

input_path <- file.path(
  "data", "raw", "enemdu", "2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip"
)
input_member <- "BDDenemdu_personas_2025_anual.csv"
out_path <- file.path(
  "data", "processed", "enemdu_juventud_empleo_2025.rds"
)

required_columns <- c(
  "p02", "p03", "prov", "condact", "empleo", "desempleo", "fexp"
)

province_labels <- c(
  `1` = "Azuay",
  `2` = "Bolívar",
  `3` = "Cañar",
  `4` = "Carchi",
  `5` = "Cotopaxi",
  `6` = "Chimborazo",
  `7` = "El Oro",
  `8` = "Esmeraldas",
  `9` = "Guayas",
  `10` = "Imbabura",
  `11` = "Loja",
  `12` = "Los Ríos",
  `13` = "Manabí",
  `14` = "Morona Santiago",
  `15` = "Napo",
  `16` = "Pastaza",
  `17` = "Pichincha",
  `18` = "Tungurahua",
  `19` = "Zamora Chinchipe",
  `20` = "Galápagos",
  `21` = "Sucumbíos",
  `22` = "Orellana",
  `23` = "Santo Domingo de los Tsáchilas",
  `24` = "Santa Elena"
)

# 1. Load the official annual microdata ----
if (!file.exists(input_path)) {
  stop("No se encontró la base oficial: ", input_path)
}

zip_members <- unzip(input_path, list = TRUE)$Name
if (!input_member %in% zip_members) {
  stop("El ZIP oficial no contiene ", input_member, ".")
}

input_connection <- unz(input_path, input_member, open = "rb")
datos_crudos <- readr::read_delim(
  input_connection,
  delim = ";",
  locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
  col_select = dplyr::all_of(required_columns),
  show_col_types = FALSE,
  trim_ws = TRUE
)
close(input_connection)

missing_columns <- setdiff(required_columns, names(datos_crudos))
if (length(missing_columns) > 0) {
  stop("Faltan columnas requeridas: ", paste(missing_columns, collapse = ", "))
}

# 2. Define the young PEA and indicators ----
# The 2025 INEC labels verify condact = 1 as Empleo Adecuado/Pleno and
# desempleo = 1 as the union of open and hidden unemployment.
datos <- datos_crudos |>
  mutate(
    across(c(p02, p03, prov, condact, empleo, desempleo), as.integer),
    fexp = as.numeric(fexp)
  ) |>
  filter(
    p03 >= 18L,
    p03 <= 29L,
    p02 %in% c(1L, 2L),
    prov %in% seq_along(province_labels),
    !is.na(fexp),
    fexp >= 0
  ) |>
  mutate(
    sexo = factor(
      p02,
      levels = c(1L, 2L),
      labels = c("Hombres", "Mujeres")
    ),
    provincia = unname(province_labels[as.character(prov)]),
    pea = !is.na(empleo) | !is.na(desempleo),
    empleo_adecuado = pea & condact == 1L,
    desempleado = pea & !is.na(desempleo)
  ) |>
  filter(pea)

if (nrow(datos) == 0L) {
  stop("El filtro de jóvenes en la PEA no produjo observaciones.")
}

if (anyNA(datos$provincia)) {
  stop("Hay códigos provinciales sin etiqueta.")
}

if (any(!is.na(datos$condact) & !(datos$condact %in% 0:9))) {
  stop("Se encontraron códigos no documentados en condact.")
}

if (any(!is.na(datos$empleo) & datos$empleo != 1L)) {
  stop("Se encontraron códigos no documentados en empleo.")
}

if (any(!is.na(datos$desempleo) & datos$desempleo != 1L)) {
  stop("Se encontraron códigos no documentados en desempleo.")
}

# 3. Weighted summaries ----
resumen_sexo <- datos |>
  summarise(
    pea = sum(fexp, na.rm = TRUE),
    empleo_adecuado = sum(fexp[empleo_adecuado], na.rm = TRUE),
    desempleo = sum(fexp[desempleado], na.rm = TRUE),
    .by = sexo
  ) |>
  mutate(
    empleo_adecuado_pct = 100 * empleo_adecuado / pea,
    desempleo_pct = 100 * desempleo / pea
  )

resumen_provincia <- datos |>
  summarise(
    pea = sum(fexp, na.rm = TRUE),
    empleo_adecuado = sum(fexp[empleo_adecuado], na.rm = TRUE),
    empleo_adecuado_pct = 100 * empleo_adecuado / pea,
    .by = provincia
  ) |>
  arrange(desc(empleo_adecuado_pct), provincia)

if (nrow(resumen_sexo) != 2L || nrow(resumen_provincia) != 24L) {
  stop("La agregación no produjo los dos sexos y las 24 provincias esperadas.")
}

# 4. Checks against the submitted visualization ----
sex_checks <- tibble::tribble(
  ~sexo, ~empleo_adecuado_pct, ~desempleo_pct,
  "Hombres", 36.7, 6.3,
  "Mujeres", 26.7, 11.5
)

sex_check <- resumen_sexo |>
  mutate(sexo = as.character(sexo)) |>
  inner_join(sex_checks, by = "sexo", suffix = c("_calculado", "_referencia"))

if (
  nrow(sex_check) != 2L ||
    any(abs(sex_check$empleo_adecuado_pct_calculado - sex_check$empleo_adecuado_pct_referencia) > 0.1) ||
    any(abs(sex_check$desempleo_pct_calculado - sex_check$desempleo_pct_referencia) > 0.1)
) {
  stop("Los valores por sexo no coinciden con la visualización entregada.")
}

province_checks <- tibble::tribble(
  ~provincia, ~empleo_adecuado_pct,
  "Pichincha", 48.2,
  "Galápagos", 45.1,
  "Azuay", 42.4,
  "Guayas", 39.0,
  "El Oro", 38.7,
  "Santo Domingo de los Tsáchilas", 33.7,
  "Chimborazo", 10.7,
  "Napo", 8.9,
  "Morona Santiago", 8.0
)

province_check <- resumen_provincia |>
  inner_join(province_checks, by = "provincia", suffix = c("_calculado", "_referencia"))

if (
  nrow(province_check) != nrow(province_checks) ||
    any(abs(province_check$empleo_adecuado_pct_calculado - province_check$empleo_adecuado_pct_referencia) > 0.1)
) {
  stop("Los valores provinciales no coinciden con la visualización entregada.")
}

# 5. Export ----
chart_data <- list(
  sexo = resumen_sexo,
  provincias = resumen_provincia,
  metadata = list(
    age_range = c(18L, 29L),
    year = 2025L,
    source = "INEC, ENEMDU anual 2025",
    source_url = paste0(
      "https://www.ecuadorencifras.gob.ec/documentos/web-inec/",
      "EMPLEO/2025/anual/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip"
    ),
    input_member = input_member,
    denominator = "PEA joven: personas de 18 a 29 años ocupadas o desempleadas",
    weighting = "Factor de expansión fexp",
    ecudata_mcp_note = paste(
      "EcuDataMCP confirmó la documentación ANDA de la ENEMDU, pero su",
      "catálogo aún no expone la edición anual 2025; se usó el ZIP oficial",
      "2025 ya conservado en data/raw/enemdu."
    )
  )
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(chart_data, out_path)
message("Guardado: ", out_path)
invisible(sessionInfo())
