# ============================================================
# clean_enemdu_empleo_adecuado_edad.R
# Carga los tabulados de la ENEMDU y calcula la variación
# interanual (YoY) del nivel de empleo adecuado por grupo de
# edad, a frecuencia mensual.
# Requiere: data/raw/enemdu/202603_Tabulados_Mercado_Laboral_EXCEL (2).xlsx
# Guarda:   data/processed/enemdu_empleo_adecuado_edad_yoy.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_empleo_adecuado_edad.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("readxl", "dplyr", "tidyr"))

input_path <- "data/raw/enemdu/202603_Tabulados_Mercado_Laboral_EXCEL (2).xlsx"
out_path   <- "data/processed/enemdu_empleo_adecuado_edad_yoy.rds"

# ---- Utilidad para parsear periodos tipo "mar-26" --------------------------

parse_enemdu_period <- function(x) {
  x <- tolower(trimws(as.character(x)))
  month_map <- c(
    ene = 1, feb = 2, mar = 3, abr = 4, may = 5, jun = 6,
    jul = 7, ago = 8, sep = 9, oct = 10, nov = 11, dic = 12
  )

  parts <- regmatches(x, regexec("^([a-z]+)-([0-9]{2,4})$", x))[[1]]
  if (length(parts) < 3 || is.null(month_map[[parts[2]]])) {
    return(as.Date(NA))
  }

  year <- as.integer(parts[3])
  if (year < 100) {
    year <- 2000 + year
  }

  as.Date(sprintf("%04d-%02d-01", year, month_map[[parts[2]]]))
}

# ---- 1. Nivel total de empleo adecuado ------------------------------------

pop_df <- readxl::read_xlsx(
  input_path,
  sheet = "1. Poblaciones",
  range = "A2:D2000",
  col_names = c("encuesta", "periodo", "indicador", "total")
) %>%
  filter(indicador == "Empleo Adecuado/Pleno") %>%
  transmute(
    periodo,
    fecha = as.Date(vapply(periodo, parse_enemdu_period, as.Date(NA))),
    total_nivel = as.numeric(total)
  ) %>%
  filter(fecha >= as.Date("2024-01-01"), fecha <= as.Date("2026-03-01"))

# ---- 2. Participación por grupo de edad -----------------------------------

char_df <- readxl::read_xlsx(
  input_path,
  sheet = "3.2 Caracterización Adec_pleno",
  range = "A2:DA13",
  col_names = FALSE
)

periods <- as.character(unlist(char_df[1, 3:ncol(char_df)]))
fechas <- as.Date(vapply(periods, parse_enemdu_period, as.Date(NA)))
keep <- fechas >= as.Date("2024-01-01") & fechas <= as.Date("2026-03-01")

shares_df <- tibble::tibble(
  periodo = periods[keep],
  fecha = fechas[keep],
  `Todas las edades` = 1,
  `15-24` = as.numeric(unlist(char_df[8, 3:ncol(char_df)]))[keep],
  `25-44` =
    as.numeric(unlist(char_df[9, 3:ncol(char_df)]))[keep] +
    as.numeric(unlist(char_df[10, 3:ncol(char_df)]))[keep],
  `45-64` = as.numeric(unlist(char_df[11, 3:ncol(char_df)]))[keep]
) %>%
  tidyr::pivot_longer(
    cols = -c(periodo, fecha),
    names_to = "grupo_edad",
    values_to = "share"
  )

# ---- 3. Calcular nivel y variación interanual -----------------------------

plot_df <- shares_df %>%
  left_join(pop_df, by = c("periodo", "fecha")) %>%
  mutate(nivel = share * total_nivel) %>%
  group_by(grupo_edad) %>%
  arrange(fecha, .by_group = TRUE) %>%
  mutate(yoy_pct = 100 * (nivel / lag(nivel, 12) - 1)) %>%
  ungroup() %>%
  filter(fecha >= as.Date("2025-01-01"), fecha <= as.Date("2026-03-01"))

# ---- 4. Guardar -----------------------------------------------------------

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(plot_df, out_path)
message("Guardado: ", out_path)
