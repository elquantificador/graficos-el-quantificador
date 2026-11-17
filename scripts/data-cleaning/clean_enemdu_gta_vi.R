# ============================================================
# clean_enemdu_gta_vi.R
# Estima la mediana provincial del ingreso laboral por hora.
# Requiere: data/raw/gta_vi/enemdu_2025_anual.zip
# Guarda: data/processed/enemdu_gta_vi.rds
# ============================================================
source('scripts/packages.R')
ensure_packages(c('dplyr', 'readr', 'Hmisc'))

input_path <- 'data/raw/gta_vi/enemdu_2025_anual.zip'
output_path <- 'data/processed/enemdu_gta_vi.rds'
price <- 79.99 * 1.15
people <- readr::read_delim(
  unz(input_path, 'BDDenemdu_personas_2025_anual.csv'),
  delim = ';', na = c('', ' ', 'NA'),
  locale = readr::locale(decimal_mark = ','),
  col_select = c(prov, p03, p24, ingrl, empleo, fexp),
  col_types = readr::cols(.default = readr::col_double()),
  show_col_types = FALSE
)
people <- people |>
  dplyr::filter(prov %in% 1:24, p03 >= 15, empleo == 1, p24 > 0,
                p24 <= 168, ingrl > 0, ingrl < 999999, fexp > 0) |>
  dplyr::mutate(hourly = ingrl / (p24 * 52 / 12))
province <- people |>
  dplyr::group_by(prov) |>
  dplyr::summarise(
    n = dplyr::n(),
    median_hourly = as.numeric(Hmisc::wtd.quantile(hourly, weights = fexp, probs = 0.5)),
    .groups = 'drop'
  ) |>
  dplyr::mutate(hours = price / median_hourly)
national_median <- as.numeric(Hmisc::wtd.quantile(people$hourly, weights = people$fexp, probs = 0.5))
attr(province, 'national_hours') <- floor(price / national_median)
dir.create('data/processed', recursive = TRUE, showWarnings = FALSE)
saveRDS(province, output_path)
message('Guardado: ', output_path)
