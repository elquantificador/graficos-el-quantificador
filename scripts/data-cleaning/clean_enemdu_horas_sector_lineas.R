# ============================================================
# clean_enemdu_horas_sector_lineas.R
# Construye la serie 2018-2026 de horas promedio trabajadas
# por sexo y sector, usando cortes de diciembre para 2018-2025
# y el archivo agregado del I trimestre de 2026.
# Requiere: data/raw/enemdu/*.sav
# Guarda:   data/processed/enemdu_horas_sector_lineas_2018_2026.rds
# ============================================================
# Ejecutar desde la raiz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_horas_sector_lineas.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven", "purrr", "readr", "stringr", "tibble"))

raw_dir <- "data/raw/enemdu"
out_path <- "data/processed/enemdu_horas_sector_lineas_2018_2026.rds"

annual_specs <- tibble::tribble(
  ~anio, ~file_name, ~p05a_informal, ~p05b_informal,
  2018, "ENEMDU_PERSONAS_2018_12_hom.sav", 7, 6,
  2019, "enemdu_persona_201912.sav", 7, 7,
  2020, "enemdu_persona_2020_12.sav", 8, 5,
  2021, "enemdu_persona_2021_12.sav", 9, 6,
  2022, "enemdu_persona_2022_12.sav", 7, 6,
  2023, "enemdu_persona_2023_12.sav", 7, 5,
  2024, "enemdu_persona_2024_12.sav", 7, 5,
  2025, "enemdu_persona_2025_12.sav", 7, 5
) |>
  dplyr::mutate(path = file.path(raw_dir, file_name))

missing_annual <- annual_specs |>
  dplyr::filter(!file.exists(path))

if (nrow(missing_annual) > 0) {
  stop(
    "Faltan archivos ENEMDU para la serie anual: ",
    paste(missing_annual$file_name, collapse = ", ")
  )
}

q1_2026_spec <- tibble::tibble(
  path = file.path(raw_dir, "enemdu_persona_2026_l_trimestre.sav"),
  file_name = "enemdu_persona_2026_l_trimestre.sav",
  anio = 2026L,
  p05a_informal = 7,
  p05b_informal = 5,
  mes = NA_integer_
)

if (!file.exists(q1_2026_spec$path[[1]])) {
  stop("Falta el archivo ENEMDU del I trimestre de 2026: ", q1_2026_spec$file_name[[1]])
}

summarise_hours <- function(path, anio, p05a_informal, p05b_informal, mes = NA_integer_) {
  raw_df <- haven::read_sav(path)

  raw_df |>
    dplyr::mutate(
      secemp_new = dplyr::case_when(
        !is.na(secemp) ~ as.numeric(secemp),
        is.na(secemp) & p05a == p05a_informal & p05b == p05b_informal ~ 2,
        is.na(secemp) ~ 1
      ),
      sexo = dplyr::case_when(
        p02 == 1 ~ "Hombres",
        p02 == 2 ~ "Mujeres",
        TRUE ~ NA_character_
      ),
      sector_desc = dplyr::case_when(
        secemp_new == 1 ~ "Sector Formal",
        secemp_new == 2 ~ "Sector Informal",
        TRUE ~ NA_character_
      ),
      horas = as.numeric(p24)
    ) |>
    dplyr::filter(
      !is.na(sexo),
      !is.na(sector_desc),
      p03 >= 15,
      !is.na(horas)
    ) |>
    dplyr::group_by(sexo, sector_desc) |>
    dplyr::summarise(
      horas_promedio = mean(horas, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      anio = anio,
      mes = mes,
      fuente_archivo = basename(path)
    ) |>
    dplyr::select(anio, mes, sexo, sector_desc, horas_promedio, fuente_archivo)
}

annual_snapshot_df <- purrr::pmap_dfr(
  annual_specs[, c("path", "anio", "p05a_informal", "p05b_informal")],
  summarise_hours
)

quarter_2026_df <- purrr::pmap_dfr(
  q1_2026_spec[, c("path", "anio", "p05a_informal", "p05b_informal", "mes")],
  summarise_hours
)

annual_2026_df <- quarter_2026_df |>
  dplyr::mutate(
    anio = 2026L,
    mes = NA_integer_,
    fuente_archivo = q1_2026_spec$file_name[[1]]
  ) |>
  dplyr::select(anio, mes, sexo, sector_desc, horas_promedio, fuente_archivo)

annual_series_df <- dplyr::bind_rows(
  annual_snapshot_df,
  annual_2026_df
) |>
  dplyr::mutate(
    sexo = factor(sexo, levels = c("Hombres", "Mujeres")),
    sector_desc = factor(sector_desc, levels = c("Sector Formal", "Sector Informal"))
  ) |>
  dplyr::arrange(sector_desc, sexo, anio)

chart_data <- list(
  annual_series = annual_series_df,
  quarter_2026 = quarter_2026_df |>
    dplyr::mutate(
      sexo = factor(sexo, levels = c("Hombres", "Mujeres")),
      sector_desc = factor(sector_desc, levels = c("Sector Formal", "Sector Informal"))
    ) |>
    dplyr::arrange(sector_desc, sexo),
  metadata = list(
    annual_snapshot_files = annual_specs$file_name,
    quarter_2026_file = q1_2026_spec$file_name[[1]],
    methodology = paste(
      "Serie 2018-2025 calculada con microdatos ENEMDU de diciembre.",
      "Para 2026 se usa el archivo de personas del I trimestre 2026."
    )
  )
)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(chart_data, out_path)
message("Guardado: ", out_path)
