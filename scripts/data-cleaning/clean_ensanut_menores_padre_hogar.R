# ============================================================
# clean_ensanut_menores_padre_hogar.R
# Calcula el porcentaje de menores cuyo padre vive en el hogar
# por grupo de edad usando ENSANUT 2018.
# Requiere: data/raw/ensanut/1_BDD_ENS2018_f1_personas.dta.zip
# Guarda:   data/processed/ensanut_menores_padre_hogar.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_ensanut_menores_padre_hogar.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven"))

zip_path <- "data/raw/ensanut/1_BDD_ENS2018_f1_personas.dta.zip"
inner_path <- "1_BDD_ENS2018_f1_personas.dta"
out_path <- "data/processed/ensanut_menores_padre_hogar.rds"
tmp_dir <- tempdir()
tmp_dta <- file.path(tmp_dir, inner_path)

utils::unzip(zip_path, files = inner_path, exdir = tmp_dir, overwrite = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

personas <- haven::read_dta(
  tmp_dta,
  col_select = c(
    id_hogar,
    id_per,
    persona,
    sexo,
    edadanios,
    f1_s2_14,
    fexp
  )
)

resumen_menores_padre <- personas |>
  dplyr::transmute(
    id_hogar,
    id_per,
    persona,
    sexo = as.numeric(sexo),
    edad = edadanios,
    padre_vive_hogar_cod = as.numeric(f1_s2_14),
    fexp
  ) |>
  dplyr::filter(edad >= 1, edad <= 18) |>
  dplyr::mutate(
    grupo_edad = dplyr::case_when(
      edad >= 1 & edad <= 5 ~ "1-5",
      edad >= 6 & edad <= 10 ~ "6-10",
      edad >= 11 & edad <= 15 ~ "11-15",
      edad >= 16 & edad <= 18 ~ "16-18",
      TRUE ~ NA_character_
    ),
    grupo_edad = factor(grupo_edad, levels = c("1-5", "6-10", "11-15", "16-18")),
    padre_vive_hogar = dplyr::case_when(
      padre_vive_hogar_cod == 1 ~ TRUE,
      padre_vive_hogar_cod == 2 ~ FALSE,
      TRUE ~ NA
    )
  ) |>
  dplyr::filter(!is.na(grupo_edad), !is.na(padre_vive_hogar), !is.na(fexp)) |>
  dplyr::group_by(grupo_edad) |>
  dplyr::summarise(
    menores_muestra = dplyr::n(),
    menores_expandidos = sum(fexp, na.rm = TRUE),
    padre_si_muestra = sum(padre_vive_hogar, na.rm = TRUE),
    padre_si_expandidos = sum(dplyr::if_else(padre_vive_hogar, fexp, 0), na.rm = TRUE),
    pct_padre_muestra = padre_si_muestra / menores_muestra,
    pct_padre_expandido = padre_si_expandidos / menores_expandidos,
    .groups = "drop"
  ) |>
  dplyr::mutate(
    menores_expandidos = round(menores_expandidos),
    padre_si_expandidos = round(padre_si_expandidos),
    dplyr::across(
      dplyr::starts_with("pct_"),
      ~ round(.x * 100, 2)
    )
  )

saveRDS(resumen_menores_padre, out_path)
message("Guardado: ", out_path)
