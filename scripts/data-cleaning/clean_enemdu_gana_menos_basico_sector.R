# ============================================================
# clean_enemdu_gana_menos_basico_sector.R
# Porcentaje de personas que trabajan y ganan menos del salario básico ($482),
# por sector, con y sin empleo formal, enero a mayo de 2026.
# Requiere: data/raw/enemdu/enemdu_persona_2026_l_trimestre.sav (enero a marzo)
#           data/raw/enemdu/enemdu_persona_2026_04.sav
#           data/raw/enemdu/enemdu_persona_2026_05.sav
# Guarda:   data/processed/enemdu_gana_menos_basico_sector.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_gana_menos_basico_sector.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven", "tibble"))

out_path <- "data/processed/enemdu_gana_menos_basico_sector.rds"
sbu_2026 <- 482
columnas <- c("secemp", "ingrl", "fexp", "rama1")

primer_trimestre <- haven::read_sav(
  "data/raw/enemdu/enemdu_persona_2026_l_trimestre.sav",
  encoding = "latin1",
  col_select = c(dplyr::all_of(columnas), mes)
) |>
  haven::zap_labels() |>
  dplyr::mutate(mes = as.character(mes))

abril <- haven::read_sav(
  "data/raw/enemdu/enemdu_persona_2026_04.sav",
  col_select = dplyr::all_of(columnas)
) |>
  haven::zap_labels() |>
  dplyr::mutate(mes = "04")

mayo <- haven::read_sav(
  "data/raw/enemdu/enemdu_persona_2026_05.sav",
  col_select = dplyr::all_of(columnas)
) |>
  haven::zap_labels() |>
  dplyr::mutate(mes = "05")

# Cada mes pesa lo mismo: el factor de cada persona se divide para la suma del
# factor de su mes.
enemdu <- dplyr::bind_rows(primer_trimestre, abril, mayo) |>
  dplyr::mutate(
    dplyr::across(c(secemp, ingrl, fexp, rama1), as.numeric),
    peso = fexp / sum(fexp, na.rm = TRUE),
    .by = mes
  )

# Personas que trabajan (secemp 1 a 4) con ingreso laboral conocido. 999999 es
# "no informa". rama1 va de 1 (sección A de la CIIU) a 21 (U).
ocupados <- enemdu |>
  dplyr::filter(
    secemp %in% 1:4,
    !is.na(ingrl),
    ingrl < 999999,
    fexp > 0,
    rama1 %in% 1:21
  ) |>
  dplyr::mutate(seccion = LETTERS[rama1])

# Los 13 sectores del Banco Central (M con N, R con S y D con E).
sectores <- tibble::tribble(
  ~seccion, ~sector,
  "A", "Agricultura y pesca",
  "C", "Manufactura",
  "D", "Electricidad y agua",
  "E", "Electricidad y agua",
  "F", "Construcción",
  "G", "Comercio",
  "H", "Transporte",
  "I", "Alojamiento y comidas",
  "J", "Información y comunicación",
  "K", "Actividades financieras",
  "M", "Profesionales y administrativos",
  "N", "Profesionales y administrativos",
  "P", "Enseñanza",
  "Q", "Salud",
  "R", "Arte y otros servicios",
  "S", "Arte y otros servicios"
)

gana_menos <- ocupados |>
  dplyr::inner_join(
    sectores,
    by = dplyr::join_by(seccion),
    relationship = "many-to-one"
  ) |>
  dplyr::summarise(
    con_empleo_formal = 100 * sum(peso[ingrl < sbu_2026 & secemp == 1], na.rm = TRUE) /
      sum(peso, na.rm = TRUE),
    sin_empleo_formal = 100 * sum(peso[ingrl < sbu_2026 & secemp != 1], na.rm = TRUE) /
      sum(peso, na.rm = TRUE),
    .by = sector
  ) |>
  dplyr::mutate(gana_menos_de_482 = con_empleo_formal + sin_empleo_formal) |>
  dplyr::arrange(dplyr::desc(gana_menos_de_482))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(gana_menos, out_path)
message("Guardado: ", out_path)
