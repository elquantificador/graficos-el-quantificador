# ============================================================
# clean_acled_conflicto_ecuador.R
# Limpia el CSV maestro de ACLED y recategoriza los eventos para el mapa.
# Requiere: data/raw/acled_conflicto_ecuador/acled_ecuador_maestro_20260903.csv
# Guarda:   data/processed/acled_conflicto_ecuador.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_acled_conflicto_ecuador.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr"))

raw_path <- "data/raw/acled_conflicto_ecuador/acled_ecuador_maestro_20260903.csv"
out_path <- "data/processed/acled_conflicto_ecuador.rds"

required_columns <- c(
  "event_id_cnty", "event_date", "year_month", "sub_event_type",
  "latitude", "longitude"
)

datos <- readr::read_csv(
  raw_path,
  locale = readr::locale(encoding = "UTF-8"),
  na = c("", "NA"),
  show_col_types = FALSE,
  progress = FALSE
)

missing_columns <- setdiff(required_columns, names(datos))
if (length(missing_columns) > 0) {
  stop("Faltan columnas requeridas: ", paste(missing_columns, collapse = ", "))
}

n_raw <- nrow(datos)

niveles_categoria <- c(
  "Control y Crimen Organizado",
  "Protesta Pac\u00EDfica",
  "Represi\u00F3n a Manifestantes",
  "Manifestaci\u00F3n Violenta",
  "Violencia de Turbas",
  "Enfrentamiento Armado",
  "Ataques Explosivos y con Drones",
  "Violencia Directa a Civiles"
)

niveles_categoria_id <- c(
  "control_crimen",
  "protesta_pacifica",
  "represion_manifestantes",
  "manifestacion_violenta",
  "violencia_turbas",
  "enfrentamiento_armado",
  "ataques_explosivos_drones",
  "violencia_directa_civiles"
)

datos_limpios <- datos %>%
  dplyr::distinct(event_id_cnty, .keep_all = TRUE) %>%
  dplyr::mutate(
    event_date = as.Date(event_date),
    year_month = as.Date(year_month),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  dplyr::filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(sub_event_type),
    sub_event_type != "",
    year_month >= as.Date("2018-01-01"),
    year_month <= as.Date("2025-12-01")
  ) %>%
  dplyr::mutate(
    concurso = dplyr::case_when(
      sub_event_type %in% c(
        "Arrestos", "Otros", "Reorganizaci\u00F3n/Alianza Criminal",
        "Saqueo y Da\u00F1o a la Propiedad", "Uso de Armas Interrumpido"
      ) ~ "Control y Crimen Organizado",
      sub_event_type == "Manifestaci\u00F3n Violenta" ~ "Manifestaci\u00F3n Violenta",
      sub_event_type %in% c(
        "Protesta Controlada/Intervenida", "Protesta Pac\u00EDfica"
      ) ~ "Protesta Pac\u00EDfica",
      sub_event_type == "Enfrentamiento Armado/Tiroteo" ~ "Enfrentamiento Armado",
      sub_event_type == "Violencia de Turbas" ~ "Violencia de Turbas",
      sub_event_type %in% c(
        "Ataque A\u00E9reo/Dron", "Uso de Explosivos/Armas Remotas"
      ) ~ "Ataques Explosivos y con Drones",
      sub_event_type == "Fuerza Excesiva Contra Manifestantes" ~ "Represi\u00F3n a Manifestantes",
      sub_event_type %in% c(
        "Ataque", "Secuestro y Desaparici\u00F3n Forzada", "Violencia Sexual"
      ) ~ "Violencia Directa a Civiles",
      TRUE ~ NA_character_
    ),
    concurso = factor(concurso, levels = niveles_categoria, ordered = TRUE),
    categoria_id = niveles_categoria_id[match(as.character(concurso), niveles_categoria)]
  ) %>%
  dplyr::filter(!is.na(concurso)) %>%
  dplyr::arrange(event_date, event_id_cnty)

if (nrow(datos_limpios) == 0) {
  stop("El filtro de eventos no produjo registros.")
}

metadata <- list(
  source_file = raw_path,
  source_description = "CSV maestro entregado por Mayari Tapia por correo el 2026-09-03",
  source_script = "https://huggingface.co/spaces/Seth77/Conflicto/blob/main/datos/2.limpieza%20y%20descarga_acled_ecuador.R",
  methodology = "Metodología_Conflicto_Ecuador--1-.pdf del replication package",
  n_raw = n_raw,
  n_clean = nrow(datos_limpios),
  first_event = min(datos_limpios$event_date, na.rm = TRUE),
  last_event = max(datos_limpios$event_date, na.rm = TRUE),
  categories = levels(datos_limpios$concurso),
  category_counts = datos_limpios %>% dplyr::count(concurso, name = "events")
)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(list(data = datos_limpios, metadata = metadata), out_path)
message("Guardado: ", out_path)
message("Eventos crudos: ", n_raw, " | eventos usados: ", nrow(datos_limpios))
message("Cobertura del archivo: ", format(metadata$first_event), " a ", format(metadata$last_event))
