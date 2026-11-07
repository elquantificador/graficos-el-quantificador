# ============================================================
# clean_endi_r3_desnutricion_sobrepeso.R
# Prepara las prevalencias de desnutrición crónica y sobrepeso
# de la ENDI, rondas 1 a 3, publicadas en el boletín técnico R3.
# Requiere: data/raw/endi_r3/endi_r3_boletin_indicadores.csv
# Guarda:   data/processed/endi_r3_desnutricion_sobrepeso.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_endi_r3_desnutricion_sobrepeso.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr", "tidyr"))

raw_path <- "data/raw/endi_r3/endi_r3_boletin_indicadores.csv"
out_path <- "data/processed/endi_r3_desnutricion_sobrepeso.rds"

raw <- readr::read_csv(raw_path, show_col_types = FALSE) |>
  dplyr::mutate(
    inicio = as.Date(inicio),
    fin = as.Date(fin),
    fecha_media = inicio + (fin - inicio) / 2,
    valor = valor_pct / 100
  )

# Controles de consistencia con el boletín ----
stopifnot(
  nrow(raw) == 36,
  !any(duplicated(raw[c("indicador", "grupo_edad", "area", "ronda")])),
  all(raw$valor_pct > 0 & raw$valor_pct < 100)
)

# Gráfico A: desnutrición crónica nacional por grupo de edad, R1 a R3 ----
grupo_labels <- c(
  menores_2 = "Menores de 2 años",
  "2_a_5"   = "De 2 a menores de 5 años",
  menores_5 = "Menores de 5 años"
)

dci_edad <- raw |>
  dplyr::filter(indicador == "dci", area == "nacional") |>
  dplyr::mutate(
    grupo = factor(grupo_edad, levels = names(grupo_labels)),
    grupo_label = unname(grupo_labels[grupo_edad])
  ) |>
  dplyr::select(ronda, periodo, inicio, fin, fecha_media, grupo, grupo_label,
                valor, sig_r2_r3) |>
  dplyr::arrange(grupo, fecha_media)

# Gráfico B: desnutrición crónica y sobrepeso en menores de 5, R2 y R3 ----
area_labels <- c(nacional = "Nacional", urbano = "Urbano", rural = "Rural")
indicador_labels <- c(dci = "Desnutrición crónica", sobrepeso = "Sobrepeso y obesidad")

doble_carga <- raw |>
  dplyr::filter(grupo_edad == "menores_5", ronda %in% c("R2", "R3")) |>
  dplyr::select(indicador, area, ronda, valor, sig_r2_r3) |>
  dplyr::group_by(indicador, area) |>
  dplyr::mutate(
    cambio_pp = (valor[ronda == "R3"] - valor[ronda == "R2"]) * 100,
    sig = max(sig_r2_r3, na.rm = TRUE) == 1
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    area = factor(area, levels = names(area_labels), labels = unname(area_labels)),
    indicador = factor(indicador, levels = names(indicador_labels),
                       labels = unname(indicador_labels))
  ) |>
  dplyr::select(-sig_r2_r3)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(list(dci_edad = dci_edad, doble_carga = doble_carga), out_path)
message("Guardado: ", out_path)
