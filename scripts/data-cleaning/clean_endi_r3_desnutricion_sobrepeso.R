# ============================================================
# Prevalencia de desnutrición crónica y sobrepeso, ENDI R1 a R3
# Entradas: tabulados oficiales de las tres rondas
# Salida: data/processed/endi_r3_desnutricion_sobrepeso.rds
# Ejecutar desde la raíz: Rscript scripts/data-cleaning/clean_endi_r3_desnutricion_sobrepeso.R
# ============================================================

# 0. Preparación ----

source("scripts/packages.R")
ensure_packages(c("dplyr", "readxl", "tidyr"))

# 1. Leer tabulados ----

rondas <- data.frame(
  ronda = c("R1", "R2", "R3"),
  periodo = c("2022-2023", "2023-2024", "2025-2026"),
  inicio = as.Date(c("2022-07-01", "2023-07-01", "2025-08-01")),
  fin = as.Date(c("2023-07-31", "2024-08-31", "2026-09-30")),
  archivo = paste0("data/raw/endi_r3/Tabulados_ENDI_", c("R1", "R2", "R3"), ".xlsx")
)

indicadores <- data.frame(
  hoja = c("T2_i1", "T2_i7", "T2_i4", "T2_i12"),
  indicador = c("dci", "dci", "dci", "sobrepeso"),
  grupo_edad = c("menores_2", "2_a_5", "menores_5", "menores_5")
)

raw <- dplyr::bind_rows(lapply(seq_len(nrow(rondas)), function(i) {
  dplyr::bind_rows(lapply(seq_len(nrow(indicadores)), function(j) {
    readxl::read_excel(rondas$archivo[i], sheet = indicadores$hoja[j],
                       range = "C9:G11", col_names = FALSE) |>
      stats::setNames(c("area", "valor_pct", "error_pct", "ic_inf_pct", "ic_sup_pct")) |>
      dplyr::mutate(ronda = rondas$ronda[i], periodo = rondas$periodo[i],
                    inicio = rondas$inicio[i], fin = rondas$fin[i],
                    indicador = indicadores$indicador[j],
                    grupo_edad = indicadores$grupo_edad[j])
  }))
})) |>
  dplyr::mutate(
    area = tolower(area),
    fecha_media = inicio + (fin - inicio) / 2,
    valor = valor_pct / 100
  )

# 2. Comparar rondas 2 y 3 con los errores estándar publicados ----

comparacion <- raw |>
  dplyr::filter(ronda %in% c("R2", "R3")) |>
  dplyr::select(indicador, grupo_edad, area, ronda, valor_pct, error_pct) |>
  tidyr::pivot_wider(names_from = ronda, values_from = c(valor_pct, error_pct)) |>
  dplyr::mutate(
    cambio_pp = valor_pct_R3 - valor_pct_R2,
    z = cambio_pp / sqrt(error_pct_R2^2 + error_pct_R3^2),
    sig = abs(z) > 1.96
  )

# 3. Preparar los datos de cada gráfico ----

grupo_labels <- c(menores_2 = "Menores de 2 años",
                  "2_a_5" = "De 2 a menores de 5 años",
                  menores_5 = "Menores de 5 años")

dci_edad <- raw |>
  dplyr::filter(indicador == "dci", area == "nacional") |>
  dplyr::left_join(dplyr::select(comparacion, indicador, grupo_edad, area, sig),
                   by = c("indicador", "grupo_edad", "area")) |>
  dplyr::mutate(
    grupo = factor(grupo_edad, levels = names(grupo_labels)),
    grupo_label = unname(grupo_labels[grupo_edad]),
    sig_r2_r3 = dplyr::if_else(ronda == "R3", as.integer(sig), NA_integer_)
  ) |>
  dplyr::select(ronda, periodo, inicio, fin, fecha_media, grupo, grupo_label,
                valor, sig_r2_r3) |>
  dplyr::arrange(grupo, fecha_media)

doble_carga <- raw |>
  dplyr::filter(grupo_edad == "menores_5", ronda %in% c("R2", "R3")) |>
  dplyr::left_join(dplyr::select(comparacion, indicador, grupo_edad, area, cambio_pp, sig),
                   by = c("indicador", "grupo_edad", "area")) |>
  dplyr::mutate(
    area = factor(area, levels = c("nacional", "urbano", "rural"),
                  labels = c("Nacional", "Urbano", "Rural")),
    indicador = factor(indicador, levels = c("dci", "sobrepeso"),
                       labels = c("Desnutrición crónica", "Sobrepeso y obesidad"))
  ) |>
  dplyr::select(indicador, area, ronda, valor, cambio_pp, sig)

# 4. Guardar ----

out_path <- "data/processed/endi_r3_desnutricion_sobrepeso.rds"
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(list(dci_edad = dci_edad, doble_carga = doble_carga), out_path)
message("Guardado: ", out_path)
