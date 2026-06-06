# ============================================================
# clean_enighur_ingreso_gasto.R
# Procesa los tabulados de la ENIGHUR para descomponer el
# ingreso monetario mensual del hogar promedio en gasto
# corriente (por categoría), gasto de no consumo y ahorro.
# Requiere: data/raw/enighur/cuadro_2_1_1_ingresos.rds
#           data/raw/enighur/cuadro_2_1_3_gastos.rds
#           data/raw/enighur/cuadro_2_2_1_promedios.rds
#           data/raw/enighur/mapeo_categorias_gasto.rds
# Guarda:   data/processed/enighur_ingreso_gasto.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enighur_ingreso_gasto.R
# ============================================================

options(scipen = 999)

source("scripts/packages.R")
ensure_packages(c("dplyr"))

suppressPackageStartupMessages(library(dplyr))

raw_dir  <- "data/raw/enighur"
out_path <- "data/processed/enighur_ingreso_gasto.rds"

normalize_label <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

get_nacional_total <- function(rows, pattern, exclude = NULL) {
  matches <- grepl(pattern, rows$label, ignore.case = TRUE)
  if (!is.null(exclude)) {
    matches <- matches & !grepl(exclude, rows$label, ignore.case = TRUE)
  }
  hit <- rows[matches & !is.na(rows$value), , drop = FALSE]
  if (nrow(hit) == 0) {
    stop("No se encontro una fila que coincida con: ", pattern, call. = FALSE)
  }
  hit$value[[1]]
}

get_category_rows <- function(rows) {
  rows |>
    mutate(
      nombre_enighur = .data$label,
      nombre_norm    = normalize_label(.data$label)
    ) |>
    filter(!is.na(.data$value))
}

match_gasto_row <- function(map_name_norm, gas_data) {
  exact_idx <- which(gas_data$nombre_norm == map_name_norm)
  if (length(exact_idx) > 0) {
    return(gas_data[exact_idx[[1]], , drop = FALSE])
  }
  map_tokens <- strsplit(map_name_norm, " +")[[1]]
  subset_idx <- which(vapply(
    gas_data$nombre_norm,
    function(lbl) all(map_tokens %in% strsplit(lbl, " +")[[1]]),
    logical(1)
  ))
  if (length(subset_idx) > 0) {
    return(gas_data[subset_idx[[1]], , drop = FALSE])
  }
  gas_data[0, , drop = FALSE]
}

ing_rows  <- readRDS(file.path(raw_dir, "cuadro_2_1_1_ingresos.rds"))
gas_rows  <- readRDS(file.path(raw_dir, "cuadro_2_1_3_gastos.rds"))
avg_sheet <- readRDS(file.path(raw_dir, "cuadro_2_2_1_promedios.rds"))
gasto_map <- readRDS(file.path(raw_dir, "mapeo_categorias_gasto.rds"))

ing_cor_tot_exp <- get_nacional_total(ing_rows, "Ingreso corriente total del hogar")
ing_mon_cor_exp <- get_nacional_total(ing_rows, "Ingreso corriente monetario del hogar")
gas_corr_exp    <- get_nacional_total(gas_rows, "Gasto corriente de consumo")
gas_no_con_exp  <- get_nacional_total(gas_rows, "Gasto de no consumo")

nac_row <- which(avg_sheet[[1]] == "Nacional" & is.na(avg_sheet[[2]]))
if (length(nac_row) == 0) {
  stop("No se encontro la fila Nacional en CUADRO 2.2.1.", call. = FALSE)
}
avg_ing_cor_tot <- as.numeric(avg_sheet[[3]][nac_row[[1]]])

avg_ing_mon_cor <- (ing_mon_cor_exp / ing_cor_tot_exp) * avg_ing_cor_tot
avg_gas_corr    <- (gas_corr_exp    / ing_cor_tot_exp) * avg_ing_cor_tot
avg_gas_no_con  <- (gas_no_con_exp  / ing_cor_tot_exp) * avg_ing_cor_tot
avg_ahorro      <- avg_ing_mon_cor - avg_gas_corr - avg_gas_no_con

if (avg_ahorro < 0) {
  stop("El ahorro calculado es negativo; los agregados no cierran.", call. = FALSE)
}

gas_categories <- get_category_rows(gas_rows)
map_rows <- gasto_map |>
  mutate(nombre_norm = normalize_label(.data$nombre_enighur))

matched_rows <- lapply(seq_len(nrow(map_rows)), function(i) {
  hit <- match_gasto_row(map_rows$nombre_norm[[i]], gas_categories)
  data.frame(
    categoria_codigo = map_rows$categoria_codigo[[i]],
    categoria_nombre = map_rows$categoria_nombre[[i]],
    orden            = map_rows$orden[[i]],
    valor            = if (nrow(hit) == 0) NA_real_ else hit$value[[1]],
    stringsAsFactors = FALSE
  )
})

category_rows <- bind_rows(matched_rows)

if (any(is.na(category_rows$valor))) {
  missing_names <- unique(map_rows$nombre_enighur[is.na(category_rows$valor)])
  stop("Faltan coincidencias de gasto para: ", paste(missing_names, collapse = ", "), call. = FALSE)
}

category_rows <- category_rows |>
  group_by(.data$categoria_codigo, .data$categoria_nombre, .data$orden) |>
  summarise(
    valor     = sum(.data$valor),
    valor_avg = (sum(.data$valor) / ing_cor_tot_exp) * avg_ing_cor_tot,
    .groups   = "drop"
  ) |>
  left_join(
    gasto_map |> distinct(.data$categoria_codigo, .data$color_hex),
    by = "categoria_codigo"
  ) |>
  arrange(.data$orden) |>
  mutate(
    label          = .data$categoria_nombre,
    share_ingreso  = .data$valor_avg / avg_ing_mon_cor
  )

if (abs(sum(category_rows$valor_avg) - avg_gas_corr) > 1e-6) {
  stop("Las categorias de gasto corriente no suman el total esperado.", call. = FALSE)
}

processed <- list(
  metadata = list(
    descripcion = "Descomposicion del ingreso monetario mensual del hogar promedio",
    moneda      = "USD mensuales promedio"
  ),
  resumen = list(
    avg_ing_cor_tot = avg_ing_cor_tot,
    avg_ing_mon_cor = avg_ing_mon_cor,
    avg_gas_corr    = avg_gas_corr,
    avg_gas_no_con  = avg_gas_no_con,
    avg_ahorro      = avg_ahorro
  ),
  category_rows = category_rows,
  gasto_map     = gasto_map
)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(processed, out_path)
message("Guardado: ", out_path)
