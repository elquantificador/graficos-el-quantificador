# ============================================================
# plot_ras_personal_salud_provincial.R
# Compara la disponibilidad provincial de personal del MSP.
#
# Entradas:
#   data/raw/ras/msp_serie_*.rds
#   data/raw/inec/estimaciones_poblacion_provincial_2021.csv
#
# Salida:
#   outputs/figures/38_personal-salud_provincia-ecuador.png
# ============================================================

# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_ras_personal_salud_provincial.R

# set.seed(42) # El script no contiene procesos aleatorios.


# 1. Configuración ---------------------------------------------------------

source("scripts/utils.R")
source("scripts/packages.R")

ensure_packages(c(
  "dplyr",
  "ggplot2",
  "ragg",
  "readr",
  "scales"
))

raw_dir <- "data/raw/ras"
population_path <- "data/raw/inec/estimaciones_poblacion_provincial_2021.csv"
out_path <- "outputs/figures/38_personal-salud_provincia-ecuador.png"

occupation_cols <- c("tmedicos", "tenf", "tobst", "ttaps")
blue <- "#00A8CB"
target_year <- 2021


# 2. Lectura de datos ------------------------------------------------------

ras <- setNames(
  lapply(
    c("nac", "prov", "cant", "parr", "area"),
    function(level) {
      readRDS(file.path(raw_dir, paste0("msp_serie_", level, ".rds")))
    }
  ),
  c("nac", "prov", "cant", "parr", "area")
)

population <- readr::read_csv(
  population_path,
  col_types = readr::cols(
    prov_cod = readr::col_character(),
    prov_nom = readr::col_character(),
    poblacion_2021 = readr::col_double()
  ),
  show_col_types = FALSE
)


# 3. Validaciones ----------------------------------------------------------

sum_occupations <- function(data, year) {
  data |>
    dplyr::filter(.data$anio == year) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(occupation_cols),
        ~ sum(.x, na.rm = TRUE)
      )
    )
}

available_years <- sort(unique(ras$nac$anio))
if (!target_year %in% available_years) {
  stop("El RAS no contiene el año objetivo: ", target_year, ".")
}

reference <- sum_occupations(ras$nac, target_year)

for (level in c("prov", "cant", "parr", "area")) {
  candidate <- sum_occupations(ras[[level]], target_year)
  difference <- abs(as.numeric(candidate) - as.numeric(reference))

  if (any(difference > 1e-8)) {
    stop(
      "La suma de ", level,
      " no coincide con la serie nacional en ", target_year, "."
    )
  }
}

province_ras <- ras$prov |>
  dplyr::filter(.data$anio == target_year) |>
  dplyr::mutate(prov_cod = sprintf("%02d", as.integer(.data$prov_cod)))

if (nrow(province_ras) != 24L || nrow(population) != 24L) {
  stop("Se esperaban 24 provincias en ambas fuentes.")
}

missing_population <- dplyr::anti_join(
  province_ras,
  population,
  by = dplyr::join_by(prov_cod)
)

missing_ras <- dplyr::anti_join(
  population,
  province_ras,
  by = dplyr::join_by(prov_cod)
)

if (nrow(missing_population) > 0L || nrow(missing_ras) > 0L) {
  stop("Los códigos provinciales del RAS y del INEC no coinciden.")
}

if (anyNA(population$poblacion_2021) || any(population$poblacion_2021 <= 0)) {
  stop("La población provincial contiene valores faltantes o no positivos.")
}


# 4. Indicadores -----------------------------------------------------------

plot_df <- province_ras |>
  dplyr::select(
    prov_cod,
    prov_nom_ras = prov_nom,
    dplyr::all_of(occupation_cols)
  ) |>
  dplyr::left_join(
    population,
    by = dplyr::join_by(prov_cod),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    personal = rowSums(dplyr::pick(dplyr::all_of(occupation_cols))),
    personal_10k = .data$personal / .data$poblacion_2021 * 10000
  ) |>
  dplyr::arrange(dplyr::desc(.data$personal_10k))

if (anyNA(plot_df$personal_10k)) {
  stop("Los indicadores calculados contienen valores faltantes.")
}

plot_df <- plot_df |>
  dplyr::mutate(
    prov_nom = dplyr::if_else(
      .data$prov_cod == "23",
      "Santo Domingo",
      .data$prov_nom
    )
  )

province_levels <- rev(plot_df$prov_nom)
plot_df <- plot_df |>
  dplyr::mutate(
    prov_nom = factor(.data$prov_nom, levels = province_levels)
  )

title_raw <- paste0(
  "Guayas tenía 59 miembros del personal médico por cada 10 mil habitantes, ",
  "mientras que Pichincha solo 19"
)

subtitle_raw <- "Personal del MSP por cada 10.000 habitantes, por provincia, 2021"

caption_raw <- paste0(
  "Fuentes: INEC, RAS y Estimaciones y Proyecciones de Población, Revisión 2024. ",
  "Elaboración: Odalis Clemente y Alonso Quijano Ruiz para El Quantificador de Laboratorio LIDE. ",
  "Nota: personal incluye médicos, enfermeros, obstetrices y TAPS. Las cifras son integrantes ",
  "por cada 10.000 habitantes. TAPS: Técnicos de Atención Primaria en Salud."
)


# 5. Visualización ---------------------------------------------------------

chart <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = .data$personal_10k, y = .data$prov_nom)
) +
  ggplot2::geom_col(width = 0.62, fill = blue) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = label_number_intl(accuracy = 0.1)(.data$personal_10k)
    ),
    hjust = -0.18,
    size = 2.35,
    colour = "grey20"
  ) +
  ggplot2::scale_x_continuous(
    labels = label_number_intl(accuracy = 10),
    expand = ggplot2::expansion(mult = c(0, 0.14))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw, width = 100),
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 7.2),
    plot.subtitle = ggplot2::element_text(size = 8),
    plot.margin = ggplot2::margin(10, 42, 6, 16)
  )


# 6. Exportación -----------------------------------------------------------

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")

ggplot2::ggsave(
  filename = out_path,
  plot = house_apply_logo(
    chart,
    "portrait",
    x = 0.89,
    y = 0.085,
    width = 0.065,
    height = 0.065
  ),
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)


# 7. Información de sesión -------------------------------------------------

sessionInfo()
