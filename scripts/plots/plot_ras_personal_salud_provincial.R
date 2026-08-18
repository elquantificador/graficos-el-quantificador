# ============================================================
# plot_ras_personal_salud_provincial.R
# Compara la disponibilidad provincial de personal del MSP y
# las atenciones registradas por integrante del grupo analizado.
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
  "cowplot",
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
orange <- "#F0A145"
light_grey <- "#D9E0E3"
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
    dplyr::all_of(occupation_cols),
    taten
  ) |>
  dplyr::left_join(
    population,
    by = dplyr::join_by(prov_cod),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    personal = rowSums(dplyr::pick(dplyr::all_of(occupation_cols))),
    personal_10k = .data$personal / .data$poblacion_2021 * 10000,
    atenciones_personal = .data$taten / .data$personal
  ) |>
  dplyr::arrange(dplyr::desc(.data$personal_10k))

if (anyNA(plot_df$personal_10k) || anyNA(plot_df$atenciones_personal)) {
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

availability_ratio <- max(plot_df$personal_10k) / min(plot_df$personal_10k)
rank_correlation <- stats::cor(
  plot_df$personal_10k,
  plot_df$atenciones_personal,
  method = "spearman"
)
highest_pressure <- plot_df |>
  dplyr::slice_max(.data$atenciones_personal, n = 1, with_ties = FALSE)

title_raw <- "Menos personal suele coincidir con más atenciones por integrante"

subtitle_raw <- paste0(
  "La disponibilidad de personal del MSP varía ",
  label_number_intl(accuracy = 0.1)(availability_ratio),
  " veces entre provincias, ", target_year
)

caption_raw <- paste0(
  "Fuentes: INEC, RAS y Estimaciones y Proyecciones de Población, Revisión 2024. ",
  "Elaboración: Odalis Clemente y Alonso Quijano Ruiz para El Quantificador de Laboratorio LIDE. ",
  "Nota: personal incluye médicos, enfermeros, obstetrices y TAPS. La razón de atenciones es descriptiva ",
  "y no mide productividad individual. TAPS: Técnicos de Atención Primaria en Salud."
)


# 5. Visualización ---------------------------------------------------------

staff_panel <- ggplot2::ggplot(
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
    breaks = NULL,
    expand = ggplot2::expansion(mult = c(0, 0.32))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    title = "Personal por 10.000 habitantes"
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 6.8),
    axis.line.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(
      size = 7.8,
      face = "bold",
      colour = blue
    ),
    plot.margin = ggplot2::margin(4, 12, 4, 16)
  )

attention_panel <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(y = .data$prov_nom)
) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = 0,
      xend = .data$atenciones_personal,
      yend = .data$prov_nom
    ),
    linewidth = 0.55,
    colour = light_grey
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = .data$atenciones_personal),
    size = 2.1,
    colour = orange
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      x = .data$atenciones_personal,
      label = label_number_intl(accuracy = 1)(.data$atenciones_personal)
    ),
    hjust = -0.28,
    size = 2.25,
    colour = "grey20"
  ) +
  ggplot2::scale_x_continuous(
    breaks = NULL,
    expand = ggplot2::expansion(mult = c(0, 0.36))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    title = "Atenciones por integrante"
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(
      size = 7.8,
      face = "bold",
      colour = orange
    ),
    plot.margin = ggplot2::margin(4, 36, 4, 0)
  )

panel_grid <- cowplot::plot_grid(
  staff_panel,
  attention_panel,
  nrow = 1,
  rel_widths = c(1.55, 1),
  align = "h",
  axis = "tb"
)

combined_chart <- cowplot::ggdraw() +
  cowplot::draw_label(
    wrap_title_house(title_raw),
    x = 0.04,
    y = 0.975,
    hjust = 0,
    vjust = 1,
    size = HOUSE_TITLE_SIZE_PT,
    fontface = "bold",
    colour = "grey20",
    lineheight = 0.95
  ) +
  cowplot::draw_label(
    wrap_subtitle_house(subtitle_raw),
    x = 0.04,
    y = 0.875,
    hjust = 0,
    vjust = 1,
    size = HOUSE_SUBTITLE_SIZE_PT,
    colour = "grey30",
    lineheight = 1.05
  ) +
  cowplot::draw_plot(
    panel_grid,
    x = 0,
    y = 0.21,
    width = 1,
    height = 0.54
  ) +
  cowplot::draw_label(
    wrap_caption_house(caption_raw),
    x = 0.04,
    y = 0.175,
    hjust = 0,
    vjust = 1,
    size = HOUSE_CAPTION_SIZE_PT,
    colour = "grey30",
    lineheight = 1.05
  )


# 6. Exportación -----------------------------------------------------------

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")

ggplot2::ggsave(
  filename = out_path,
  plot = house_apply_logo(
    combined_chart,
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
message(
  "Mayor razón de atenciones: ",
  as.character(highest_pressure$prov_nom),
  " (",
  label_number_intl(accuracy = 1)(highest_pressure$atenciones_personal),
  ")."
)
message(
  "Correlación de rangos entre disponibilidad y atenciones: ",
  label_number_intl(accuracy = 0.01)(rank_correlation),
  "."
)


# 7. Información de sesión -------------------------------------------------

sessionInfo()
