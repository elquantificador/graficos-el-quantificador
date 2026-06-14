# ============================================================
# plot_norteamerica_mapa_placeholder.R
# Genera un mapa coropletico de ecuatorianos en
# Norteamerica.
# Requiere: datos embebidos en el script
# Guarda:   outputs/figures/22_mapa-valores_norteamerica.png
# ============================================================
# Ejecutar desde la raiz del proyecto:
#   Rscript scripts/plots/plot_norteamerica_mapa_placeholder.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c(
  "dplyr", "ggplot2", "sf", "rnaturalearth",
  "scales", "ragg", "tibble"
))

portrait_path <- "outputs/figures/22_mapa-valores_norteamerica.png"

values_df <- tibble::tribble(
  ~admin,                     ~country_label,            ~value,
  "Canada",                   "Canadá\n16.320",          16320,
  "United States of America", "Estados Unidos\n440.337", 440337,
  "Mexico",                   "México\n4.615",            4615
)

map_df <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  dplyr::filter(admin %in% values_df$admin) |>
  dplyr::left_join(values_df, by = "admin")

label_points <- suppressWarnings(
  map_df |>
    sf::st_transform(3857) |>
    sf::st_point_on_surface() |>
    sf::st_transform(sf::st_crs(map_df))
)

label_df <- cbind(
  sf::st_drop_geometry(label_points),
  sf::st_coordinates(label_points)
)

title_raw <- "Cerca de medio millón de ecuatorianos apoyan a la tricolor desde Norteamérica"
subtitle_raw <- paste(
  "Ecuatorianos viviendo en Norteamérica,",
  "por país, estimados censales"
)
caption_raw <- paste(
  "Fuentes: Statistics Canada, Census of Population 2021, tabla 98-10-0349-01;",
  "U.S. Census Bureau, tabla B05006 (2020); INEGI, Censos de Población y Vivienda 2020.",
  "Los datos de Estados Unidos y México corresponden al mismo año; Statistics Canada considera población en 2021.",
  "Se utilizan los censos más recientes al momento de elaboración.",
  "Base cartográfica: Natural Earth.",
  "Elaboración: Daniel Sánchez para el Quantificador de Laboratorio LIDE."
)

build_chart <- function() {
  label_size <- 2.55
  title_width <- 54
  caption_width <- 118
  subtitle_txt <- "Ecuatorianos viviendo en Norteamérica, por país, estimados censales"

  ggplot2::ggplot(map_df) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = value),
      colour = "white",
      linewidth = 0.5
    ) +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(x = X, y = Y, label = country_label),
      size = label_size,
      colour = "grey15",
      lineheight = 0.9,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("#e9f3fb", "#c9e1f2", "#8fc0df", "#4d94c2", "#1696b5"),
      values = scales::rescale(c(4615, 20000, 100000, 250000, 440337)),
      limits = c(4615, 440337),
      labels = label_number_intl(accuracy = 1),
      breaks = c(4615, 150000, 300000, 440337),
      na.value = "grey90"
    ) +
    ggplot2::coord_sf(
      xlim = c(-162, -58),
      ylim = c(14, 82),
      expand = FALSE,
      datum = NA
    ) +
    ggplot2::labs(
      title = wrap_title_house(title_raw, width = title_width),
      subtitle = subtitle_txt,
      caption = wrap_caption_house(caption_raw, width = caption_width),
      fill = NULL,
      x = NULL,
      y = NULL
    ) +
    theme_quantificador("portrait") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.position = c(0.075, 0.30),
      legend.justification = c(0, 0.5),
      legend.direction = "vertical",
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      legend.key.width = grid::unit(3.2, "mm"),
      legend.title = ggplot2::element_text(size = 6.1, face = "bold", colour = "grey20"),
      legend.text = ggplot2::element_text(size = 5.8, colour = "grey20"),
      plot.title = ggplot2::element_text(
        size = 11.1, face = "bold", colour = "grey20",
        hjust = 0, lineheight = 1.01,
        margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 6.8, colour = "grey30", lineheight = 1.02,
        hjust = 0, margin = ggplot2::margin(b = 3)
      ),
      plot.caption = ggplot2::element_text(
        size = 4.5, colour = "grey30", lineheight = 1.05,
        hjust = 0, margin = ggplot2::margin(t = 2)
      ),
      plot.margin = ggplot2::margin(0, 14, 2, 8)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title = "Número de ecuatorianos\nen el año censal",
        title.position = "top",
        title.hjust = 0,
        label.position = "right",
        barheight = grid::unit(22, "mm"),
        barwidth = grid::unit(3.2, "mm"),
        ticks = FALSE
      )
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")
p_final <- house_apply_logo(
  build_chart(),
  "portrait",
  x = 0.88,
  y = 0.10,
  width = 0.09,
  height = 0.09
)
ggplot2::ggsave(
  filename = portrait_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)
message("Guardado: ", portrait_path)
