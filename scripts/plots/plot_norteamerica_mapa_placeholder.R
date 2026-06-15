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
  ~admin,                     ~country_label,            ~value,  ~fill_group,
  "Canada",                   "Canadá\n16.320 (2021)",          16320,   "yellow",
  "United States of America", "Estados Unidos\n621.915 (2024)", 621915,  "blue",
  "Mexico",                   "México\n4.615 (2020)",            4615,   "red"
)

map_df <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  dplyr::filter(admin %in% values_df$admin) |>
  suppressWarnings(sf::st_cast("POLYGON")) |>
  dplyr::mutate(
    centroid = sf::st_centroid(geometry),
    centroid_x = sf::st_coordinates(centroid)[, "X"],
    centroid_y = sf::st_coordinates(centroid)[, "Y"]
  ) |>
  dplyr::filter(!(admin == "United States of America" & centroid_y < 24)) |>
  dplyr::filter(!(admin == "United States of America" & centroid_x < -150 & centroid_y < 30)) |>
  dplyr::select(-centroid, -centroid_x, -centroid_y) |>
  dplyr::left_join(values_df, by = "admin") |>
  dplyr::group_by(admin, country_label, value, fill_group) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")

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

title_txt <- "Más de 640 mil ecuatorianos apoyan a la Tricolor\ndesde Norteamérica, en las buenas y en las malas"
subtitle_raw <- paste(
  "Ecuatorianos viviendo en Norteamérica,",
  "por país"
)
caption_raw <- paste(
  "Fuentes: Statistics Canada, Census of Population 2021, tabla 98-10-0349-01;",
  "U.S. Census Bureau (2024), American Community Survey, tabla B05006; Instituto Nacional de Estadística y Geografía (INEGI), Censos de Población y Vivienda 2020.",
  "Notas:",
  "Se utilizan los estimados censales de Canadá y México más recientes.",
  "Para Estados Unidos, se utilizó la American Community Survey (2024).",
  "* Se considera a personas nacidas en territorio ecuatoriano solamente.",
  "Debido a los flujos irregulares masivos hacia EE.UU. desde 2020, las cifras deben interpretarse con cautela.",
  "Base cartográfica: Natural Earth.",
  "Elaboración: Daniel Sánchez para el Quantificador de Laboratorio LIDE."
)

build_chart <- function() {
  label_size <- 2.55
  caption_width <- 132
  subtitle_txt <- "Estimados más recientes de ecuatorianos* viviendo en Norteamérica, por país"

  ggplot2::ggplot(map_df) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = fill_group),
      colour = "white",
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "rect",
      xmin = -162,
      xmax = -146,
      ymin = 14,
      ymax = 24,
      fill = "white",
      colour = NA
    ) +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(x = X, y = Y, label = country_label),
      size = label_size,
      colour = "grey15",
      lineheight = 0.9,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "yellow" = "#FFD100",
        "blue" = "#0072CE",
        "red" = "#EF3340"
      )
    ) +
    ggplot2::coord_sf(
      xlim = c(-162, -58),
      ylim = c(14, 82),
      expand = FALSE,
      datum = NA
    ) +
    ggplot2::labs(
      title = title_txt,
      subtitle = subtitle_txt,
      caption = wrap_caption_house(caption_raw, width = caption_width),
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_quantificador("portrait") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.position = "none",
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
        hjust = 0, margin = ggplot2::margin(t = 1)
      ),
      plot.margin = ggplot2::margin(0, 10, 0, 6)
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")
p_final <- house_apply_logo(
  build_chart(),
  "portrait",
  x = 0.88,
  y = 0.14,
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
