# ============================================================
# plot_acled_conflicto_ecuador.R
# Renderiza la vista acumulada del mapa de tácticas de conflicto en Ecuador.
# Requiere: data/processed/acled_conflicto_ecuador.rds,
#           data/raw/inec_geoestadistico_2022/shapefile/provincias/provincias.shp
# Guarda:   outputs/figures/44_conflicto-tacticas-visual-pass-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_acled_conflicto_ecuador.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("cowplot", "dplyr", "ggplot2", "sf", "ragg", "scales"))

processed_path <- "data/processed/acled_conflicto_ecuador.rds"
boundary_path <- "data/raw/inec_geoestadistico_2022/shapefile/provincias/provincias.shp"
out_path <- "outputs/figures/44_conflicto-tacticas-visual-pass-ecuador.png"

objeto <- readRDS(processed_path)
datos <- objeto$data
ecuador_base <- sf::st_read(boundary_path, quiet = TRUE)

data_agregada <- datos %>%
  dplyr::group_by(latitude, longitude, categoria_id) %>%
  dplyr::summarise(n_eventos = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(categoria_id != "protesta_pacifica") %>%
  dplyr::mutate(n_eventos_cap = pmin(n_eventos, 15))

total_eventos <- sum(data_agregada$n_eventos)
total_eventos_label <- scales::number(
  total_eventos,
  accuracy = 1,
  big.mark = ".",
  decimal.mark = ","
)

color_map <- c(
  "control_crimen" = "#f0a500",
  "represion_manifestantes" = "#c2185b",
  "manifestacion_violenta" = "#4361ee",
  "violencia_turbas" = "#2d3748",
  "enfrentamiento_armado" = "#b04030",
  "ataques_explosivos_drones" = "#7a2818",
  "violencia_directa_civiles" = "#e17055"
)

category_labels <- c(
  "control_crimen" = "Control y Crimen Organizado",
  "represion_manifestantes" = "Represi\u00F3n a Manifestantes",
  "manifestacion_violenta" = "Manifestaci\u00F3n Violenta",
  "violencia_turbas" = "Violencia de Turbas",
  "enfrentamiento_armado" = "Enfrentamiento Armado",
  "ataques_explosivos_drones" = "Ataques Explosivos y con Drones",
  "violencia_directa_civiles" = "Violencia Directa a Civiles"
)

total_box <- data.frame(
  longitude = -76.6,
  latitude = 1.5,
  label = paste0("Total de eventos\n", total_eventos_label)
)

title_raw <- "\u00BFC\u00F3mo ha evolucionado el conflicto en Ecuador?"
subtitle_raw <- "Eventos de conflicto registrados en Ecuador, 2018-2025"
caption_raw <- paste0(
  "Fuente: ACLED (Armed Conflict Location & Event Data Project). ",
  "Elaboraci\u00F3n: Mayari Tapia, ganadora del Concurso Ecuador Quantificado 2026, adaptaci\u00F3n por El Quantificador. ",
  "Nota: El mapa re\u00FAne ", total_eventos_label, " eventos registrados entre enero de 2018 y el 8 de junio de 2025, sin incluir protestas pac\u00EDficas. El color identifica la categor\u00EDa t\u00E1ctica. Cada punto corresponde a una combinaci\u00F3n de categor\u00EDa y ubicaci\u00F3n; el tama\u00F1o indica el total de eventos y los valores de 15 o m\u00E1s se agrupan en la categor\u00EDa m\u00E1xima."
)

spec <- house_spec("portrait")
caption_size <- 7.2
caption_wrap_width <- round(
  HOUSE_CAPTION_WRAP_WIDTH * HOUSE_CAPTION_SIZE_PT / caption_size
)

p_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = ecuador_base,
    inherit.aes = FALSE,
    fill = "#fcfcfc",
    colour = "#d1d1d1",
    linewidth = 0.16
  ) +
  ggplot2::geom_point(
    data = data_agregada,
    ggplot2::aes(
      x = longitude,
      y = latitude,
      fill = categoria_id,
      size = n_eventos_cap
    ),
    inherit.aes = FALSE,
    shape = 21,
    colour = "white",
    stroke = 0.05,
    alpha = 0.62
  ) +
  ggplot2::geom_label(
    data = total_box,
    ggplot2::aes(x = longitude, y = latitude, label = label),
    inherit.aes = FALSE,
    fill = "white",
    colour = "#0d1f2d",
    fontface = "bold",
    size = 1.55,
    hjust = 0.5,
    label.padding = grid::unit(0.3, "lines"),
    linewidth = 0.2
  ) +
  ggplot2::scale_fill_manual(
    values = color_map,
    breaks = names(color_map),
    labels = unname(category_labels[names(color_map)]),
    name = "Categor\u00EDa",
    drop = FALSE,
    guide = ggplot2::guide_legend(
      order = 2,
      override.aes = list(size = 3.8, alpha = 1)
    )
  ) +
  ggplot2::scale_size_continuous(
    range = c(0.7, 3.8),
    breaks = c(1, 3, 6, 10, 15),
    labels = c("1", "3", "6", "10", "15+"),
    name = "N\u00B0 total de eventos",
    guide = ggplot2::guide_legend(
      order = 1,
      override.aes = list(fill = "#a0aec0", colour = "white", alpha = 1)
    )
  ) +
  ggplot2::coord_sf(
    xlim = c(-81.5, -75),
    ylim = c(-5, 2.3),
    expand = FALSE,
    clip = "off"
  ) +
  ggplot2::labs(x = NULL, y = NULL) +
  theme_quantificador("portrait") +
  ggplot2::theme(
    legend.position = "none",
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(0, 0, 0, 0)
  )

legend_grob <- cowplot::get_legend(
  p_map +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold", size = 6.8, colour = "grey20"),
      legend.text = ggplot2::element_text(size = 5, colour = "grey20"),
      legend.key.height = grid::unit(0.25, "cm"),
      legend.key.width = grid::unit(0.35, "cm"),
      legend.spacing.y = grid::unit(0.06, "cm")
    )
)

p_title <- ggplot2::ggplot() +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw)
  ) +
  theme_quantificador("portrait") +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    legend.position = "none",
    plot.title = ggplot2::element_text(size = 14, face = "bold", colour = "grey20"),
    plot.subtitle = ggplot2::element_text(size = 10.5, colour = "grey30", lineheight = 1.1),
    plot.margin = ggplot2::margin(12, 16, 0, 16)
  )

p_caption <- cowplot::ggdraw() +
  cowplot::draw_label(
    wrap_caption_house(caption_raw, width = caption_wrap_width),
    x = 0.055,
    y = 0.98,
    hjust = 0,
    vjust = 1,
    size = caption_size,
    colour = "grey30",
    lineheight = 1.1
  )

p_body <- cowplot::ggdraw() +
  cowplot::draw_plot(p_map, x = -0.04, y = 0.06, width = 0.69, height = 0.92) +
  cowplot::draw_grob(legend_grob, x = 0.72, y = 0.13, width = 0.25, height = 0.78)

p_composed <- cowplot::plot_grid(
  p_title,
  p_body,
  p_caption,
  ncol = 1,
  rel_heights = c(0.15, 0.61, 0.24),
  align = "v"
)

p_final <- house_apply_logo(p_composed, "portrait", y = 0.26)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
ggplot2::ggsave(
  filename = out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
message("Eventos representados: ", total_eventos)
