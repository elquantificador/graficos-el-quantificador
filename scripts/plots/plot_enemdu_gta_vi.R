# ============================================================
# plot_enemdu_gta_vi.R
# Horas de trabajo para comprar GTA VI por provincia.
# Requiere: data/processed/enemdu_gta_vi.rds, data/raw/gta_vi/geoecu_dpa_baja.gpkg
# Guarda: outputs/figures/draft_gta-vi-horas-trabajo-ecuador.png
# ============================================================
source('scripts/utils.R')
source('scripts/packages.R')
ensure_packages(c('dplyr', 'sf', 'geoecu', 'ggplot2', 'cowplot', 'ragg', 'scales'))

estimates <- readRDS('data/processed/enemdu_gta_vi.rds')
geoecu::geoecu_usar_carpeta('data/raw/gta_vi')
provinces <- geoecu::read_provincias(crs = 32717) |>
  dplyr::mutate(prov = suppressWarnings(as.integer(dpa_provin))) |>
  dplyr::left_join(estimates, by = 'prov')
points <- provinces |>
  dplyr::filter(!is.na(hours)) |>
  sf::st_point_on_surface() |>
  sf::st_transform(4326)
coordinates <- sf::st_coordinates(points)
labels <- sf::st_drop_geometry(points) |>
  dplyr::mutate(x = coordinates[, 1], y = coordinates[, 2], label = scales::number(hours, accuracy = 1))

# Separate nearby labels with short leader lines in the central highlands.
labels <- labels |>
  dplyr::mutate(
    label_x = x + dplyr::case_when(prov == 2 ~ -0.13, prov == 6 ~ 0.17, prov == 18 ~ 0.18, prov == 5 ~ -0.15, prov == 4 ~ 0.12, prov == 10 ~ -0.08, TRUE ~ 0),
    label_y = y + dplyr::case_when(prov == 2 ~ 0.05, prov == 6 ~ -0.10, prov == 18 ~ 0.02, prov == 5 ~ -0.02, prov == 4 ~ 0.16, prov == 10 ~ -0.08, TRUE ~ 0)
  )
main_labels <- dplyr::filter(labels, prov != 20)
continent <- sf::st_transform(provinces, 4326)

title <- paste0('La mayoría de trabajadores ecuatorianos\nnecesita trabajar al menos ', attr(estimates, 'national_hours'), ' horas para\npoder comprar GTA VI')
subtitle <- 'Horas de trabajo requeridas para ganar $91.99, por provincia continental, ENEMDU anual 2025'
caption <- paste('Fuente: INEC, ENEMDU anual 2025; CONALI (geoecu) y PlayStation Ecuador.',
  'Elaboración: Alonso Quijano y Daniel Sánchez para El Quantificador de Laboratorio LIDE.',
  'Nota: estimaciones basadas en la mediana del ingreso laboral por hora de personas ocupadas de 15 años o más con ingresos positivos. Se supone que todo el ingreso se destina al juego. Precio: US$91,99 con IVA; no incluye consola ni ISD.')

base <- ggplot2::ggplot(continent) +
  ggplot2::geom_sf(ggplot2::aes(fill = hours), colour = 'white', linewidth = 0.2) +
  ggplot2::geom_segment(data = dplyr::filter(main_labels, x != label_x | y != label_y),
    ggplot2::aes(x = x, y = y, xend = label_x, yend = label_y),
    inherit.aes = FALSE, colour = 'grey35', linewidth = 0.25) +
  ggplot2::geom_label(data = main_labels,
    ggplot2::aes(x = label_x, y = label_y, label = label),
    inherit.aes = FALSE, size = 2.45, fontface = 'bold', colour = 'grey20',
    fill = 'white', linewidth = 0, label.padding = grid::unit(0.08, 'lines')) +
  ggplot2::scale_fill_gradient(low = '#dceef4', high = '#005b83',
    name = 'Horas', na.value = 'grey90', limits = range(estimates$hours[estimates$prov != 20]), breaks = c(35, 45, 55, 65),
    guide = ggplot2::guide_colourbar(barheight = grid::unit(1.2, 'in'), barwidth = grid::unit(0.12, 'in'))) +
  ggplot2::coord_sf(xlim = c(-81.2, -75), ylim = c(-5.2, 1.8), expand = FALSE) +
  ggplot2::labs(x = NULL, y = NULL) +
  theme_quantificador() +
  ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
    legend.position = 'right', legend.title = ggplot2::element_text(size = 7),
    legend.text = ggplot2::element_text(size = 7.5))

header <- cowplot::ggdraw() +
  cowplot::draw_label(title, x = 0, y = 0.95, hjust = 0, vjust = 1, fontface = 'bold', size = 12.5, colour = 'grey20') +
  cowplot::draw_label(wrap_subtitle_house(subtitle), x = 0, y = 0.35, hjust = 0, vjust = 1, size = 9, colour = 'grey30')
footer <- cowplot::ggdraw() + cowplot::draw_label(wrap_caption_house(caption), x = 0, y = 1, hjust = 0, vjust = 1, size = 6.5, colour = 'grey30', lineheight = 1.1)
legend <- cowplot::get_legend(base)
body <- cowplot::ggdraw() +
  cowplot::draw_plot(base + ggplot2::theme(legend.position = 'none'), x = 0.06, y = 0, width = 0.94, height = 1) +
  cowplot::draw_grob(legend, x = 0.83, y = 0.28, width = 0.16, height = 0.44)
composed <- cowplot::plot_grid(header, body, footer, ncol = 1, rel_heights = c(0.21, 0.65, 0.14))
framed <- cowplot::ggdraw() + cowplot::draw_plot(composed, x = 0.05, y = 0, width = 0.80, height = 1)
final <- house_apply_logo(framed, y = 0.19)
out <- 'outputs/figures/draft_gta-vi-horas-trabajo-ecuador.png'
ggplot2::ggsave(out, final, width = 4, height = 5, dpi = 300, device = ragg::agg_png, bg = 'white')
message('Guardado: ', out)
