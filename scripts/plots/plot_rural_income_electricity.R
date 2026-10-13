# ============================================================
# plot_rural_income_electricity.R
# Genera un gráfico de borrador para comparar el ingreso laboral
# rural y la recaudación nacional del servicio eléctrico.
# Requiere: data/processed/rural_income_electricity_annual.rds
# Guarda:   outputs/figures/draft_rural_income_electricity-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_rural_income_electricity.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "stringr", "tidyr"))

annual <- readRDS("data/processed/rural_income_electricity_annual.rds")

plot_data <- annual |>
  dplyr::select(
    year,
    `Ingreso laboral rural` = income_index_2007,
    `Recaudación del servicio eléctrico` = recaudacion_index_2007
  ) |>
  tidyr::pivot_longer(
    cols = -year,
    names_to = "serie",
    values_to = "indice"
  )

if (nrow(plot_data) != 38L || any(!is.finite(plot_data$indice))) {
  stop("El gráfico espera dos series completas para 2007-2025.")
}

series_colours <- c(
  "Ingreso laboral rural" = "#2D7DB3",
  "Recaudación del servicio eléctrico" = "#E07B39"
)

caption_raw <- paste(
  "Fuentes: INEC, ENEMDU e IPC; ARCONEL, Facturación Total.",
  "Índice 2007 = 100. El ingreso rural es mensual promedio por persona con empleo, en dólares de diciembre de 2025.",
  "La recaudación suma las filas del reporte nacional de ARCONEL y no mide hogares rurales.",
  "La comparación describe movimientos agregados y no prueba causalidad. Borrador."
)

final_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = year, y = indice, colour = serie, group = serie)
) +
  ggplot2::geom_hline(
    yintercept = 100,
    colour = "grey80",
    linewidth = 0.45
  ) +
  ggplot2::geom_vline(
    xintercept = 2020,
    colour = "grey82",
    linewidth = 0.45,
    linetype = "dashed"
  ) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.6) +
  ggplot2::scale_colour_manual(values = series_colours, name = NULL) +
  ggplot2::scale_x_continuous(
    breaks = c(2007, 2010, 2015, 2020, 2025),
    limits = c(2007, 2025),
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(100, 180, by = 20),
    labels = label_number_intl(accuracy = 1),
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    title = "Ingreso rural y recaudación eléctrica",
    subtitle = "Ecuador, 2007-2025 | índice 2007 = 100",
    x = NULL,
    y = "Índice",
    caption = stringr::str_wrap(caption_raw, width = 64)
  ) +
  theme_quantificador() +
  ggplot2::theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.text = ggplot2::element_text(size = 6.5),
    legend.key.width = grid::unit(0.9, "cm"),
    legend.margin = ggplot2::margin(b = 3),
    axis.title.y = ggplot2::element_text(size = 7, margin = ggplot2::margin(r = 6)),
    plot.margin = ggplot2::margin(6, 24, 6, 16)
  )

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")
dest <- "outputs/figures/draft_rural_income_electricity-ecuador.png"

ggplot2::ggsave(
  dest,
  house_apply_logo(final_plot, "portrait", x = 0.89, y = 0.25, width = 0.06, height = 0.06),
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", dest)
