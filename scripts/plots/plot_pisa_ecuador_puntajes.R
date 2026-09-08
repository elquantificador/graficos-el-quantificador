# ============================================================
# plot_pisa_ecuador_puntajes.R
# Genera el panel de puntajes promedio de Ecuador en PISA.
# Requiere: data/processed/pisa_ecuador_puntajes.rds
# Guarda:   outputs/figures/pisa-puntajes-ecuador.png
#           outputs/figures/pisa-puntajes-ecuador.svg
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_pisa_ecuador_puntajes.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "svglite"))

input_path <- "data/processed/pisa_ecuador_puntajes.rds"
png_path <- "outputs/figures/pisa-puntajes-ecuador.png"
svg_path <- "outputs/figures/pisa-puntajes-ecuador.svg"

chart_data <- readRDS(input_path)
plot_df <- chart_data$summary |>
  dplyr::mutate(
    subject = as.character(subject),
    year = as.character(year),
    ci_low = mean_score - 1.96 * standard_error,
    ci_high = mean_score + 1.96 * standard_error,
    score_label = formatC(round(mean_score, 1), format = "f", digits = 1, decimal.mark = ","),
    subject_y = dplyr::case_when(
      subject == unique(subject)[1] ~ 3,
      subject == unique(subject)[2] ~ 2,
      TRUE ~ 1
    ),
    point_y = subject_y + dplyr::if_else(year == "2017", 0.10, -0.10)
  )

palette <- c(
  "2017" = "#00A1CB",
  "2025" = "#EF9F4E"
)

add_horizontal_ci <- function(plot, data, low, high, y, colour = "grey45") {
  plot +
    ggplot2::geom_segment(
      data = data,
      ggplot2::aes(x = {{ low }}, xend = {{ high }}, y = {{ y }}, yend = {{ y }}),
      inherit.aes = FALSE,
      colour = colour,
      linewidth = 0.7
    ) +
    ggplot2::geom_segment(
      data = data,
      ggplot2::aes(x = {{ low }}, xend = {{ low }}, y = {{ y }} - 0.065, yend = {{ y }} + 0.065),
      inherit.aes = FALSE,
      colour = colour,
      linewidth = 0.7
    ) +
    ggplot2::geom_segment(
      data = data,
      ggplot2::aes(x = {{ high }}, xend = {{ high }}, y = {{ y }} - 0.065, yend = {{ y }} + 0.065),
      inherit.aes = FALSE,
      colour = colour,
      linewidth = 0.7
    )
}

p_base <- ggplot2::ggplot(plot_df, ggplot2::aes(y = point_y))
p_base <- add_horizontal_ci(p_base, plot_df, ci_low, ci_high, point_y)
p_base <- p_base +
  ggplot2::geom_point(
    ggplot2::aes(x = mean_score, colour = year),
    size = 2.8
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      x = mean_score,
      label = score_label,
      vjust = ifelse(year == "2017", -1.3, 2.0)
    ),
    colour = "grey20",
    size = 2.8
  ) +
  ggplot2::scale_colour_manual(
    values = palette,
    name = NULL,
    breaks = c("2017", "2025"),
    labels = c("2017", "2025")
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(3, 2, 1),
    labels = unique(plot_df$subject),
    limits = c(0.55, 3.45),
    expand = c(0, 0)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(360, 420, by = 20),
    limits = c(355, 425),
    labels = label_number_intl(accuracy = 1),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    title = wrap_title_house("Ecuador obtuvo puntajes más bajos en lectura y matemáticas en PISA 2025"),
    subtitle = wrap_subtitle_house("Puntaje promedio, por componente PISA, 2017 y 2025"),
    x = "Puntaje promedio",
    y = NULL,
    caption = wrap_caption_house(paste(
      "Fuente: OECD, PISA 2025 Results Volume I.",
      "Elaboración: Daniel Sánchez Pazmiño para el Quantificador de Laboratorio LIDE.",
      "Nota: 2017 corresponde a PISA for Development. Los intervalos de confianza del 95% usan los errores estándar reportados por el OECD. La diferencia en ciencias no fue estadísticamente significativa según el OECD."
    ))
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 8),
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.direction = "horizontal",
    legend.background = ggplot2::element_rect(fill = "white", colour = NA),
    legend.box.background = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 7.5, colour = "grey20"),
    legend.key.width = grid::unit(5, "mm"),
    legend.key.height = grid::unit(4, "mm"),
    plot.margin = ggplot2::margin(6, 32, 6, 16)
  )

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
spec <- house_spec("portrait")

ggplot2::ggsave(
  filename = png_path,
  plot = house_apply_logo(p_base, "portrait", x = 0.88, y = 0.16),
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

ggplot2::ggsave(
  filename = svg_path,
  plot = house_apply_logo(p_base, "portrait", x = 0.88, y = 0.16),
  width = spec$width,
  height = spec$height,
  device = svglite::svglite,
  bg = "white"
)

message("Guardado: ", png_path)
message("Guardado: ", svg_path)
