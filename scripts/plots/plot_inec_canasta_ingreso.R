# ============================================================
# plot_inec_canasta_ingreso.R
# Grafica la distribución de ingreso y los umbrales de canasta básica.
# Requiere: data/processed/inec_canasta_ingreso.rds
# Guarda:   outputs/figures/46_canasta-basica-ingreso-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_inec_canasta_ingreso.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "tibble"))

data_path <- "data/processed/inec_canasta_ingreso.rds"
out_path <- "outputs/figures/46_canasta-basica-ingreso-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_inec_canasta_ingreso.R")
}

processed <- readRDS(data_path)
income_data <- processed$data
summary <- processed$summary

ingreso_familiar <- summary$ingreso_familiar_usd[[1]]
canasta_value <- summary$canasta_basica_usd[[1]]
p95 <- summary$ingreso_p95[[1]]
share_below <- summary$share_below_canasta[[1]]

max_break <- max(
  ceiling(max(income_data$ingreso_monetario_hogar) / 100) * 100,
  ceiling(p95 / 100) * 100
)
breaks <- sort(unique(c(seq(0, max_break, by = 100), canasta_value, ingreso_familiar)))
bin_id <- pmin(
  findInterval(income_data$ingreso_monetario_hogar, breaks, rightmost.closed = TRUE),
  length(breaks) - 1
)

hist_data <- tibble::tibble(
  xmin = breaks[bin_id],
  xmax = breaks[bin_id + 1],
  fexp = income_data$fexp
) |>
  dplyr::group_by(.data$xmin, .data$xmax) |>
  dplyr::summarise(weight = sum(.data$fexp), .groups = "drop") |>
  dplyr::mutate(
    share = .data$weight / sum(.data$weight),
    xmid = (.data$xmin + .data$xmax) / 2,
    zona = dplyr::case_when(
      .data$xmax <= canasta_value ~ "Por debajo de la canasta",
      .data$xmin >= ingreso_familiar ~ "Por encima del ingreso familiar",
      TRUE ~ "Entre la canasta y el ingreso familiar"
    )
  )

max_share <- max(hist_data$share[hist_data$xmin <= p95], na.rm = TRUE)
label_y <- max_share * 1.12

title_raw <- paste0(
  "Un ",
  round(100 * share_below),
  "% de hogares* gana menos de lo que cuesta la canasta básica"
)
subtitle_raw <- paste(
  "Ingreso monetario mensual de hogares con dos adultos y dos hijos menores,",
  "Ecuador, ENIGHUR 2024-2025"
)
caption_raw <- paste(
  "Fuente: INEC, ENIGHUR 2024-2025 e Índice de Precios al Consumidor.",
  "Elaboración: El Quantificador.",
  "Nota: se incluyen hogares de cuatro personas con dos adultos de 18 años o más, dos hijos menores de 18 años, un representante del hogar y un cónyuge o conviviente; el ingreso es monetario mensual y se pondera con Fexp. La línea azul marca el ingreso familiar oficial de 1,6 perceptores, incluidos los décimos ($877,33); la naranja, el costo de la Canasta Familiar Básica en diciembre de 2025 ($819,01). El porcentaje se calcula sobre los hogares seleccionados; el gráfico muestra ingresos hasta el percentil 95."
)

p_base <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = hist_data,
    ggplot2::aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = 0,
      ymax = .data$share,
      fill = .data$zona
    ),
    colour = "white",
    linewidth = 0.15
  ) +
  ggplot2::geom_vline(
    xintercept = ingreso_familiar,
    colour = "#2D7DB3",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  ggplot2::geom_vline(
    xintercept = canasta_value,
    colour = "#D97729",
    linewidth = 0.9
  ) +
  ggplot2::annotate(
    "segment",
    x = canasta_value - 165,
    xend = canasta_value,
    y = label_y * 0.98,
    yend = label_y * 0.98,
    colour = "#D97729",
    linewidth = 0.55
  ) +
  ggplot2::annotate(
    "label",
    x = canasta_value - 180,
    y = label_y,
    label = paste0("Canasta\nb\u00e1sica\n$", format(round(canasta_value), big.mark = ".", scientific = FALSE)),
    hjust = 1,
    vjust = 0.5,
    size = 3,
    lineheight = 0.95,
    colour = "#D97729",
    fill = "white",
    linewidth = 0,
    label.padding = grid::unit(0.18, "lines"),
    fontface = "bold"
  ) +
  ggplot2::annotate(
    "segment",
    x = ingreso_familiar + 165,
    xend = ingreso_familiar,
    y = label_y * 0.98,
    yend = label_y * 0.98,
    colour = "#2D7DB3",
    linewidth = 0.55
  ) +
  ggplot2::annotate(
    "label",
    x = ingreso_familiar + 180,
    y = label_y,
    label = paste0("Ingreso familiar de referencia\n(1,6 salarios b\u00e1sicos\nunificados)\n$", format(round(ingreso_familiar), big.mark = ".", scientific = FALSE)),
    hjust = 0,
    vjust = 0.5,
    size = 3,
    lineheight = 0.95,
    colour = "#2D7DB3",
    fill = "white",
    linewidth = 0,
    label.padding = grid::unit(0.18, "lines"),
    fontface = "bold"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Por debajo de la canasta" = "#D97729",
      "Entre la canasta y el ingreso familiar" = "#B8C7D2",
      "Por encima del ingreso familiar" = "#6FA0C4"
    )
  ) +
  ggplot2::scale_x_continuous(
    labels = label_dollar_intl(accuracy = 1),
    breaks = scales::breaks_width(250),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    expand = ggplot2::expansion(mult = c(0, 0.30))
  ) +
  ggplot2::coord_cartesian(xlim = c(0, p95), clip = "off") +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = "Ingreso monetario mensual del hogar",
    y = "Porcentaje de hogares",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 7.5, angle = 45, hjust = 1),
    panel.grid.major.y = ggplot2::element_line(colour = "grey90", linetype = "dashed"),
    plot.margin = ggplot2::margin(6, 36, 6, 16)
  )

spec <- house_spec("portrait")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  filename = out_path,
  plot = house_apply_logo(p_base, "portrait", y = 0.22),
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
