# ============================================================
# plot_50_inec_mercado_laboral_pobreza_2026.R
# Grafica los últimos indicadores oficiales disponibles del mercado laboral y la pobreza.
# Requiere: data/processed/inec_mercado_laboral_pobreza_2026.rds
# Guarda:   outputs/figures/50_mercado-laboral-pobreza-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_50_inec_mercado_laboral_pobreza_2026.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/inec_mercado_laboral_pobreza_2026.rds"
out_path <- "outputs/figures/50_mercado-laboral-pobreza-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_inec_mercado_laboral_pobreza_2026.R")
}

processed <- readRDS(data_path)
plot_data <- processed$data |>
  dplyr::mutate(
    label = paste0(
      scales::number(.data$valor, accuracy = 0.1, decimal.mark = ","),
      " %"
    )
  )

title_raw <- "Ecuador no tiene datos de empleo desde mayo de 2026 (y el Banco Mundial pide justificaciones)"
subtitle_raw <- paste(
  "Indicadores del mercado laboral y pobreza, ENEMDU 2026"
)
caption_raw <- paste(
  "Fuente: INEC, ENEMDU mayo de 2026 (mercado laboral) y diciembre de 2025",
  "(pobreza por ingresos). Elaboración: Daniel Sánchez Pazmiño para El Quantificador.",
  "Nota: fecha prevista de publicación de los datos, 22 de diciembre de 2026."
)

p_base <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = .data$indicador, y = .data$valor)
) +
  ggplot2::geom_col(fill = "#3A789F", width = 0.72) +
  ggplot2::geom_text(
    ggplot2::aes(label = .data$label),
    hjust = -0.08,
    size = 3.1,
    colour = "grey20"
  ) +
  ggplot2::facet_wrap(
    ~grupo,
    ncol = 1,
    scales = "free_y",
    strip.position = "top"
  ) +
  ggplot2::scale_y_continuous(
    labels = label_number_intl(accuracy = 10),
    limits = c(0, 60),
    breaks = seq(0, 60, by = 20),
    expand = ggplot2::expansion(mult = c(0, 0.08))
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Porcentaje",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 7.5),
    axis.text.x = ggplot2::element_text(size = 7),
    axis.title.x = ggplot2::element_text(size = 7, margin = ggplot2::margin(t = 6)),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      colour = "grey20",
      size = 9,
      face = "bold",
      hjust = 0
    ),
    panel.spacing = grid::unit(1.2, "lines"),
    plot.margin = ggplot2::margin(6, 42, 6, 16)
  )

spec <- house_spec("portrait")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  filename = out_path,
  plot = house_apply_logo(p_base, "portrait", y = 0.18),
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
