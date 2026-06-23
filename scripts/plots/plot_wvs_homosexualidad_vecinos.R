# ============================================================
# plot_wvs_homosexualidad_vecinos.R
# Genera el gráfico sobre el porcentaje de personas que
# preferirían no tener a un homosexual como vecino.
# Requiere: data/processed/wvs_homosexualidad_vecinos.rds
# Guarda:   outputs/figures/25_homosexualidad-vecinos-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_wvs_homosexualidad_vecinos.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

input_path <- "data/processed/wvs_homosexualidad_vecinos.rds"
out_path <- "outputs/figures/25_homosexualidad-vecinos-ecuador.png"

plot_df <- readRDS(input_path)

title_raw <- "Uno de cada tres ecuatorianos preferiría no tener a un homosexual como vecino"
subtitle_raw <- paste(
  "Porcentaje de hombres y mujeres que preferirían no tener",
  "a un homosexual como vecino, WVS 2013 y 2018"
)
caption_raw <- paste(
  "Fuente: World Values Survey (WVS), Ecuador, 2013 y 2018. Elaboración:",
  "Alonso Quijano-Ruiz para El Quantificador. Nota: Los porcentajes",
  "están ponderados usando el factor muestral de la encuesta."
)

palette_year <- c("2013" = "#4F669C", "2018" = "#D1495B")

p_base <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = sexo, y = porcentaje, fill = anio)
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.72),
    width = 0.6
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = percent_intl(porcentaje, accuracy = 1)),
    position = ggplot2::position_dodge(width = 0.72),
    vjust = -0.35,
    size = 2.9,
    colour = "grey20"
  ) +
  ggplot2::scale_fill_manual(values = palette_year) +
  ggplot2::scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    limits = c(0, 0.45),
    breaks = seq(0, 0.4, by = 0.1),
    expand = ggplot2::expansion(mult = c(0, 0.03))
  ) +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Porcentaje",
    fill = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  ggplot2::theme(
    legend.position = "top",
    legend.justification = "left",
    legend.text = ggplot2::element_text(size = 7.5, colour = "grey20"),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 8, colour = "grey20"),
    plot.margin = ggplot2::margin(6, 14, 6, 12)
  )

p_final <- add_logo(p_base, x = 0.88, y = 0.18)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggplot2::ggsave(
  out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
