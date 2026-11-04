# ============================================================
# plot_21_enemdu_ingreso_sector_laboral.R
# Distribución del ingreso laboral por sector, marzo de 2026.
# Requiere: data/processed/enemdu_ingreso_sector_laboral_2026_03.rds
# Guarda: outputs/figures/21_ingreso-laboral_sector-formal-informal-ecuador.png
# Ejecutar desde la raíz: Rscript scripts/plots/plot_21_enemdu_ingreso_sector_laboral.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

input_path <- "data/processed/enemdu_ingreso_sector_laboral_2026_03.rds"
out_path <- "outputs/figures/21_ingreso-laboral_sector-formal-informal-ecuador.png"

chart_data <- readRDS(input_path)
plot_df <- chart_data$summary |>
  dplyr::mutate(
    sector = factor(sector, levels = c("Informal", "Formal")),
    median_label = paste0(
      "Mediana: ", scales::dollar(mediana, accuracy = 1), "\n",
      scales::number(personas_ponderadas / 1e6, accuracy = 0.1, decimal.mark = ","),
      " millones"
    )
  )

title_raw <- "La mediana del ingreso informal es menos de la mitad de la formal"
subtitle_raw <- "Ingresos laborales mensuales, por sector de ocupación, marzo de 2026"
caption_raw <- paste(
  "Fuente: INEC, ENEMDU marzo 2026. Elaboración: Eddie Tomalá para El Quantificador de Laboratorio LIDE.",
  "Nota: Trabajadores de 15 años o más con ingreso laboral positivo y sector formal o informal identificado.",
  "Estadísticas ponderadas con el factor de expansión. Caja: percentiles 25 a 75; línea: mediana; bigotes: percentiles 10 a 90."
)

p_base <- ggplot(
  plot_df,
  aes(x = sector, fill = sector, ymin = p10, lower = p25,
      middle = mediana, upper = p75, ymax = p90)
) +
  geom_boxplot(stat = "identity", width = 0.48, colour = "grey20", linewidth = 0.5) +
  geom_text(
    aes(y = mediana, label = median_label),
    nudge_x = 0.46, hjust = 0.5, size = 3, colour = "grey20", lineheight = 1.1
  ) +
  coord_flip(ylim = c(0, 1350), clip = "off") +
  scale_fill_manual(values = c("Formal" = "#EF9F4E", "Informal" = "#BFD9DE")) +
  scale_y_continuous(labels = label_dollar_intl(accuracy = 1), breaks = seq(0, 1200, 300)) +
  labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Ingreso laboral mensual (USD)",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador()

p_final <- add_logo(p_base)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300, device = ragg::agg_png, bg = "white")
message("Guardado: ", out_path)
