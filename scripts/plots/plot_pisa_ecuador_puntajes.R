# ============================================================
# plot_pisa_ecuador_puntajes.R
# Genera barras agrupadas de los puntajes promedio de Ecuador en PISA.
# Requiere: data/processed/pisa_ecuador_puntajes.rds
# Guarda:   outputs/figures/pisa-puntajes-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_pisa_ecuador_puntajes.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

input_path <- "data/processed/pisa_ecuador_puntajes.rds"
out_path <- "outputs/figures/pisa-puntajes-ecuador.png"

chart_data <- readRDS(input_path)
plot_df <- chart_data$summary

palette <- c(
  "2017" = "#00A1CB",
  "2025" = "#EF9F4E"
)

dodge <- position_dodge(width = 0.72)

title_raw <- "Ecuador obtuvo puntajes más bajos en PISA 2025 que en PISA-D 2017"
subtitle_raw <- "Puntaje promedio en ciencias, lectura y matemáticas, Ecuador. La diferencia en ciencias no fue estadísticamente significativa."
caption_raw <- paste(
  "Fuente: OECD, PISA 2025 Results Volume I, tablas I.B1.2a.36-38.",
  "Elaboración: El Quantificador.",
  "Nota: 2017 corresponde a PISA for Development. La diferencia en ciencias no fue estadísticamente significativa según el OECD."
)

p_base <- ggplot(plot_df, aes(x = subject, y = mean_score, fill = year)) +
  geom_col(
    position = dodge,
    width = 0.62,
    colour = NA
  ) +
  geom_text(
    aes(label = score_label),
    position = dodge,
    vjust = -0.28,
    size = 2.6,
    colour = "grey20"
  ) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(
    breaks = seq(0, 500, by = 100),
    limits = c(0, 500),
    labels = label_number_intl(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Puntaje promedio",
    fill = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.text = element_text(size = 7.5, colour = "grey20"),
    legend.key.width = grid::unit(5, "mm"),
    legend.key.height = grid::unit(4, "mm"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 8),
    plot.margin = margin(6, 32, 6, 16)
  )

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
spec <- house_spec("portrait")
p_final <- house_apply_logo(p_base, "portrait", x = 0.88, y = 0.16)

ggsave(
  filename = out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
