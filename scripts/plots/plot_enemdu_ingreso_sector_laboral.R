# ============================================================
# plot_enemdu_ingreso_sector_laboral.R
# Genera un boxplot horizontal del ingreso laboral mensual por
# sector formal e informal con estilo de El Quantificador.
# Requiere: data/processed/enemdu_ingreso_sector_laboral_2026_03.rds
# Guarda:   outputs/figures/21_ingreso-laboral_sector-formal-informal-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_ingreso_sector_laboral.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "stringr", "ggtext"))

input_path <- "data/processed/enemdu_ingreso_sector_laboral_2026_03.rds"
out_path <- "outputs/figures/21_ingreso-laboral_sector-formal-informal-ecuador.png"

chart_data <- readRDS(input_path)
plot_df <- chart_data$summary %>%
  mutate(
    ratio_mediana = mediana[sector == "Formal"] / mediana[sector == "Informal"],
    label_x = c(610, 730),
    label_y = c(0.86, 2),
    median_label = paste0("Mediana: ", scales::dollar(mediana, accuracy = 1))
  )

ratio_txt <- scales::number(plot_df$ratio_mediana[[1]], accuracy = 0.1, decimal.mark = ",")

title_txt <- stringr::str_wrap(
  paste0(
    "La mediana del ingreso laboral formal es ",
    ratio_txt,
    " veces la del sector informal"
  ),
  width = 42
)

subtitle_txt <- stringr::str_wrap(
  "Distribución del ingreso laboral mensual de personas ocupadas de 15 años o más, ENEMDU marzo 2026",
  width = 58
)

caption_txt <- paste0(
  "Fuente: ENEMDU - INEC, marzo 2026. Cálculos de Daniel Sánchez para El Quantificador de ",
  "Laboratorio LIDE. Se muestran únicamente trabajadores de 15 años o más con ingreso laboral ",
  "positivo. Los casos sin clasificación de sector se reasignan usando la afiliación a la seguridad social ",
  "como criterio auxiliar de formalidad. Los percentiles son ponderados por el factor de expansión muestral. La visualización ",
  "recorta el eje en el percentil 90 del ingreso laboral para mejorar la lectura. Caja = p25-p75; línea = mediana; ",
  "bigotes = p10-p90."
)

palette_fill <- c(
  "Formal" = "#EF9F4E",
  "Informal" = "#BFD9DE"
)

palette_text <- c(
  "Formal" = "#A85F12",
  "Informal" = "#5E7E84"
)

p_base <- ggplot(
  plot_df,
  aes(
    x = sector,
    fill = sector,
    ymin = p10,
    lower = p25,
    middle = mediana,
    upper = p75,
    ymax = p90
  )
) +
  geom_boxplot(
    stat = "identity",
    width = 0.48,
    color = "#222222",
    linewidth = 0.5,
    alpha = 0.98
  ) +
  geom_segment(
    aes(
      xend = sector,
      y = mediana,
      yend = label_x - 22,
      color = sector
    ),
    linewidth = 0.35,
    show.legend = FALSE
  ) +
  geom_label(
    aes(
      y = label_x,
      x = label_y,
      label = median_label,
      color = sector
    ),
    hjust = 0,
    fill = "white",
    fontface = "plain",
    size = 2.9,
    label.padding = grid::unit(0.12, "lines"),
    label.r = grid::unit(0.12, "lines"),
    linewidth = 0.2,
    show.legend = FALSE
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = palette_fill) +
  scale_color_manual(values = palette_text) +
  scale_y_continuous(
    labels = label_dollar_intl(accuracy = 1),
    breaks = seq(0, 900, by = 200),
    limits = c(0, 920),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = "Ingreso laboral mensual (USD)",
    caption = caption_txt
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(colour = "black", size = 8),
    axis.text.x = element_text(colour = "black", size = 7),
    axis.title.x = element_text(size = 7, colour = "black", margin = margin(t = 8)),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(colour = "black", size = 12.3, face = "bold", lineheight = 1.02),
    plot.subtitle = element_text(colour = "black", size = 8.7, lineheight = 1.0),
    plot.caption = ggtext::element_textbox_simple(
      colour = "black",
      size = 5.5,
      lineheight = 1.06,
      halign = 0,
      hjust = 0,
      margin = margin(t = 10),
      padding = margin(0, 0, 0, 0),
      fill = NA,
      box.color = NA
    ),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(6, 12, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
p_final <- add_logo(p_base, x = 0.90, y = 0.19)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)

message("Guardado: ", out_path)
