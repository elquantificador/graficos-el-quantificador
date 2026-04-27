# ============================================================
# plot_sest_puntaje_provincia.R
# Genera el gráfico del puntaje global promedio de
# Ser Estudiante 2024-2025 por provincia.
# Requiere: data/processed/sest_puntaje_provincia.rds
# Guarda:   figures/sest_puntaje_provincia.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_sest_puntaje_provincia.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "stringr", "ragg"))

out_path <- "figures/sest_puntaje_provincia.png"
series <- readRDS("data/processed/sest_puntaje_provincia.rds")

plot_df <- series$province_scores %>%
  mutate(
    provincia = stringr::str_wrap(as.character(provincia), width = 24),
    provincia = factor(provincia, levels = rev(provincia)),
    etiqueta = scales::number(puntaje_global, accuracy = 0.1, decimal.mark = ",")
  )

caption_txt <- stringr::str_wrap(
  paste(
    "Fuente: INEVAL, Ser Estudiante 2024-2025, microdatos de evaluación nacional.",
    "Se presenta el promedio global provincial ponderado con el factor de expansión",
    "`fex_inev` para sustentantes con estado de evaluación `Evaluado`.",
    "Cálculos de Daniel Sánchez para El Quantificador de Laboratorio LIDE."
  ),
  width = 95
)

p_base <- ggplot(plot_df, aes(x = provincia, y = puntaje_global)) +
  geom_col(fill = "#EF9F4E", width = 0.64) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.08,
    size = 2.2,
    colour = "black"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", accuracy = 1),
    limits = c(0, 735),
    breaks = seq(0, 700, 100),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Chimborazo y Cotopaxi registran los puntajes\nglobales más altos en Ser Estudiante",
    subtitle = "Puntaje global promedio por provincia, Ser Estudiante 2024-2025",
    x = NULL,
    y = "Puntaje global promedio",
    caption = caption_txt
  ) +
  theme_quantificador() +
  theme(
    axis.text.y = element_text(colour = "grey20", size = 6.6),
    axis.text.x = element_text(colour = "grey20", size = 7),
    plot.caption = element_text(size = 5.4, lineheight = 1.1, hjust = 0, margin = margin(t = 8)),
    plot.margin = margin(8, 52, 8, 14)
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.885, y = 0.14, width = 0.09, height = 0.09)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 7.2,
  dpi = 320,
  device = ragg::agg_png
)

message("Guardado: ", out_path)
