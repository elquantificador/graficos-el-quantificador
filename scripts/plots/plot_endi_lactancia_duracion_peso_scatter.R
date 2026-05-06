# ============================================================
# plot_endi_lactancia_duracion_peso_scatter.R
# Genera un scatter exploratorio de duracion de lactancia
# exclusiva vs peso infantil.
# Requiere: data/processed/endi_r2_lactancia_duracion_peso_scatter.rds
# Guarda:   figures/endi_lactancia_duracion_peso_scatter.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "stringr"))

out_path <- "figures/endi_lactancia_duracion_peso_scatter.png"
plot_df <- readRDS("data/processed/endi_r2_lactancia_duracion_peso_scatter.rds")

title_txt <- stringr::str_wrap(
  "La duracion reportada de la lactancia exclusiva no muestra una relacion clara con el peso infantil",
  width = 52
)

subtitle_txt <- stringr::str_wrap(
  "Scatter exploratorio entre la duracion de la lactancia exclusiva y el peso de ninos y ninas. ENDI Ronda 2 2023-2024",
  width = 60
)

caption_txt <- stringr::str_wrap(
  paste(
    "Fuente: INEC, Encuesta Nacional sobre Desnutricion Infantil (ENDI), Ronda 2 2023-2024.",
    "La duracion de lactancia exclusiva combina meses y dias reportados en el modulo de lactancia.",
    "El peso del nino o nina corresponde al promedio de las mediciones disponibles de peso."
  ),
  width = 102
)

p_base <- ggplot(
  plot_df,
  aes(x = duracion_lact_excl_meses, y = weight_kg)
) +
  geom_point(alpha = 0.22, size = 1.3, colour = "#2A9D8F") +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8, colour = "#C44E52") +
  scale_x_continuous(
    labels = scales::label_number(decimal.mark = ","),
    breaks = seq(0, 24, by = 3),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ","),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = "Duracion de lactancia exclusiva (meses)",
    y = "Peso del nino o nina (kg)",
    caption = caption_txt
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(colour = "black", size = 7.5),
    axis.text.x = element_text(colour = "black", size = 7),
    axis.title.x = element_text(size = 7, margin = margin(t = 10), colour = "black"),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), colour = "black"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.2, lineheight = 1.1, hjust = 0, margin = margin(t = 6)),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(6, 32, 8, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.08, width = 0.09, height = 0.09)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 5,
  height = 5.3,
  dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)
