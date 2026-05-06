# ============================================================
# plot_endi_lactancia_etnia.R
# Genera el grafico del porcentaje de ninos y ninas cuya madre
# les dio el seno al nacer, por etnia.
# Requiere: data/processed/endi_r2_lactancia_nacer_etnia.rds
# Guarda:   figures/endi_lactancia_nacer_etnia.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "forcats", "scales", "ragg", "stringr"))

out_path <- "figures/endi_lactancia_nacer_etnia.png"
plot_df <- readRDS("data/processed/endi_r2_lactancia_nacer_etnia.rds")

title_txt <- stringr::str_wrap(
  "La lactancia al nacer supera el 85% en todos los grupos etnicos del Ecuador",
  width = 46
)

subtitle_txt <- stringr::str_wrap(
  "Porcentaje de ninos y ninas cuya madre les dio el seno al nacer, por etnia. ENDI Ronda 2 2023-2024",
  width = 58
)

caption_txt <- stringr::str_wrap(
  paste(
    "Fuente: INEC, Encuesta Nacional sobre Desnutricion Infantil (ENDI), Ronda 2 2023-2024.",
    "Calculos de Daniel Sanchez para El Quantificador de Laboratorio LIDE.",
    "La estimacion usa el modulo de lactancia y el factor de expansion para lactantes.",
    "La etnia corresponde a la madre, enlazada desde el archivo de personas."
  ),
  width = 95
)

p_base <- ggplot(
  plot_df,
  aes(x = forcats::fct_reorder(as.character(etnia), pct_dio_seno), y = pct_dio_seno)
) +
  geom_col(fill = "#EF9F4E", width = 0.65) +
  geom_text(
    aes(label = scales::percent(pct_dio_seno, accuracy = 1)),
    vjust = -0.25,
    size = 2.6
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.02),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = "Porcentaje (%)",
    caption = caption_txt
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(colour = "black", size = 8),
    axis.text.x = element_text(colour = "black", size = 6.5, angle = 25, margin = margin(t = 10)),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 0.5, colour = "black"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 4)),
    axis.ticks.x = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(6, 36, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.10)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)
