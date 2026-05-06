# ============================================================
# plot_endi_lactancia_peso_boxplot.R
# Genera un boxplot exploratorio del peso infantil segun si la
# madre dio el seno al nacer.
# Requiere: data/processed/endi_r2_lactancia_ingreso_peso_scatter.rds
# Guarda:   figures/endi_lactancia_peso_boxplot.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "stringr"))

out_path <- "figures/endi_lactancia_peso_boxplot.png"
plot_df <- readRDS("data/processed/endi_r2_lactancia_ingreso_peso_scatter.rds") %>%
  mutate(
    breastfed_birth = factor(
      breastfed_birth,
      levels = c("No dio el seno al nacer", "Si dio el seno al nacer")
    )
  )

title_txt <- stringr::str_wrap(
  "El peso infantil luce bastante parecido entre quienes recibieron seno al nacer y quienes no",
  width = 50
)

subtitle_txt <- stringr::str_wrap(
  "Boxplot exploratorio del peso de ninos y ninas segun si la madre les dio el seno al nacer. ENDI Ronda 2 2023-2024",
  width = 60
)

caption_txt <- stringr::str_wrap(
  paste(
    "Fuente: INEC, Encuesta Nacional sobre Desnutricion Infantil (ENDI), Ronda 2 2023-2024.",
    "El peso del nino o nina corresponde al promedio de las mediciones disponibles de peso.",
    "Se muestran observaciones con datos no faltantes de peso y lactancia al nacer."
  ),
  width = 100
)

p_base <- ggplot(
  plot_df,
  aes(x = breastfed_birth, y = weight_kg, fill = breastfed_birth)
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.75,
    outlier.alpha = 0.45,
    outlier.size = 1.5
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.22,
    size = 1,
    colour = "grey35"
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = paste0(scales::number(after_stat(y), accuracy = 0.1, decimal.mark = ","), " kg")),
    vjust = -0.7,
    size = 2.6,
    colour = "black"
  ) +
  scale_fill_manual(
    values = c(
      "No dio el seno al nacer" = "#C44E52",
      "Si dio el seno al nacer" = "#2A9D8F"
    )
  ) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ","),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = "Peso del nino o nina (kg)",
    caption = caption_txt
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(colour = "black", size = 7.5),
    axis.text.x = element_text(colour = "black", size = 7),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), colour = "black"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.2, lineheight = 1.1, hjust = 0, margin = margin(t = 6)),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    plot.margin = margin(6, 30, 8, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.08, width = 0.09, height = 0.09)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4.8,
  height = 5.2,
  dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)
