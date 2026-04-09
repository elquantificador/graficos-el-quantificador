# ============================================================
# plot_uso_tiempo.R
# Genera el grafico del tiempo semanal dedicado a cocinar
# por sexo usando la encuesta de uso del tiempo 2019.
# Requiere: data/processed/S51P2_UT2019_clean.rds
# Guarda:   figures/post_cocina_2019.png
# ============================================================
# Ejecutar desde la raiz del proyecto:
#   Rscript scripts/plots/plot_uso_tiempo.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "survey", "ragg"))

df <- readRDS("data/processed/S51P2_UT2019_clean.rds")

des <- svydesign(
  ids = ~upm,
  strata = ~estrato,
  weights = ~fexp,
  data = df,
  nest = TRUE
)

plot_df <- svyby(
  ~t_horas_cocina,
  ~sexo,
  des,
  svymean,
  na.rm = TRUE,
  vartype = "se",
  keep.names = FALSE
) %>%
  as.data.frame() %>%
  transmute(
    sexo = factor(sexo, levels = c("Hombre", "Mujer")),
    media = t_horas_cocina,
    media_se = se
  )

p_base <- ggplot(plot_df, aes(x = sexo, y = media, fill = sexo)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(media, 2), " horas")),
    size = 3,
    vjust = -0.8
  ) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +
  scale_y_continuous(
    breaks = 0:7,
    expand = expansion(mult = c(0, 0.06))
  ) +
  coord_cartesian(ylim = c(0, 7)) +
  labs(
    title = "La mujer ecuatoriana dedica 4 veces más\ntiempo a cocinar que el hombre",
    subtitle = "Promedio de horas semanales dedicadas a cocinar, por sexo,\nEncuesta Multipropósito 2019",
    x = NULL,
    y = "Horas semanales",
    caption = "Fuente: INEC, Encuesta Multipropósito 2019. Elaboracion: El Quantificador por Laboratorio LIDE."
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(colour = "black", size = 9),
    axis.text.x = element_text(colour = "black", size = 9),
    axis.title.y = element_text(size = 8, margin = margin(r = 6), hjust = 1, colour = "black"),
    plot.title = element_text(colour = "black", size = 13.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 6.2, lineheight = 1.1, hjust = 0, margin = margin(t = 6)),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(6, 36, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank(),
    axis.ticks = element_line(colour = "black")
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.05)
ggsave(
  "figures/post_cocina_2019.png",
  p_final,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  device = ragg::agg_png
)

message("Guardado: figures/post_cocina_2019.png")
