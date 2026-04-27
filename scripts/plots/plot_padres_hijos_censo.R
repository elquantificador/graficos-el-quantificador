# ============================================================
# plot_padres_hijos_censo.R
# Genera el gráfico de convivencia con padres/abuelos a partir
# de los datos procesados del censo 2010 y 2022.
# Requiere: data/processed/padres_hijos_censo.rds
# Guarda:   figures/cohab_parents_ecuador_instagram.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_padres_hijos_censo.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("scales", "ragg"))

out_path <- "figures/cohab_parents_ecuador_instagram.png"
plot_df <- readRDS("data/processed/padres_hijos_censo.rds")

p_base <- ggplot(plot_df, aes(x = age_group, y = share, fill = factor(year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.65) +
  geom_text(
    aes(label = percent(share, accuracy = 1)),
    position = position_dodge(width = 0.85),
    vjust = -0.2,
    size = 2.4,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.06))
  ) +
  scale_fill_manual(values = c("2010" = "#00A1CB", "2022" = "#EF9F4E")) +
  labs(
    title = "¿Cada vez es más difícil independizarse?",
    subtitle = "La proporción de adultos jóvenes en Ecuador que vive con sus\npadres y abuelos* ha aumentado de 2010 a 2022",
    x = NULL,
    y = "Porcentaje de personas que viven con sus padres o abuelos*",
    fill = NULL,
    caption = paste(
      "Fuente: Censo de Población y Vivienda 2010 y 2022, archivo REDATAM.",
      "Nota: La proporción graficada considera individuos que reportan ser hijo/a, hijastro/a o nieto/a del\nrepresentante o jefe del hogar. No se incluyen personas que son padres, padrastros o abuelos\ndel representante, ni relaciones entre miembros del hogar que no sean el representante.",
      sep = "\n"
    )
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(colour = "black", size = 8),
    axis.text.x = element_text(colour = "black", size = 8),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 1, colour = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 8, colour = "black"),
    legend.key.size = grid::unit(0.35, "cm"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 6)),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(6, 36, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.20)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)
