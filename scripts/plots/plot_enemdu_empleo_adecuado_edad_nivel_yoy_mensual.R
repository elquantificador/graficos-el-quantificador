# ============================================================
# plot_enemdu_empleo_adecuado_edad_nivel_yoy_mensual.R
# Genera un gráfico de la variación interanual porcentual en el
# nivel de empleo adecuado por grupo de edad.
# Requiere: data/processed/enemdu_empleo_adecuado_edad_yoy.rds
#           (generado por clean_enemdu_empleo_adecuado_edad.R)
# Guarda:   figures/empleo_adecuado_grupo_edad_nivel_yoy_mensual.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "stringr"))

out_path <- "figures/empleo_adecuado_grupo_edad_nivel_yoy_mensual.png"

plot_df <- readRDS("data/processed/enemdu_empleo_adecuado_edad_yoy.rds") %>%
  filter(fecha >= as.Date("2025-03-01"))

title_txt <- stringr::str_wrap(
  "Se desploma el empleo adecuado en marzo 2026, impactando a los más jóvenes",
  width = 44
)

subtitle_txt <- stringr::str_wrap(
  "Variación interanual del nivel de empleo adecuado por grupo de edad, marzo de 2025 a marzo de 2026",
  width = 67
)

caption_txt <- paste0(
  "Fuente: INEC, Encuesta Nacional de Empleo, Desempleo y Subempleo, tabulados marzo\n",
  "2026. Cálculos de Daniel Sánchez para El Quantificador de Laboratorio LIDE. Nota: El\n",
  "empleo adecuado comprende a las personas ocupadas que trabajan al menos la jornada\n",
  "laboral legal y perciben ingresos laborales iguales o superiores al salario mínimo.\n",
  "Las líneas muestran suavizado LOESS."
)

palette <- c(
  "Todas las edades" = "#D04A3E",
  "15-24" = "#00A8CB",
  "25-44" = "#F0A145",
  "45-64" = "#7B8D97"
)

meses_es <- c(
  "ene", "feb", "mar", "abr", "may", "jun",
  "jul", "ago", "sep", "oct", "nov", "dic"
)

last_fecha <- max(plot_df$fecha)
loess_ends <- plot_df %>%
  group_by(grupo_edad) %>%
  summarise(
    yoy_smooth = predict(loess(yoy_pct ~ as.numeric(fecha), span = 0.5),
                         newdata = data.frame(fecha = as.numeric(last_fecha))),
    .groups = "drop"
  ) %>%
  arrange(yoy_smooth) %>%
  mutate(rank = row_number())

label_df <- plot_df %>%
  group_by(grupo_edad) %>%
  filter(fecha == last_fecha) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  left_join(loess_ends, by = "grupo_edad") %>%
  mutate(
    label = grupo_edad,
    x_label = fecha + 16,
    y_label = yoy_smooth + case_when(
      rank == 1 ~ -1.5,
      rank == 2 ~ -1.0,
      rank == 3 ~  0.8,
      TRUE      ~  1.5
    )
  )

p_base <- ggplot(
  plot_df,
  aes(x = fecha, y = yoy_pct, color = grupo_edad, group = grupo_edad)
) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.4) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 1, span = 0.5) +
  geom_text(
    data = label_df,
    aes(x = x_label, y = y_label, label = label, color = grupo_edad),
    hjust = 0,
    size = 2,
    fontface = "bold",
    lineheight = 1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = palette) +
  scale_x_date(
    breaks = seq(as.Date("2025-03-01"), as.Date("2026-03-01"), by = "3 months"),
    minor_breaks = seq(as.Date("2025-03-01"), as.Date("2026-03-01"), by = "1 month"),
    labels = function(x) {
      mes <- meses_es[as.integer(format(x, "%m"))]
      paste0(toupper(substr(mes, 1, 1)), substr(mes, 2, 3), "-", substr(format(x, "%Y"), 3, 4))
    },
    expand = expansion(mult = c(0.02, 0.14))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(scales::number(x, accuracy = 1, decimal.mark = ","), "%"),
    breaks = seq(-20, 30, 10),
    minor_breaks = seq(-25, 35, 5),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = "Variación interanual del empleo adecuado (%)",
    color = NULL,
    caption = caption_txt
  ) +
  coord_cartesian(ylim = c(-25, 35), clip = "off") +
  guides(
    x = guide_axis(minor.ticks = TRUE),
    y = guide_axis(minor.ticks = TRUE)
  ) +
  theme_quantificador() +
  theme(
    axis.text = element_text(colour = "black", size = 8),
    axis.text.x = element_text(angle = 40, hjust = 1),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 0.5, colour = "black"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 6.5, lineheight = 1.1, hjust = 0, margin = margin(t = 10)),
    legend.position = "none",
    plot.margin = margin(6, 30, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.89, y = 0.17)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  device = ragg::agg_png
)

message("Guardado: ", out_path)
