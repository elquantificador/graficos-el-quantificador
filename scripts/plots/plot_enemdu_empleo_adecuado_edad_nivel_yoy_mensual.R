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
  "El empleo adecuado se deterioró sobre todo entre personas de 45 a 64 años en marzo de 2026",
  width = 44
)

subtitle_txt <- stringr::str_wrap(
  "Variación interanual del nivel de empleo adecuado por grupo de edad, marzo de 2025 a marzo de 2026",
  width = 50
)

caption_txt <- stringr::str_wrap(
  paste(
    "Fuente: INEC, Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU), tabulados de marzo de 2026.",
    "Cálculos de Daniel Sánchez para El Quantificador de Laboratorio LIDE.",
    "La serie muestra la variación porcentual del nivel de empleo adecuado frente al mismo mes del año previo.",
    "El grupo 25-44 agrega a personas de 25 a 34 y de 35 a 44 años."
  ),
  width = 85
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

label_df <- plot_df %>%
  group_by(grupo_edad) %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    label = grupo_edad,
    x_label = fecha + 16,
    y_label = case_when(
      grupo_edad == "15-24" ~ yoy_pct + 0.8,
      grupo_edad == "25-44" ~ yoy_pct - 1.2,
      grupo_edad == "45-64" ~ yoy_pct - 0.8,
      TRUE ~ yoy_pct + 0.2
    )
  )

p_base <- ggplot(
  plot_df,
  aes(x = fecha, y = yoy_pct, color = grupo_edad, group = grupo_edad)
) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.9) +
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
      paste0(meses_es[as.integer(format(x, "%m"))], "-", substr(format(x, "%Y"), 3, 4))
    },
    expand = expansion(mult = c(0.02, 0.14))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(scales::number(x, accuracy = 1, decimal.mark = ","), "%"),
    breaks = seq(-25, 30, 5),
    limits = c(-25, 31),
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
  coord_cartesian(clip = "off") +
  theme_quantificador() +
  theme(
    axis.text = element_text(colour = "black", size = 8),
    axis.text.x = element_text(angle = 40, hjust = 1),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 0.5, colour = "black"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 10)),
    legend.position = "none",
    plot.margin = margin(6, 30, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.89, y = 0.13)

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
