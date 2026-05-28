# ============================================================
# plot_ras_personal_salud.R
# Genera el grafico de la evolucion del personal del sistema
# publico de salud en Ecuador a partir de la serie RAS.
# Requiere: data/processed/ras_personal_salud_nacional.rds
# Guarda:   figures/18_personal-salud-publica-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" scripts/plots/plot_ras_personal_salud.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

in_path <- "data/processed/ras_personal_salud_nacional.rds"
out_path <- "figures/18_personal-salud-publica-ecuador.png"

plot_df <- readRDS(in_path)
max_personal <- max(plot_df$total, na.rm = TRUE)

label_df <- plot_df %>%
  filter(!is.na(total)) %>%
  group_by(ocupacion) %>%
  filter(anio == max(anio, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    label = case_when(
      ocupacion == "Medicos" ~ "Médicos",
      ocupacion == "Enfermeros" ~ "Enfermeros",
      ocupacion == "Obstetrices" ~ "Obstetrices",
      ocupacion == "TAPS" ~ "TAPS"
    ),
    anio_label = case_when(
      ocupacion == "Obstetrices" ~ anio + 0.22,
      TRUE ~ anio + 0.4
    ),
    total_label = case_when(
      ocupacion == "Medicos" ~ total + 500,
      ocupacion == "Enfermeros" ~ total - 120,
      ocupacion == "Obstetrices" ~ total + 700,
      ocupacion == "TAPS" ~ total - 300,
      TRUE ~ total
    ),
    label_color = case_when(
      ocupacion == "Medicos" ~ "#D04A3E",
      ocupacion == "Enfermeros" ~ "#00A8CB",
      ocupacion == "Obstetrices" ~ "#F0A145",
      ocupacion == "TAPS" ~ "#7B8D97"
    )
  )

caption_txt <- paste0(
  "Fuente: Registro de Actividades y Recursos de Salud (RAS), 2006-2021. Elaboración: Odalis Clemente\n",
  "y Alonso Quijano Ruiz para el Quantificador de Laboratorio LIDE. TAPS: Técnicos de Atención Primaria\n",
  "en Salud, personal comunitario vinculado al primer nivel de atención."
)

p_base <- ggplot(plot_df, aes(x = anio, y = total, color = ocupacion)) +
  geom_vline(
    xintercept = c(2008, 2011),
    linetype = "dashed",
    colour = "grey60",
    linewidth = 0.4
  ) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_text(
    data = label_df,
    aes(x = anio_label, y = total_label, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 0.5,
    size = 3.4,
    fontface = "bold",
    lineheight = 1,
    color = label_df$label_color,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = 2008,
    y = max_personal * 0.87,
    label = "Nueva Constitución",
    angle = 90,
    vjust = -0.5,
    size = 3,
    colour = "black"
  ) +
  annotate(
    "text",
    x = 2011,
    y = max_personal * 0.87,
    label = "Reforma de Salud MAIS",
    angle = 90,
    vjust = -0.5,
    size = 3,
    colour = "black"
  ) +
  scale_x_continuous(
    breaks = seq(2006, 2021, by = 2),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  scale_y_continuous(
    labels = scales::label_comma(),
    breaks = seq(0, ceiling(max_personal / 2500) * 2500, by = 2500),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_color_manual(
    values = c(
      "Medicos" = "#D04A3E",
      "Enfermeros" = "#00A8CB",
      "Obstetrices" = "#F0A145",
      "TAPS" = "#7B8D97"
    )
  ) +
  labs(
    title = "El mayor crecimiento de personal de salud\nfue después de la reforma de salud de 2011",
    subtitle = "Evolución del personal del MSP en Ecuador, 2006-2021",
    x = NULL,
    y = "Número de profesionales",
    caption = caption_txt
  ) +
  coord_cartesian(clip = "off") +
  theme_quantificador() +
  theme(
    plot.subtitle = element_text(size = 11, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(size = 6.8, lineheight = 1.1, hjust = 0, margin = margin(t = 7)),
    axis.title.y = element_text(
      colour = "grey20",
      size = 8.5,
      hjust = 0.5,
      vjust = 0.5,
      margin = margin(r = 10, b = 8)
    ),
    plot.margin = margin(10, 48, 0, 12),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0, size = 14.5)
  )

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
p_final <- add_logo(p_base, x = 0.88, y = 0.07, width = 0.08, height = 0.08)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4.5,
  height = 5.5,
  units = "in",
  dpi = 300,
  device = ragg::agg_png
)

message("Guardado: ", out_path)
