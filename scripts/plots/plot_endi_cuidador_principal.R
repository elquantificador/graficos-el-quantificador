# ============================================================
# plot_endi_cuidador_principal.R
# Genera el gráfico del principal cuidador entre semana para
# niños y niñas en la ENDI R2.
# Requiere: data/processed/endi_r2_cuidador_principal.rds
# Guarda:   figures/endi_cuidador_principal.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_endi_cuidador_principal.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "scales", "ragg", "stringr"))

out_path <- "figures/endi_cuidador_principal.png"
plot_df <- readRDS("data/processed/endi_r2_cuidador_principal.rds") %>%
  mutate(
    cuidador_label = case_when(
      cuidador_principal == "Padre o abuelos" ~ "Padre o abuelos",
      cuidador_principal == "Centro de Desarrollo Infantil (CDI)" ~ "Centro de Desarrollo\nInfantil (CDI)",
      TRUE ~ as.character(cuidador_principal)
    ),
    cuidador_label = factor(
      cuidador_label,
      levels = c(
        "Madre",
        "Padre o abuelos",
        "Centro de Desarrollo\nInfantil (CDI)",
        "Otros"
      )
    )
  )

title_txt <- "La infancia ecuatoriana crece bajo el cuidado,\ncariño y dedicación de sus madres"

subtitle_txt <- "Proporción de niños y niñas menores de 5 años, por cuidador\nprincipal, ENDI Ronda 2 2023-2024"

caption_txt <- stringr::str_wrap(
  paste(
    "Fuente: INEC, Encuesta Nacional sobre Desnutrición Infantil (ENDI), Ronda 2 2023-2024.",
    "Elaborado por: Daniel Sánchez para El Quantificador del Laboratorio LIDE.",
    "Nota: El principal cuidador del niño de hasta 5 años es quien permanece con el niño la mayor parte del tiempo de lunes a viernes.",
    "La categoría Otros agrupa tíos/tías, otros miembros del hogar, otros familiares, vecinos, amigos, empleada o niñera y la opción 'se queda solo'.",
    "Las proporciones son ponderadas por el factor de expansión de la encuesta."
  ),
  width = 92
)

p_base <- ggplot(
  plot_df,
  aes(x = cuidador_label, y = share)
) +
  geom_col(fill = "#EF9F4E", width = 0.56) +
  geom_text(
    aes(label = scales::percent(share, accuracy = 0.1)),
    hjust = -0.10,
    size = 2.6
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.85),
    breaks = seq(0, 0.8, by = 0.2),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_x_discrete(limits = rev(levels(plot_df$cuidador_label))) +
  coord_flip(clip = "off") +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = NULL,
    caption = caption_txt
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(colour = "black", size = 8),
    axis.text.x = element_text(colour = "black", size = 7),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.8, lineheight = 1.1, hjust = 0, margin = margin(t = 2)),
    axis.line.y = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "black"),
    plot.margin = margin(6, 32, 6, 24),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.895, y = 0.13, width = 0.09, height = 0.09)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)
