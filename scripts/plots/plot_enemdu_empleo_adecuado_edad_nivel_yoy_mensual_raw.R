# ============================================================
# plot_enemdu_empleo_adecuado_edad_nivel_yoy_mensual_raw.R
# Genera un gráfico de la variación interanual porcentual en el
# nivel de empleo adecuado por grupo de edad (sin suavizado).
# Requiere: data/processed/enemdu_empleo_adecuado_edad_yoy.rds
#           (generado por clean_enemdu_empleo_adecuado_edad.R)
# Guarda:   outputs/figures/13_empleo-adecuado_edad_yoy_mar-2026.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "stringr"))

out_path <- "outputs/figures/13_empleo-adecuado_edad_yoy_mar-2026.png"

plot_df <- readRDS("data/processed/enemdu_empleo_adecuado_edad_yoy.rds") %>%
  filter(fecha >= as.Date("2025-03-01"))

plot_df <- plot_df %>%
  mutate(grupo_edad = ifelse(grupo_edad == "Todas las edades", "Total", grupo_edad))

headline_yoy <- plot_df %>%
  filter(grupo_edad == "Total", fecha == max(fecha)) %>%
  pull(yoy_pct) %>%
  round(1)

title_raw <- "Se desploma el empleo adecuado en marzo 2026, impactando a los más jóvenes"
subtitle_raw <- "Variación interanual del nivel de empleo adecuado por grupo de edad, marzo de 2025 a marzo de 2026"
caption_portrait <- paste0(
  "Fuente: INEC, Encuesta Nacional de Empleo, Desempleo y Subempleo, tabulados marzo\n",
  "2026. Cálculos de Daniel Sánchez para El Quantificador de Laboratorio LIDE.\n",
  "Nota: En marzo de 2026, el empleo adecuado cayó ", abs(headline_yoy), "% respecto a marzo de 2025.\n",
  "El empleo adecuado comprende a las personas ocupadas que trabajan al menos la jornada\n",
  "laboral legal y perciben ingresos laborales iguales o superiores al salario mínimo."
)
caption_raw <- paste0(
  "Fuente: INEC, Encuesta Nacional de Empleo, Desempleo y Subempleo, tabulados marzo 2026. ",
  "Cálculos de Daniel Sánchez para El Quantificador de Laboratorio LIDE. ",
  "Nota: En marzo de 2026, el empleo adecuado cayó ", abs(headline_yoy), "% respecto a marzo de 2025. ",
  "El empleo adecuado comprende a las personas ocupadas que trabajan al menos la jornada ",
  "laboral legal y perciben ingresos laborales iguales o superiores al salario mínimo."
)

palette <- c(
  "Total"  = "#D04A3E",
  "15-24"  = "#00A8CB",
  "25-44"  = "#F0A145",
  "45-64"  = "#7B8D97"
)

meses_es <- c(
  "ene", "feb", "mar", "abr", "may", "jun",
  "jul", "ago", "sep", "oct", "nov", "dic"
)

last_fecha <- max(plot_df$fecha)
label_df <- plot_df %>%
  filter(fecha == last_fecha) %>%
  arrange(yoy_pct) %>%
  mutate(rank = row_number()) %>%
  mutate(
    label   = grupo_edad,
    x_label = fecha + 16,
    y_label = yoy_pct + case_when(
      rank == 1 ~ -1.5,
      rank == 2 ~ -1.0,
      rank == 3 ~  0.8,
      TRUE      ~  1.5
    )
  )

build_chart <- function(orientation) {
  spec <- house_spec(orientation)
  title_txt    <- stringr::str_wrap(title_raw, width = if (orientation == "landscape") spec$title_wrap else 44)
  subtitle_txt <- stringr::str_wrap(subtitle_raw, width = if (orientation == "landscape") spec$subtitle_wrap else 67)
  caption_txt  <- if (orientation == "landscape") stringr::str_wrap(caption_raw, width = spec$caption_wrap) else caption_portrait

  ggplot(
    plot_df,
    aes(x = fecha, y = yoy_pct, color = grupo_edad, group = grupo_edad)
  ) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_text(
    data = label_df,
    aes(x = x_label, y = y_label, label = label, color = grupo_edad),
    hjust = 0,
    size = 3.2,
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
  theme_quantificador(orientation) +
  theme(
    axis.text = element_text(colour = "black", size = 8),
    axis.text.x = element_text(angle = 40, hjust = 1),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 0.5, colour = "black"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 6.5, lineheight = 1.1, hjust = 0, margin = margin(t = 10)),
    legend.position = "none",
    plot.margin = if (orientation == "landscape") margin(6, 16, 6, 16) else margin(6, 30, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )
}

dir.create("outputs/figures", showWarnings = FALSE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  p_final <- house_apply_logo(build_chart(orientation), orientation, x = 0.89, y = 0.17)
  dest <- house_out_path(out_path, orientation)
  ggsave(
    filename = dest,
    plot = p_final,
    width = spec$width,
    height = spec$height,
    units = "in",
    dpi = spec$dpi,
    device = ragg::agg_png,
    bg = "white"
  )
  message("Guardado: ", dest)
}

