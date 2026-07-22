# ============================================================
# plot_enemdu_horas_sector_informal_linea.R
# Evolución de las horas promedio trabajadas por sexo en el
# sector informal, 2018-2026.
# Requiere: data/processed/enemdu_horas_sector_lineas_2018_2026.rds
# Guarda:   outputs/figures/33_b_horas-promedio_sector-informal-ecuador.png
# ============================================================
# Ejecutar desde la raiz del proyecto:
#   Rscript scripts/plots/plot_enemdu_horas_sector_informal_linea.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg"))

data_path <- "data/processed/enemdu_horas_sector_lineas_2018_2026.rds"
out_path <- "outputs/figures/33_b_horas-promedio_sector-informal-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_enemdu_horas_sector_lineas.R")
}

chart_data <- readRDS(data_path)
plot_df <- chart_data$annual_series |>
  dplyr::filter(
    sector_desc == "Sector Informal",
    anio >= 2018,
    anio <= 2025
  )

last_anio <- max(plot_df$anio)
label_df <- plot_df |>
  dplyr::filter(anio == last_anio) |>
  dplyr::arrange(horas_promedio) |>
  dplyr::mutate(
    label = sexo,
    x_label = anio + 0.28,
    y_label = horas_promedio + dplyr::case_when(
      sexo == "Hombres" ~ 0.18,
      TRUE ~ -0.18
    )
  )

title_raw <- "La jornada laboral en el sector informal permanece muy por debajo de las 40 horas"
subtitle_raw <- "Horas semanales promedio trabajadas, por sexo, ENEMDU mensual de diciembre 2018-2025, sector informal"
caption_raw <- paste(
  "Fuente: ENEMDU - INEC. Cálculos de Eddie Tomalá adaptados para El Quantificador de Laboratorio LIDE.",
  "La serie muestra el promedio de horas trabajadas a diciembre de cada año para 2018-2025.",
  "La franja gris señala el periodo de la pandemia.",
  "Las omisiones de sector se imputan con la tenencia de seguridad social."
)

build_chart <- function() {
  ggplot(plot_df, aes(x = anio, y = horas_promedio, color = sexo)) +
    annotate(
      "rect",
      xmin = 2019.5,
      xmax = 2021.5,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey80",
      alpha = 0.35
    ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.7) +
    scale_color_manual(values = c("Hombres" = "#2D7DB3", "Mujeres" = "#d97729")) +
    scale_x_continuous(
      breaks = 2018:2025,
      expand = expansion(mult = c(0.02, 0.18))
    ) +
    scale_y_continuous(
      labels = label_number_intl(accuracy = 0.1),
      expand = expansion(mult = c(0.05, 0.08))
    ) +
    geom_text(
      data = label_df,
      aes(x = x_label, y = y_label, label = label, color = sexo),
      hjust = 0,
      size = 3,
      fontface = "bold",
      show.legend = FALSE
    ) +
    labs(
      title = wrap_title_house(title_raw),
      subtitle = wrap_subtitle_house(subtitle_raw),
      x = NULL,
      y = "Horas promedio semanales",
      caption = wrap_caption_house(caption_raw)
    ) +
    coord_cartesian(clip = "off") +
    theme_quantificador() +
    theme(
      axis.text = element_text(colour = "grey20", size = 8),
      axis.text.x = element_text(angle = 40, hjust = 1),
      legend.position = "none",
      panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
      plot.margin = margin(6, 54, 6, 16)
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

p_final <- add_logo(build_chart(), x = 0.88, y = 0.14)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
