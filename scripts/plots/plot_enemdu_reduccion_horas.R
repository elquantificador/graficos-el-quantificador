# ============================================================
# plot_enemdu_reduccion_horas.R
# Variación porcentual de las horas trabajadas promedio entre
# 2007 y 2026, por sector (formal/informal) y sexo.
# Requiere: data/processed/enemdu_horas_sector_2007_2026.rds
# Guarda:   outputs/figures/33_variacion-horas-trabajadas-sector-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_reduccion_horas.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "ggplot2", "scales", "ragg", "stringr"))

data_path <- "data/processed/enemdu_horas_sector_2007_2026.rds"
out_path <- "outputs/figures/33_variacion-horas-trabajadas-sector-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_enemdu_horas_sector.R")
}

chart_data <- readRDS(data_path)

plot_df <- chart_data |>
  filter(
    anio %in% c(2007, 2026),
    sector_desc %in% c("Sector Formal", "Sector Informal")
  ) |>
  pivot_wider(names_from = anio, values_from = horas_promedio, names_prefix = "anio_") |>
  mutate(
    variacion = (anio_2026 / anio_2007) - 1,
    sexo = factor(sexo, levels = c("Mujeres", "Hombres")),
    sector_desc = factor(sector_desc, levels = c("Sector Formal", "Sector Informal"))
  )

title_raw <- "Los ecuatorianos cada vez trabajan menos\na la semana"
subtitle_raw <- "Cambio en la jornada laboral promedio, 2007-2026"
caption_raw <- paste(
  "Fuente: ENEMDU - INEC, marzo 2026. Cálculos de Eddie Tomalá para El Quantificador de Laboratorio LIDE.",
  "Las barras hacia abajo indican la reducción porcentual de horas trabajadas entre 2007 y 2026, en población de 15 años o más."
)

title_txt <- title_raw
subtitle_txt <- wrap_subtitle_house(subtitle_raw)
caption_txt <- wrap_caption_house(caption_raw)

palette_fill <- c(
  "Mujeres" = "#d97729",
  "Hombres" = "#2D7DB3"
)

build_chart <- function() {
  ggplot(plot_df, aes(x = sector_desc, y = variacion, fill = sexo)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.6) +
    geom_col(position = position_dodge(width = 0.8), width = 0.65) +
    geom_text(
      aes(label = percent_intl(variacion, accuracy = 0.1)),
      position = position_dodge(width = 0.8),
      vjust = 1.4,
      size = 2.8,
      fontface = "bold",
      colour = "grey20"
    ) +
    scale_fill_manual(values = palette_fill) +
    scale_y_continuous(
      labels = label_percent_intl(accuracy = 1),
      expand = expansion(mult = c(0.15, 0.05))
    ) +
    scale_x_discrete(position = "top") +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL,
      y = "Variación porcentual",
      fill = NULL,
      caption = caption_txt
    ) +
    theme_quantificador() +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(colour = "grey20", size = 8, face = "bold", margin = margin(b = 4)),
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(size = 7.5, colour = "grey20"),
      legend.margin = margin(b = 2),
      legend.box.spacing = unit(2, "pt")
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

p_final <- add_logo(build_chart())
dest <- out_path

ggsave(
  filename = dest,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", dest)
