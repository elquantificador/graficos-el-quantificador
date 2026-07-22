# ============================================================
# plot_inec_proyecciones_edades.R
# Evolución de la composición etaria de la población ecuatoriana,
# proyectada de 1950 a 2050, por grandes grupos de edad.
# Requiere: data/processed/inec_proyecciones_edades.rds
# Guarda:   outputs/figures/composicion-edad-poblacion-2050-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_inec_proyecciones_edades.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "stringr"))

data_path <- "data/processed/inec_proyecciones_edades.rds"
out_path <- "outputs/figures/composicion-edad-poblacion-2050-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_inec_proyecciones_edades.R")
}

datos_composicion <- readRDS(data_path)

title_raw <- "La proyección a 2050 confirma menos niños y más jubilados en Ecuador"
subtitle_raw <- "Porcentaje de cada grupo etario respecto a la población total, 1950-2050"
caption_raw <- paste(
  "Fuente: INEC, Proyecciones poblacionales. Cálculos de Eddie Tomalá para El Quantificador de Laboratorio LIDE.",
  "Las trayectorias integran los registros históricos con las proyecciones del INEC hacia 2050."
)

title_txt <- wrap_title_house(title_raw)
subtitle_txt <- wrap_subtitle_house(subtitle_raw)
caption_txt <- wrap_caption_house(caption_raw)

palette_color <- c(
  "0-14 años" = "#7B8D97",
  "15-24 años" = "#00A8CB",
  "25-54 años" = "#d97729",
  "55-64 años" = "#2D7DB3",
  "65 años y más" = "#C44E52"
)

build_chart <- function() {
  ggplot(datos_composicion, aes(x = anio, y = porcentaje, color = grupo_edad)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = palette_color) +
    scale_y_continuous(
      labels = label_percent_intl(accuracy = 1),
      limits = c(0, max(datos_composicion$porcentaje) * 1.1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_continuous(
      breaks = seq(1950, 2050, by = 20),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL,
      y = "% de la población",
      color = NULL,
      caption = caption_txt
    ) +
    theme_quantificador() +
    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(size = 6.5, colour = "grey20"),
      legend.key.width = unit(0.7, "lines"),
      legend.margin = margin(t = 0, b = 4),
      panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed")
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
