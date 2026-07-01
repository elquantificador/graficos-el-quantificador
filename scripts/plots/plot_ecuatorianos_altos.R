# ============================================================
# plot_ecuatorianos_altos.R
# Genera el gráfico de estatura e ingresos laborales por sexo.
# Requiere: data/processed/ecuatorianos_altos_ensanut_2018.rds
# Guarda:   outputs/figures/01_altura-ingresos_ensanut-2018.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_ecuatorianos_altos.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "stringr", "ragg"))

df <- readRDS("data/processed/ecuatorianos_altos_ensanut_2018.rds")

caption_txt <- "Fuente: ENSANUT 2018. Cálculos por el autor. Se considera individuos mestizos entre 40 y 50 años."
subtitle_txt <- "Un aumento de 1 cm se relaciona con aproximadamente 3% más ingresos para mujeres y 2% más para hombres"

portrait_path <- "outputs/figures/01_altura-ingresos_ensanut-2018.png"

build_chart <- function() {
  spec <- house_spec("portrait")
  subtitle_w <- 58
  caption_w  <- 64

  ggplot(df, aes(x = estatura, y = linc, color = sexo, linetype = sexo)) +
    geom_point(alpha = 0.25, size = 0.9, show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
    scale_color_manual(values = c("Hombre" = "#2F4B7C", "Mujer" = "#C44E52")) +
    scale_linetype_manual(values = c("Hombre" = "solid", "Mujer" = "dashed")) +
    labs(
      x = "Estatura (cm)",
      y = "Ingresos laborales (en logaritmo)",
      title = "¿Los ecuatorianos más altos ganan más?",
      subtitle = str_wrap(subtitle_txt, width = subtitle_w),
      caption = str_wrap(caption_txt, width = caption_w),
      color = "Sexo",
      linetype = "Sexo"
    ) +
    theme_quantificador() +
    theme(
      legend.position = "bottom",
      plot.margin = margin(12, 16, 16, 16)
    )
}

dir.create("outputs/figures", showWarnings = FALSE)

  spec <- house_spec("portrait")
  p_final <- house_apply_logo(build_chart(), "portrait", x = 0.88, y = 0.09)
  dest <- portrait_path
  ggsave(
    filename = dest,
    plot = p_final,
    width = spec$width, height = spec$height, units = "in",
    dpi = spec$dpi,
    device = ragg::agg_png,
    bg = "white"
  )
  message("Guardado: ", dest)

