# ============================================================
# plot_femicidios.R
# Genera el gráfico de femicidios y otras muertes de mujeres
# por año (Fiscalía General del Estado, 2014–2025).
# Requiere: data/processed/femicidios.rds
# Guarda:   outputs/figures/05_femicidios_contexto-delictivo-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_femicidios.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "stringr", "ragg"))

muertes_fem <- readRDS("data/processed/femicidios.rds")

purple_women  <- "#88398a"
purple_women2 <- "#52307c"

portrait_path <- "outputs/figures/05_femicidios_contexto-delictivo-ecuador.png"

caption_grafo1 <- paste0(
  "Fuente: Fiscalía General del Estado. Femicidios corresponden al delito de femicidio según el art. 141 del Código Orgánico",
  "\n",
  str_wrap("Integral Penal. Las cifras de otras muertes incluyen asesinatos, homicidios intencionales, sicariatos, robos, ejecuciones extrajudiciales, entre otros. Elaboración por los autores.", 121)
)
title_raw <- "El gobierno ecuatoriano no conoce con exactitud cuantas mujeres mueren por femicidio cada año"
subtitle_raw <- "Los femicidios en Ecuador han caído en los últimos años, pero las mujeres siguen muriendo en contexto delictivo"
caption_raw <- paste(
  "Fuente: Fiscalía General del Estado. Femicidios corresponden al delito de femicidio según el art. 141 del Código Orgánico Integral Penal.",
  "Las cifras de otras muertes incluyen asesinatos, homicidios intencionales, sicariatos, robos, ejecuciones extrajudiciales, entre otros.",
  "Elaboración por los autores."
)

build_chart <- function() {
  spec <- house_spec("portrait")
    title_txt    <- "El gobierno ecuatoriano no conoce con exactitud\ncuantas mujeres mueren por femicidio cada año"
    subtitle_txt <- "Los femicidios en Ecuador han caído en los últimos años, pero las mujeres\nsiguen muriendo en contexto delictivo"
    caption_txt  <- caption_grafo1

  ggplot(muertes_fem, aes(x = as.character(año), y = cantidad, fill = tipo)) +
    geom_col(width = 0.7, position = "stack", color = "black") +
    labs(
      x        = "",
      y        = "Número de muertes",
      title    = title_txt,
      subtitle = subtitle_txt,
      fill     = "Tipo de muerte",
      caption  = caption_txt
    ) +
    scale_fill_manual(
      values = c("Femicidios" = purple_women, "Otras muertes" = purple_women2),
      limits = c("Femicidios", "Otras muertes")
    ) +
    scale_y_continuous(breaks = seq(0, 800, 100), limits = c(0, 800), expand = c(0, 0)) +
    geom_text(aes(label = cantidad),
              position = position_stack(vjust = 0.5),
              color = "white", size = 2.5, fontface = "bold") +
    theme_women() +
    theme(
      legend.position    = "bottom",
      legend.text        = element_text(size = 6, color = "black"),
      legend.title       = element_text(size = 6, color = "black"),
      axis.text.y        = element_text(size = 6, color = "black"),
      axis.text.x        = element_text(size = 6, color = "black"),
      axis.ticks.y       = element_line(color = "black", linewidth = 0.3),
      legend.box.spacing = unit(2, "pt"),
      plot.margin        = margin(14, 36, 4, 16)
    )
}

dir.create("outputs/figures", showWarnings = FALSE)

  spec <- house_spec("portrait")
  p_final <- house_apply_logo(build_chart(), "portrait", x = 0.90, y = 0.07)
  dest <- portrait_path
  ggsave(dest, p_final, width = spec$width, height = spec$height, units = "in",
         dpi = spec$dpi, device = ragg::agg_png, bg = "white")
  message("Guardado: ", dest)

