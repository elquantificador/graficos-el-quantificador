# ============================================================
# plot_endi_desnutricion.R
# Genera el gráfico de prevalencia de desnutrición crónica
# por etnia a partir de los datos procesados de la ENDI R2.
# Requiere: data/processed/endi_r2_prev_dcronica_etnia.rds
# Guarda:   outputs/figures/07_desnutricion-cronica_etnia-ecuador.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("forcats", "scales", "ragg"))

out_path <- "outputs/figures/07_desnutricion-cronica_etnia-ecuador.png"
plot_df <- readRDS("data/processed/endi_r2_prev_dcronica_etnia.rds")

portrait_path <- out_path

title_raw <- "Casi uno de cada tres niños indígenas padece de desnutrición en Ecuador"
subtitle_raw <- "Prevalencia de desnutrición crónica por etnia, niños y niñas en Ecuador (Ronda 2 ENDI 2023-2024)"
caption_raw <- "Fuente: Encuesta Nacional de Desnutrición Infantil, Ronda 2 2023-2024. Cálculos propios. Proporciones son ponderadas de acuerdo a pesos muestrales. La desnutrición presentada es para niños menores de 2 años."

build_chart <- function() {
  spec <- house_spec("portrait")
    title_txt    <- "Casi uno de cada tres niños indígenas\npadece de desnutrición en Ecuador"
    subtitle_txt <- "Prevalencia de desnutrición crónica por etnia, niños y niñas\nen Ecuador (Ronda 2 ENDI 2023-2024)"
    caption_txt  <- "Fuente: Encuesta Nacional de Desnutrición Infantil, Ronda 2 2023-2024. Cálculos propios. Proporciones son\nponderadas de acuerdo a pesos muestrales. La desnutrición presentada es para niños menores de 2 años."

  ggplot(plot_df, aes(x = fct_reorder(etnia, prev_dcronica), y = prev_dcronica)) +
    geom_col(fill = "#EF9F4E", width = 0.65) +
    geom_text(
      aes(label = percent_intl(prev_dcronica, accuracy = 1)),
      vjust = -0.2,
      size = 2.6
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.06))
    ) +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL,
      y = "Porcentaje con desnutrición crónica (%)",
      caption = caption_txt
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(colour = "black", size = 8),
      axis.text.x = element_text(colour = "black", size = 6, angle = 25, margin = margin(t = 10)),
      axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 0.5, colour = "black"),
      plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
      plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 4)),
      axis.ticks.x = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.margin = margin(6, 36, 6, 16),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid = element_blank()
    )
}

dir.create("outputs/figures", showWarnings = FALSE)

  spec <- house_spec("portrait")
  p_final <- house_apply_logo(build_chart(), "portrait", x = 0.88, y = 0.10)
  dest <- portrait_path
  ggsave(
    filename = dest,
    plot = p_final,
    width = spec$width,
    height = spec$height,
    dpi = spec$dpi,
    device = ragg::agg_png,
    bg = "white"
  )
  message("Guardado: ", dest)

