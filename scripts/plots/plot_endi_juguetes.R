# ============================================================
# plot_endi_juguetes.R
# Genera el gráfico sobre con qué juguetes o elementos juegan
# los niños y niñas en la ENDI R2.
# Requiere: data/processed/endi_r2_juguetes.rds
# Guarda:   outputs/figures/19_juguetes-infancia-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_endi_juguetes.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "stringr"))

out_path <- "outputs/figures/19_juguetes-infancia-ecuador.png"
plot_df <- readRDS("data/processed/endi_r2_juguetes.rds")

share_comprados <- plot_df$share[plot_df$variable == "f3_s1_100_c"]
plot_df <- plot_df %>%
  filter(variable != "f3_s1_100_c")

title_txt <- paste0(
  "¿Con qué juegan los niños y niñas en Ecuador?"
)

subtitle_txt <- "Porcentaje de niños menores a 5 años, por tipo de juguete, ENDI R2"

caption_line1 <- "Fuente: INEC, Encuesta Nacional sobre Desnutrición Infantil (ENDI), Ronda 2 2023-2024."

caption_line2 <- "Elaborado por Alonso Quijano Ruiz y Daniel Sánchez para El Quantificador de Laboratorio"

caption_line3 <- paste0(
  "LIDE. El ",
  percent_intl(share_comprados, accuracy = 0.1),
  " juega con juguetes comprados en un almacén o mercado. ",
  "Los porcentajes suman más de 100% porque un mismo niñ@ puede jugar con varios tipos de juguetes."
)

caption_txt <- paste(
  caption_line1,
  caption_line2,
  stringr::str_wrap(caption_line3, width = 86),
  sep = "\n"
)
caption_raw <- paste(caption_line1, caption_line2, caption_line3)

build_chart <- function(orientation) {
  spec <- house_spec(orientation)
  caption_use <- if (orientation == "landscape") {
    stringr::str_wrap(caption_raw, width = landscape_wrap_for_size(6.8))
  } else {
    caption_txt
  }

  ggplot(
    plot_df,
    aes(x = toy_type, y = share, fill = highlight)
  ) +
    geom_col(width = 0.60, show.legend = FALSE) +
    geom_text(
      aes(label = percent_intl(share, accuracy = 0.1)),
      hjust = -0.10,
      size = 2.6
    ) +
    scale_fill_manual(values = c(`TRUE` = "#EF9F4E", `FALSE` = "#8CC0C6")) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.8),
      breaks = seq(0, 0.8, by = 0.2),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_flip(clip = "off") +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL,
      y = NULL,
      caption = caption_use
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(colour = "black", size = 6.9, lineheight = 0.92),
      axis.text.x = element_text(colour = "black", size = 7),
      plot.title = element_text(colour = "black", size = 12.2, face = "bold", hjust = 0, lineheight = 1.0),
      plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption = element_text(colour = "black", size = 6.8, lineheight = 1.03, hjust = 0, margin = margin(t = 3)),
      axis.line.y = element_line(colour = "black"),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(colour = "black"),
      plot.margin = if (orientation == "landscape") margin(3, 16, 3, 3) else margin(3, 20, 3, 3),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid = element_blank()
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  p_final <- house_apply_logo(build_chart(orientation), orientation, x = 0.905, y = 0.14, width = 0.09, height = 0.09)
  dest <- house_out_path(out_path, orientation)
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
}
