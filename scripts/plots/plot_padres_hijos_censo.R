# ============================================================
# plot_padres_hijos_censo.R
# Genera el gráfico de convivencia con padres/abuelos a partir
# de los datos procesados del censo 2010 y 2022.
# Requiere: data/processed/padres_hijos_censo.rds
# Guarda:   outputs/figures/06_jovenes_viven-con-padres-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_padres_hijos_censo.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("scales", "ragg"))

out_path <- "outputs/figures/06_jovenes_viven-con-padres-ecuador.png"
plot_df <- readRDS("data/processed/padres_hijos_censo.rds")

portrait_path <- out_path

subtitle_raw <- "La proporción de adultos jóvenes en Ecuador que vive con sus padres y abuelos* ha aumentado de 2010 a 2022"
caption_raw <- paste(
  "Fuente: Censo de Población y Vivienda 2010 y 2022, archivo REDATAM.",
  "Nota: La proporción graficada considera individuos que reportan ser hijo/a, hijastro/a o nieto/a del representante o jefe del hogar. No se incluyen personas que son padres, padrastros o abuelos del representante, ni relaciones entre miembros del hogar que no sean el representante."
)

build_chart <- function(orientation) {
  spec <- house_spec(orientation)
  if (orientation == "landscape") {
    subtitle_txt <- stringr::str_wrap(subtitle_raw, width = spec$subtitle_wrap)
    caption_txt  <- stringr::str_wrap(caption_raw, width = landscape_wrap_for_size(5.5))
  } else {
    subtitle_txt <- "La proporción de adultos jóvenes en Ecuador que vive con sus\npadres y abuelos* ha aumentado de 2010 a 2022"
    caption_txt  <- paste(
      "Fuente: Censo de Población y Vivienda 2010 y 2022, archivo REDATAM.",
      "Nota: La proporción graficada considera individuos que reportan ser hijo/a, hijastro/a o nieto/a del\nrepresentante o jefe del hogar. No se incluyen personas que son padres, padrastros o abuelos\ndel representante, ni relaciones entre miembros del hogar que no sean el representante.",
      sep = "\n"
    )
  }

  ggplot(plot_df, aes(x = age_group, y = share, fill = factor(year))) +
    geom_col(position = position_dodge(width = 0.85), width = 0.65) +
    geom_text(
      aes(label = percent_intl(share, accuracy = 1)),
      position = position_dodge(width = 0.85),
      vjust = -0.2,
      size = 2.4,
      show.legend = FALSE
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.06))
    ) +
    scale_fill_manual(values = c("2010" = "#00A1CB", "2022" = "#EF9F4E")) +
    labs(
      title = "¿Cada vez es más difícil independizarse?",
      subtitle = subtitle_txt,
      x = NULL,
      y = "Porcentaje de personas que viven con sus padres o abuelos*",
      fill = NULL,
      caption = caption_txt
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(colour = "black", size = 8),
      axis.text.x = element_text(colour = "black", size = 8),
      axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 1, colour = "black"),
      legend.position = "bottom",
      legend.text = element_text(size = 8, colour = "black"),
      legend.key.size = grid::unit(0.35, "cm"),
      plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
      plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 6)),
      axis.line = element_line(colour = "black"),
      plot.margin = if (orientation == "landscape") margin(6, 16, 6, 16) else margin(6, 36, 6, 16),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid = element_blank()
    )
}

dir.create("outputs/figures", showWarnings = FALSE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  p_final <- house_apply_logo(build_chart(orientation), orientation, x = 0.88, y = 0.20)
  dest <- house_out_path(portrait_path, orientation)
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

