# ============================================================
# plot_endi_anemia_quintil.R
# Genera el gráfico de prevalencia de anemia en niñas/os de
# 6 a 23 meses por quintil de bienestar.
# Requiere: data/processed/endi_r2_prev_anemia_quintil.rds
# Guarda:   outputs/figures/14_anemia-infantil_quintil-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_endi_anemia_quintil.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "scales", "ragg", "stringr"))

out_path <- "outputs/figures/14_anemia-infantil_quintil-ecuador.png"
plot_df <- readRDS("data/processed/endi_r2_prev_anemia_quintil.rds") %>%
  mutate(
    quintil_label = case_when(
      quintil == "Quintil 1" ~ "Quintil 1\n(más pobre)",
      quintil == "Quintil 5" ~ "Quintil 5\n(más rico)",
      TRUE ~ as.character(quintil)
    )
  )

portrait_path <- out_path

title_raw <- "La prevalencia de anemia es alta incluso en los hogares más ricos del Ecuador"
subtitle_raw <- "Prevalencia de anemia en niños y niñas de 6 a 23 meses, por quintil de ingreso, ENDI Ronda 2 2023-2024"
caption_raw <- paste(
  "Fuente: INEC, Encuesta Nacional sobre Desnutrición Infantil (ENDI), Ronda 2 2023-2024.",
  "Cálculos de Daniel Sánchez para El Quantificador de Laboratorio LIDE.",
  "Nota: Los quintiles dividen a la población en cinco grupos de 20%, ordenados por ingreso per cápita del hogar.",
  "El quintil 1 corresponde al 20% con menores ingresos y el quintil 5 al 20% con mayores ingresos.",
  "Las proporciones son ponderadas por el peso de muestra proporcionado por INEC."
)

build_chart <- function() {
  spec <- house_spec("portrait")
  # (caption a 5,5 pt con su propio ancho).
  title_w    <- 44
  subtitle_w <- 56
  caption_w  <- 105

  ggplot(plot_df, aes(x = factor(quintil_label, levels = quintil_label), y = prev_anemia)) +
    geom_col(fill = "#EF9F4E", width = 0.56) +
    geom_text(
      aes(label = percent_intl(prev_anemia, accuracy = 0.1)),
      vjust = -0.2,
      size = 2.6
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.45),
      breaks = seq(0, 0.45, 0.10),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = stringr::str_wrap(title_raw, width = title_w),
      subtitle = stringr::str_wrap(subtitle_raw, width = subtitle_w),
      x = NULL,
      y = "Prevalencia de anemia (%)",
      caption = stringr::str_wrap(caption_raw, width = caption_w)
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(colour = "black", size = 8),
      axis.text.x = element_text(colour = "black", size = 7, margin = margin(t = 12)),
      axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 0.5, colour = "black"),
      plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
      plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 2)),
      axis.ticks.x = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.margin = margin(6, 30, 6, 16),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid = element_blank()
    )
}

dir.create("outputs/figures", showWarnings = FALSE)

  spec <- house_spec("portrait")
  p_final <- house_apply_logo(build_chart(), "portrait", x = 0.89, y = 0.16)
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

