# ============================================================
# plot_enighur_gasolina_share_quintiles_years.R
# Genera el grafico de la participacion de la gasolina dentro del
# gasto monetario del hogar por quintil y encuesta.
# Requiere: data/processed/enighur_gasolina_share_quintiles_years.rds
# Guarda:   outputs/figures/31_b_gasolina-share_quintil-ingreso-2012-2025.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

input_path <- "data/processed/enighur_gasolina_share_quintiles_years.rds"
out_path <- "outputs/figures/31_b_gasolina-share_quintil-ingreso-2012-2025.png"

chart_data <- readRDS(input_path)
plot_df <- chart_data$summary |>
  dplyr::mutate(
    quintil_ingreso = factor(
      .data$quintil_ingreso,
      levels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
      labels = c("Q1 (mas pobre)", "Q2", "Q3", "Q4", "Q5 (mas rico)")
    ),
    etiqueta = percent_intl(.data$share_gasto_monetario, accuracy = 0.1)
  )

title_raw <- "El peso de la gasolina en el presupuesto del hogar cambio mucho entre 2012 y 2025"
subtitle_raw <- "Gasolina como porcentaje del gasto monetario del hogar, por quintil de ingreso, ENIGHUR 2011-2012 y 2024-2025"
caption_raw <- paste(
  "Fuente: ENIGHUR 2011-2012 y ENIGHUR 2024-2025, INEC. Elaboracion: Daniel Sanchez para El Quantificador.",
  "Nota: Las barras muestran la participacion de la gasolina dentro del gasto monetario total del hogar en cada quintil.",
  "En cada encuesta, los quintiles dividen a los hogares en cinco grupos de 20%, ordenados por ingreso monetario del hogar dentro de esa misma ronda. El quintil 1 corresponde al 20% con menores ingresos y el quintil 5 al 20% con mayores ingresos."
)

palette <- c(
  "ENIGHUR 2011-2012" = "#A7C7DC",
  "ENIGHUR 2024-2025" = "#1F618D"
)

dodge <- position_dodge(width = 0.72)

build_chart <- function() {
  ggplot(plot_df, aes(x = .data$share_gasto_monetario, y = .data$quintil_ingreso, fill = .data$encuesta)) +
    geom_col(position = dodge, width = 0.62, alpha = 0.94, colour = NA) +
    geom_text(
      aes(label = .data$etiqueta),
      position = dodge,
      hjust = -0.08,
      size = 2.9,
      colour = "grey20"
    ) +
    scale_fill_manual(values = palette) +
    scale_x_continuous(
      labels = label_percent_intl(accuracy = 1),
      breaks = seq(0, 0.09, by = 0.01),
      limits = c(0, 0.092),
      expand = expansion(mult = c(0, 0.18))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = wrap_title_house(title_raw),
      subtitle = wrap_subtitle_house(subtitle_raw),
      x = "Gasolina como porcentaje del gasto monetario del hogar",
      y = "Quintiles de ingreso",
      fill = NULL,
      caption = wrap_caption_house(caption_raw)
    ) +
    theme_quantificador() +
    theme(
      legend.position = "bottom",
      plot.margin = margin(6, 44, 6, 16),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")
p_final <- house_apply_logo(build_chart(), "portrait", x = 0.88, y = 0.12)

ggsave(
  filename = out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)


