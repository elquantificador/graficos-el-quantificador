# ============================================================
# plot_enighur_gasolina_transporte_quintiles.R
# Genera el grafico del gasto mensual promedio en gasolina y
# transporte publico por quintil de ingreso del hogar.
# Requiere: data/processed/enighur_gasolina_transporte_quintiles_2025.rds
# Guarda:   outputs/figures/31_a_gasolina-vs-transporte-publico_quintil-ingreso-ecuador.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

input_path <- "data/processed/enighur_gasolina_transporte_quintiles_2025.rds"
out_path <- "outputs/figures/31_a_gasolina-vs-transporte-publico_quintil-ingreso-ecuador.png"

chart_data <- readRDS(input_path)
plot_df <- chart_data$summary |>
  dplyr::mutate(
    quintil_ingreso = factor(
      .data$quintil_ingreso,
      levels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
      labels = c("Q1 (más pobre)", "Q2", "Q3", "Q4", "Q5 (más rico)")
    ),
    etiqueta = paste0(
      "$", formatC(round(.data$gasto_promedio, 1), format = "f", digits = 1),
      " | ", percent_intl(.data$share_gasto_monetario, accuracy = 0.1)
    )
  )

title_raw <- "En los hogares pobres pesa más el transporte publico; en los ricos, la gasolina"
subtitle_raw <- "Gasto mensual promedio por hogar en gasolina y transporte público, por quintil de ingreso, ENIGHUR 2024-2025"
caption_raw <- paste(
  "Fuente: ENIGHUR 2024-2025, INEC. Elaboracion: Daniel Sánchez para El Quantificador.",
  "Nota: Las barras muestran el gasto mensual promedio por hogar en cada rubro. Las etiquetas indican el valor en dólares y su peso dentro del gasto monetario total del hogar.",
  "Los quintiles dividen a los hogares en cinco grupos de 20%, ordenados por ingreso monetario del hogar. El quintil 1 corresponde al 20% con menores ingresos y el quintil 5 al 20% con mayores ingresos."
)

palette <- c(
  "Gasolina" = "#1F618D",
  "Transporte publico" = "#6BB7C9"
)

dodge <- position_dodge(width = 0.72)

build_chart <- function() {
  ggplot(plot_df, aes(x = .data$gasto_promedio, y = .data$quintil_ingreso, fill = .data$rubro)) +
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
      labels = label_dollar_intl(accuracy = 1),
      breaks = seq(0, 60, by = 10),
      limits = c(0, 63),
      expand = expansion(mult = c(0, 0.18))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = wrap_title_house(title_raw),
      subtitle = wrap_subtitle_house(subtitle_raw),
      x = "Gasto mensual promedio por hogar",
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



