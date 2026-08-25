# ============================================================
# plot_wdi_remesas_origin.R
# Renderiza el panel 40_b: participación de Estados Unidos en las
# remesas recibidas por Ecuador durante 2025.
# Requiere: data/processed/remesas_regional_world_bank_bce.rds
# Guarda:   outputs/figures/40_b_remesas-estados-unidos.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/remesas_regional_world_bank_bce.rds"
out_path <- "outputs/figures/40_b_remesas-estados-unidos.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_remesas_regional.R")
}

processed <- readRDS(data_path)
plot_df <- processed$bce_origin_2025 |>
  dplyr::mutate(
    pais_procedencia = factor(
      pais_procedencia,
      levels = rev(c(
        "Estados Unidos",
        "España",
        "Italia",
        "Resto del mundo"
      ))
    ),
    etiqueta = paste0(
      label_number_intl(accuracy = 0.1)(porcentaje),
      "%"
    )
  )

if (nrow(plot_df) != 4 ||
    any(!is.finite(plot_df$porcentaje)) ||
    abs(plot_df$porcentaje[plot_df$pais_procedencia == "Estados Unidos"] - 77.8) > 0.1) {
  stop("La composición de remesas por origen no coincide con el boletín del BCE")
}

palette_color <- c(
  "Estados Unidos" = "#D97729",
  "España" = "#2D7DB3",
  "Italia" = "#55A868",
  "Resto del mundo" = "grey55"
)

title_raw <- "El 77,8% de las remesas proviene de Estados Unidos"
subtitle_raw <- "Distribución de las remesas recibidas por país de procedencia, Ecuador, 2025"
caption_raw <- paste(
  "Fuente: Banco Central del Ecuador, Boletín Analítico de la Evolución Anual de Remesas 2025, Figura 3.",
  "Elaboración: Carlos Israel Jiménez; adaptación y verificación de El Quantificador.",
  "Nota: Los porcentajes se calculan con los montos publicados por el BCE y se redondean a una decimal."
)

p_base <- ggplot(
  plot_df,
  aes(x = porcentaje, y = pais_procedencia, fill = pais_procedencia)
) +
  geom_col(width = 0.64) +
  geom_text(
    aes(
      x = porcentaje + 1.5,
      label = etiqueta,
      colour = pais_procedencia
    ),
    hjust = 0,
    size = 3.1,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = palette_color) +
  scale_colour_manual(values = palette_color) +
  scale_y_discrete(
    labels = c(
      "Estados Unidos" = "Estados Unidos",
      "España" = paste0("Espa", intToUtf8(c(110, 771)), "a"),
      "Italia" = "Italia",
      "Resto del mundo" = "Resto del mundo"
    )
  ) +
  scale_x_continuous(
    name = NULL,
    limits = c(0, 90),
    breaks = seq(0, 80, by = 20),
    labels = label_number_intl(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  coord_cartesian(clip = "off") +
  theme_quantificador() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8.5, colour = "grey20"),
    panel.grid.major.x = element_line(colour = "grey90", linetype = "dashed"),
    plot.margin = margin(6, 36, 6, 16)
  )

spec <- house_spec("portrait")
p_final <- house_apply_logo(p_base, x = 0.80, y = 0.16)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(
  filename = out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
