# ============================================================
# plot_wdi_remesas_merged.R
# Renderiza el panel 40_c, que combina los paneles B, D y E:
# remesas como porcentaje del PIB en el eje principal y remesas
# absolutas en millones de USD en el eje secundario.
# Requiere: data/processed/remesas_regional_world_bank_bce.rds
# Guarda:   outputs/figures/40_c_evolucion-remesas-ecuador.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/remesas_regional_world_bank_bce.rds"
out_path <- "outputs/figures/40_c_evolucion-remesas-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_remesas_regional.R")
}

processed <- readRDS(data_path)
plot_df <- processed$ecuador_remesas_pib |>
  dplyr::filter(anio >= 2000, anio <= 2025)

if (nrow(plot_df) < 2 || any(!is.finite(plot_df$remesas_pct_pib))) {
  stop("La tabla ecuador_remesas_pib no tiene datos válidos para el panel 40_c")
}

scale_factor <- max(plot_df$remesas_millones_usd, na.rm = TRUE) /
  max(plot_df$remesas_pct_pib, na.rm = TRUE)

plot_df <- plot_df |>
  dplyr::mutate(
    monto_escalado = remesas_millones_usd / scale_factor
  )

last_row <- plot_df |>
  dplyr::filter(anio == max(anio))

label_df <- tibble::tibble(
  anio = last_row$anio - c(7.8, 7.8),
  valor = c(
    last_row$remesas_pct_pib + 0.35,
    last_row$monto_escalado - 0.50
  ),
  label = c(
    paste0(
      label_number_intl(accuracy = 0.1)(last_row$remesas_pct_pib),
      "% PIB"
    ),
    paste0(
      "USD ",
      label_number_intl(accuracy = 1)(last_row$remesas_millones_usd),
      ""
    )
  ),
  serie = c("% del PIB", "Monto absoluto")
)

long_df <- dplyr::bind_rows(
  plot_df |>
    dplyr::transmute(
      anio,
      valor = remesas_pct_pib,
      serie = "% del PIB"
    ),
  plot_df |>
    dplyr::transmute(
      anio,
      valor = monto_escalado,
      serie = "Monto absoluto"
    )
)

palette_color <- c(
  "% del PIB" = "#2D7DB3",
  "Monto absoluto" = "#D97729"
)

title_raw <- "Las remesas se dispararon post-pandemia, acercándose al nivel de la crisis del 99"
subtitle_raw <- "Evolución de remesas recibidas, millones de USD y % del PIB, 2000-2025"
caption_raw <- paste(
  "Fuente: Banco Mundial, indicadores BX.TRF.PWKR.CD.DT y NY.GDP.MKTP.CD, descargados con el paquete R WDI.",
  "Elaboración: Carlos Israel Jiménez; adaptación y verificación de El Quantificador.",
  "Nota: El eje izquierdo muestra remesas como porcentaje del PIB; el eje derecho muestra el monto absoluto en millones de USD corrientes. El PIB es nominal."
)

p_base <- ggplot(long_df, aes(x = anio, y = valor, color = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_text(
    data = label_df,
    aes(x = anio, y = valor, label = label, color = serie),
    inherit.aes = FALSE,
    hjust = 0,
    size = 2.8,
    fontface = "bold"
  ) +
  scale_color_manual(values = palette_color) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020, 2025),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name = "% del PIB",
    labels = label_number_intl(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0.02, 0.12)),
    sec.axis = ggplot2::sec_axis(
      ~ . * scale_factor,
      name = "Millones de USD",
      labels = label_number_intl(accuracy = 1000)
    )
  ) +
  labs(
    title = wrap_title_house(title_raw, width = 46),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  coord_cartesian(xlim = c(2000, 2025.35), clip = "off") +
  theme_quantificador() +
  theme(
    legend.position = "none",
    axis.title.y.left = element_text(colour = palette_color[["% del PIB"]]),
    axis.text.y.left = element_text(colour = palette_color[["% del PIB"]]),
    axis.title.y.right = element_text(
      colour = palette_color[["Monto absoluto"]],
      angle = 90,
      hjust = 0.5,
      vjust = 0.5
    ),
    axis.text.y.right = element_text(colour = palette_color[["Monto absoluto"]]),
    panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
    plot.margin = margin(6, 24, 6, 12)
  )

spec <- house_spec("portrait")
p_final <- house_apply_logo(
  p_base,
  x = 0.90,
  y = 0.12,
  width = 0.08,
  height = 0.08
)

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
