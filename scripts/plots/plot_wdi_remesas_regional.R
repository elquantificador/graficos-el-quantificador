# ============================================================
# plot_wdi_remesas_regional.R
# Renderiza la comparación regional del crecimiento de remesas recibidas.
# Requiere: data/processed/remesas_regional_world_bank_bce.rds
# Guarda:   outputs/figures/40_a_crecimiento-remesas-region-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_wdi_remesas_regional.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/remesas_regional_world_bank_bce.rds"
out_path <- "outputs/figures/40_a_crecimiento-remesas-region-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_remesas_regional.R")
}

processed <- readRDS(data_path)

plot_df <- processed$world_bank |>
  dplyr::filter(anio >= 2020, anio <= 2024) |>
  dplyr::group_by(pais) |>
  dplyr::arrange(anio, .by_group = TRUE) |>
  dplyr::mutate(
    indice_2020 = 100 * remesas_millones_usd / first(remesas_millones_usd)
  ) |>
  dplyr::ungroup()

growth_df <- plot_df |>
  dplyr::group_by(pais) |>
  dplyr::summarise(
    crecimiento = 100 * (last(indice_2020) / first(indice_2020) - 1),
    indice_final = last(indice_2020),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    pais_label = dplyr::recode(pais, Peru = "Per\u00FA"),
    label = paste0(pais_label, " ", percent_intl(crecimiento / 100, accuracy = 0.1)),
    x_label = 2024.12,
    y_label = indice_final + dplyr::case_when(
      pais == "Ecuador" ~ 2.5,
      pais == "Colombia" ~ 0,
      TRUE ~ -2.5
    )
  )

palette_color <- c(
  Ecuador = "#D97729",
  Colombia = "#2D7DB3",
  Peru = "#55A868"
)

title_raw <- "Ecuador tiene el crecimiento de remesas más acelerado de la región"
subtitle_raw <- paste0(
  "Índice 2020 = 100 de las remesas recibidas, Ecuador, Colombia y Perú, 2020-2024"
)
caption_raw <- paste(
  "Fuente: Banco Mundial, indicador BX.TRF.PWKR.CD.DT, descargado con el paquete R WDI.",
  "Elaboración: Carlos Israel Jiménez; adaptación y verificación de El Quantificador.",
  "Nota: El índice compara el crecimiento acumulado desde 2020 hasta 2024 en dólares corrientes."
)

p_base <- ggplot(plot_df, aes(x = anio, y = indice_2020, color = pais)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_text(
    data = growth_df,
    aes(x = x_label, y = y_label, label = label, color = pais),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    fontface = "bold"
  ) +
  scale_color_manual(values = palette_color) +
  scale_x_continuous(
    breaks = 2020:2024,
    expand = expansion(mult = c(0.02, 0.20))
  ) +
  scale_y_continuous(
    labels = label_number_intl(accuracy = 10),
    breaks = seq(100, 200, by = 20),
    limits = c(95, 205),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Índice de remesas recibidas",
    caption = wrap_caption_house(caption_raw)
  ) +
  coord_cartesian(xlim = c(2020, 2024.85), clip = "off") +
  theme_quantificador() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(colour = "grey90", linetype = "dashed"),
    plot.margin = margin(6, 64, 6, 16)
  )

spec <- house_spec("portrait")
# Reubicación solicitada: el logo queda sobre el caption y junto al extremo
# derecho del eje X, sin cubrir las etiquetas de crecimiento.
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
