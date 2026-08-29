# ============================================================
# plot_ipc_ciudades_heatmap_leonor.R
# Renderiza el heatmap de la variacion acumulada del IPC por ciudad.
# Requiere: data/processed/ipc_ciudades_inec_2026_06.rds
# Guarda:   outputs/figures/41_a_costo-vida-ciudades-heatmap.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/ipc_ciudades_inec_2026_06.rds"
out_path <- "outputs/figures/41_a_costo-vida-ciudades-heatmap.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_ipc_ciudades_leonor.R")
}

processed <- readRDS(data_path)
ranking <- processed$ranking |>
  dplyr::arrange(.data$puesto)

heatmap_df <- processed$ipc |>
  dplyr::group_by(.data$ciudad) |>
  dplyr::arrange(.data$fecha, .by_group = TRUE) |>
  dplyr::mutate(
    variacion_desde_enero_2021 = (.data$ipc / dplyr::first(.data$ipc) - 1) * 100
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    .data$fecha %in% as.Date(c(
      "2021-01-01", "2021-12-01", "2022-12-01", "2023-12-01",
      "2024-12-01", "2025-12-01", "2026-06-01"
    ))
  ) |>
  dplyr::mutate(
    ciudad = factor(.data$ciudad, levels = rev(ranking$ciudad)),
    corte = factor(
      format(.data$fecha, "%Y-%m"),
      levels = c("2021-01", "2021-12", "2022-12", "2023-12", "2024-12", "2025-12", "2026-06"),
      labels = c("ene 2021", "dic 2021", "dic 2022", "dic 2023", "dic 2024", "dic 2025", "jun 2026")
    ),
    etiqueta = ifelse(
      .data$corte == "jun 2026",
      paste0(label_number_intl(accuracy = 0.1)(.data$variacion_desde_enero_2021), "%"),
      label_number_intl(accuracy = 0.1)(.data$variacion_desde_enero_2021)
    )
  )

title_raw <- "El costo de vida subió más en unas ciudades que en otras"
subtitle_raw <- "Variación acumulada del IPC frente a enero de 2021, cortes seleccionados, enero 2021 – junio 2026"
caption_raw <- paste(
  "Fuente: INEC, Índice de Precios al Consumidor, series IPC nacional/regional/ciudad, corte junio 2026.",
  "Elaboración: Leonor Molina Zapata; adaptación y verificación de El Quantificador.",
  "Nota: la variación acumulada fue calculada como (IPC del periodo / IPC de enero 2021 - 1) × 100."
)

max_value <- max(heatmap_df$variacion_desde_enero_2021, na.rm = TRUE)

p_base <- ggplot(
  heatmap_df,
  aes(x = .data$corte, y = .data$ciudad, fill = .data$variacion_desde_enero_2021)
) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(
    aes(label = .data$etiqueta),
    size = 2.65,
    colour = "grey15",
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#FFF7BC",
    high = "#A50026",
    limits = c(0, max_value),
    labels = label_number_intl(accuracy = 2, suffix = "%")
  ) +
  labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = NULL,
    fill = "Variacion acumulada",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  theme(
    axis.text.y = element_text(size = 7.1),
    axis.text.x = element_text(size = 6.4),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 6.5),
    legend.text = element_text(size = 6),
    legend.key.width = grid::unit(0.75, "cm"),
    legend.key.height = grid::unit(0.24, "cm"),
    plot.margin = margin(6, 26, 6, 12)
  )

spec <- house_spec("portrait")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(
  filename = out_path,
  plot = house_apply_logo(p_base, "portrait", x = 0.89, y = 0.075, width = 0.07, height = 0.07),
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
