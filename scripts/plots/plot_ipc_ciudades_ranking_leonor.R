# ============================================================
# plot_ipc_ciudades_ranking_leonor.R
# Renderiza el ranking de la variacion acumulada del IPC por ciudad.
# Requiere: data/processed/ipc_ciudades_inec_2026_06.rds
# Guarda:   outputs/figures/41_b_costo-vida-ciudades-ranking.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/ipc_ciudades_inec_2026_06.rds"
out_path <- "outputs/figures/41_b_costo-vida-ciudades-ranking.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_ipc_ciudades_leonor.R")
}

processed <- readRDS(data_path)
ranking <- processed$ranking |>
  dplyr::arrange(.data$variacion_acumulada_pct) |>
  dplyr::mutate(
    ciudad = factor(.data$ciudad, levels = .data$ciudad),
    etiqueta = paste0(
      label_number_intl(accuracy = 0.1)(.data$variacion_acumulada_pct), "%"
    )
  )

title_raw <- "Santo Domingo registró el mayor aumento de precios entre 2021 y 2026"
subtitle_raw <- "Variación acumulada del IPC por ciudad, enero 2021 – junio 2026"
caption_raw <- paste(
  "Fuente: INEC, Índice de Precios al Consumidor, series IPC nacional/regional/ciudad, corte junio 2026.",
  "Elaboración: Leonor Molina Zapata; adaptación y verificación de El Quantificador.",
  "Nota: la variación acumulada compara el IPC de junio de 2026 con el de enero de 2021."
)

p_base <- ggplot(ranking, aes(x = .data$variacion_acumulada_pct, y = .data$ciudad)) +
  geom_col(fill = "#4F7EAE", width = 0.68) +
  geom_text(
    aes(label = .data$etiqueta),
    hjust = -0.15,
    size = 3.0,
    colour = "grey20",
    fontface = "bold"
  ) +
  scale_x_continuous(
    labels = label_number_intl(accuracy = 2, suffix = "%"),
    breaks = seq(0, 16, by = 4),
    limits = c(0, 18),
    expand = c(0, 0)
  ) +
  labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = "Aumento acumulado",
    y = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  theme(
    axis.text.y = element_text(size = 7.4),
    axis.text.x = element_text(size = 7),
    axis.title.x = element_text(size = 7, margin = margin(t = 7)),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88", linetype = "dashed"),
    plot.margin = margin(6, 30, 6, 16)
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
