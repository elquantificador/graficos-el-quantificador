# ============================================================
# plot_50_bce_importaciones_colombia.R
# Compara las importaciones mensuales desde Colombia en enero-julio.
# Author: Daniel Sanchez
# Purpose: Muestra valores FOB mensuales de 2025 y 2026 y su cambio acumulado.
# Inputs: data/processed/bce_importaciones_colombia.rds
# Outputs: outputs/figures/50_importaciones-colombia-ecuador.png
# Requiere: data/processed/bce_importaciones_colombia.rds
# Guarda:   outputs/figures/50_importaciones-colombia-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_50_bce_importaciones_colombia.R
# ============================================================

# 0. Setup ----

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

input_path <- "data/processed/bce_importaciones_colombia.rds"
out_path <- "outputs/figures/50_importaciones-colombia-ecuador.png"
base_year <- 2025L
comparison_year <- 2026L
last_month <- 7L

# 1. Read inputs ----

importaciones <- readRDS(input_path)

# 2. Prepare data ----

comparacion_mensual <- importaciones |>
  filter(
    .data$anio %in% c(base_year, comparison_year),
    .data$mes <= last_month
  ) |>
  summarise(
    fob_millones_usd = sum(.data$fob_millones_usd, na.rm = TRUE),
    .by = c(anio, mes)
  ) |>
  mutate(
    anio = factor(
      .data$anio,
      levels = c(base_year, comparison_year),
      labels = as.character(c(base_year, comparison_year))
    ),
    mes = factor(
      .data$mes,
      levels = seq_len(last_month),
      labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul")
    )
  )

# 3. Calculate estimates ----

totales_enero_julio <- comparacion_mensual |>
  summarise(
    fob_enero_julio_millones_usd = sum(
      .data$fob_millones_usd,
      na.rm = TRUE
    ),
    .by = anio
  )

fob_base_millones_usd <- totales_enero_julio |>
  filter(.data$anio == as.character(base_year)) |>
  pull(.data$fob_enero_julio_millones_usd)

fob_comparacion_millones_usd <- totales_enero_julio |>
  filter(.data$anio == as.character(comparison_year)) |>
  pull(.data$fob_enero_julio_millones_usd)

cambio_acumulado_pct <-
  (fob_comparacion_millones_usd / fob_base_millones_usd - 1) * 100

title_raw <- paste0(
  "Las importaciones desde Colombia ",
  if_else(cambio_acumulado_pct < 0, "cayeron ", "aumentaron "),
  label_number_intl(accuracy = 0.1)(abs(cambio_acumulado_pct)),
  "% frente a ",
  base_year
)
subtitle_raw <- paste0(
  "Importaciones mensuales, enero-julio de ",
  base_year,
  " y ",
  comparison_year
)
caption_raw <- paste(
  "Fuente: Banco Central del Ecuador (BCE), importaciones mensuales por país de origen.",
  "Elaboración: Daniel Sánchez-Pazmiño para El Quantificador.",
  "La tasa de control aduanero para importaciones desde Colombia empezó el 1 de febrero de 2026."
)

p_base <- ggplot(
  comparacion_mensual,
  aes(
    x = .data$mes,
    y = .data$fob_millones_usd,
    fill = .data$anio,
    group = .data$anio
  )
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  scale_fill_manual(
    values = stats::setNames(
      c("grey55", "#2D7DB3"),
      as.character(c(base_year, comparison_year))
    ),
    name = NULL
  ) +
  scale_y_continuous(
    labels = label_number_intl(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Millones de USD (FOB)",
    fill = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    axis.text.x = element_text(size = 7.5),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.margin = margin(6, 36, 6, 16)
  )

# 4. Write outputs ----

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.15, width = 0.09, height = 0.09)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)

sessionInfo()
