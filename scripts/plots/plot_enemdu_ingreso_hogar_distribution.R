# ============================================================
# plot_enemdu_ingreso_hogar_distribution.R
# Genera un histograma ponderado de la distribución del ingreso
# total del hogar en la ENEMDU marzo 2026.
# Requiere: data/processed/enemdu_ingreso_hogar_2026_03.rds
# Guarda:   outputs/figures/16_ingreso-hogar_distribucion-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_ingreso_hogar_distribution.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "scales", "ragg", "Hmisc"))

input_path <- "data/processed/enemdu_ingreso_hogar_2026_03.rds"
out_path <- "outputs/figures/16_ingreso-hogar_distribucion-ecuador.png"

df <- readRDS(input_path)

wtd_q <- function(x, w, p) {
  as.numeric(Hmisc::wtd.quantile(x, weights = w, probs = p, na.rm = TRUE)[[1]])
}

p99_income <- wtd_q(df$ingreso_total_hogar, df$fexp, 0.99)
share_below_1500 <- 100 * sum(df$fexp[df$ingreso_total_hogar < 1500], na.rm = TRUE) / sum(df$fexp, na.rm = TRUE)
share_at_or_above_1500 <- 100 - share_below_1500

plot_df <- df %>%
  filter(!is.na(ingreso_total_hogar), !is.na(fexp), ingreso_total_hogar >= 0) %>%
  mutate(
    ingreso_plot = pmin(ingreso_total_hogar, p99_income),
    fue_recortado = ingreso_total_hogar > p99_income,
    tramo_1500 = if_else(ingreso_total_hogar < 1500, "Menos de $1,500", "$1,500 o más")
  )

bin_df <- plot_df %>%
  mutate(bin = floor(ingreso_plot / 100) * 100) %>%
  group_by(bin) %>%
  summarise(weighted_count = sum(fexp, na.rm = TRUE), .groups = "drop")

y_max <- max(bin_df$weighted_count, na.rm = TRUE)

subtitle_txt <- "Distribución del ingreso mensual de los hogares, ENEMDU 2026"

title_raw <- "Solo el 14% de hogares ecuatorianos ganan $1.500 o más (el 60% gana $513 o menos)"
caption_portrait <- paste0(
  "Fuente: ENEMDU - INEC, marzo 2026. Cálculos de Daniel Sánchez para El Quantificador\n",
  "de Laboratorio LIDE. Ingreso total del hogar = suma del ingreso laboral primario y secundario,\n",
  "ingresos de capital, transferencias, pensiones y bonos de todos los miembros del hogar.\n",
  "Histograma ponderado por pesos de muestra con intervalos de USD 100. Visualización hasta\n",
  "el percentil 99. Línea punteada = umbral de $1,500."
)
caption_raw <- paste0(
  "Fuente: ENEMDU - INEC, marzo 2026. Cálculos de Daniel Sánchez para El Quantificador ",
  "de Laboratorio LIDE. Ingreso total del hogar = suma del ingreso laboral primario y secundario, ",
  "ingresos de capital, transferencias, pensiones y bonos de todos los miembros del hogar. ",
  "Histograma ponderado por pesos de muestra con intervalos de USD 100. Visualización hasta ",
  "el percentil 99. Línea punteada = umbral de $1,500."
)

build_chart <- function(orientation) {
  spec <- house_spec(orientation)
  if (orientation == "landscape") {
    title_txt   <- stringr::str_wrap(title_raw, width = spec$title_wrap)
    caption_txt <- stringr::str_wrap(caption_raw, width = spec$caption_wrap)
  } else {
    title_txt   <- "Solo el 14% de hogares ecuatorianos ganan\n$1.500 o más (el 60% gana $513 o menos)"
    caption_txt <- caption_portrait
  }

  ggplot(plot_df, aes(x = ingreso_plot, weight = fexp)) +
  geom_histogram(
    aes(fill = tramo_1500),
    binwidth = 100,
    boundary = 0,
    closed = "left",
    color = "white",
    linewidth = 0.2
  ) +
  geom_vline(
    xintercept = 1500,
    color = scales::alpha("grey35", 0.6),
    linetype = "dashed",
    linewidth = 0.6
  ) +
  annotate(
    "segment",
    x = 950, xend = 1460,
    y = y_max * 0.78, yend = y_max * 0.60,
    colour = "#127A96",
    linewidth = 0.5,
    arrow = grid::arrow(length = grid::unit(0.10, "inches"))
  ) +
  annotate(
    "text",
    x = 760,
    y = y_max * 0.82,
    label = paste0(
      scales::number(share_below_1500, accuracy = 0.1, decimal.mark = ","), "% de hogares\n",
      "ganan menos de $1.500"
    ),
    hjust = 0,
    size = 2.7,
    lineheight = 1.05,
    colour = "#127A96",
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = 2400, xend = 1540,
    y = y_max * 0.42, yend = y_max * 0.52,
    colour = "#0A4F78",
    linewidth = 0.5,
    arrow = grid::arrow(length = grid::unit(0.10, "inches"))
  ) +
  annotate(
    "text",
    x = 2480,
    y = y_max * 0.46,
    label = paste0(
      scales::number(share_at_or_above_1500, accuracy = 0.1, decimal.mark = ","), "% de hogares\n",
      "ganan $1.500 o más"
    ),
    hjust = 0,
    size = 2.7,
    lineheight = 1.05,
    colour = "#0A4F78",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Menos de $1,500" = "#8CCFE3",
      "$1,500 o más" = "#127A96"
    )
  ) +
  scale_x_continuous(
    labels = label_dollar(big.mark = ".", decimal.mark = ",", accuracy = 1),
    breaks = seq(0, ceiling(p99_income / 500) * 500, by = 500),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",", accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = "Ingreso total mensual del hogar (USD)",
    y = "Número ponderado de hogares",
    caption = caption_txt
  ) +
  theme_quantificador(orientation) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    plot.caption = element_text(colour = "black", size = 6.5, lineheight = 1.1, hjust = 0, margin = margin(t = 10)),
    legend.position = "none",
    plot.margin = if (orientation == "landscape") margin(6, 16, 6, 16) else margin(6, 30, 6, 16)
  )
}

dir.create("outputs/figures", showWarnings = FALSE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  p_final <- house_apply_logo(build_chart(orientation), orientation, x = 0.895, y = 0.18, width = 0.09, height = 0.09)
  dest <- house_out_path(out_path, orientation)
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

