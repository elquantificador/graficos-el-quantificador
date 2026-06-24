# ============================================================
# plot_wvs_homosexualidad_padres.R
# Genera el gráfico sobre opiniones acerca de si las parejas
# homosexuales son tan buenos padres como otras parejas.
# Requiere: data/processed/wvs_homosexualidad_padres.rds
# Guarda:   outputs/figures/26_homoparentalidad-opinion-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_wvs_homosexualidad_padres.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

input_path <- "data/processed/wvs_homosexualidad_padres.rds"
out_path <- "outputs/figures/26_homoparentalidad-opinion-ecuador.png"

plot_df <- readRDS(input_path) |>
  dplyr::mutate(
    respuesta = factor(
      as.character(respuesta),
      levels = c(
        "Totalmente de acuerdo",
        "De acuerdo",
        "Indiferente",
        "En desacuerdo",
        "Totalmente en desacuerdo",
        "No sabe"
      )
    ),
    grupo = dplyr::case_when(
      respuesta %in% c("Totalmente de acuerdo", "De acuerdo") ~ "acuerdo",
      respuesta %in% c("En desacuerdo", "Totalmente en desacuerdo") ~ "desacuerdo",
      TRUE ~ "neutral"
    )
  )

title_raw <- "La mayoría de ecuatorianos discrepa con que las parejas homosexuales sean tan buenos padres"
subtitle_raw <- paste(
  "Respuesta a la afirmación \"Las parejas homosexuales son tan buenos padres",
  "como otras parejas\", WVS Ecuador 2018"
)
caption_raw <- paste(
  "Fuente: World Values Survey (WVS), Ecuador, 2018. Elaboración:",
  "Alonso Quijano-Ruiz para El Quantificador. Nota: La encuesta",
  "es autoponderada."
)

palette_response <- c(
  "acuerdo" = "#55A6A6",
  "neutral" = "#B9C3C9",
  "desacuerdo" = "#D97729"
)

p_base <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = respuesta, y = porcentaje, fill = grupo)
) +
  ggplot2::geom_col(width = 0.64) +
  ggplot2::geom_text(
    ggplot2::aes(label = percent_intl(porcentaje, accuracy = 1)),
    hjust = -0.18,
    size = 2.9,
    colour = "grey20"
  ) +
  ggplot2::scale_fill_manual(values = palette_response, guide = "none") +
  ggplot2::scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    limits = c(0, 0.37),
    breaks = seq(0, 0.35, by = 0.05),
    expand = ggplot2::expansion(mult = c(0, 0.03))
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Porcentaje",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 8, colour = "grey20"),
    plot.margin = ggplot2::margin(6, 22, 6, 12)
  )

p_final <- add_logo(p_base, x = 0.88, y = 0.17)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggplot2::ggsave(
  out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
