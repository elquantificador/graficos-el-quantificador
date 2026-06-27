# ============================================================
# plot_lgbti_conocen_orientacion_identidad.R
# Author: Daniel Sánchez Pazmiño
# Purpose: Gráfico sobre quiénes conocen la orientación sexual o
#          identidad de género de la población LGBTI+.
# Inputs:  data/processed/lgbti_conocen_orientacion_identidad_2025.rds
# Outputs: outputs/figures/27_a_quienes-conocen-orientacion-identidad-lgbti-ecuador.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales"))

# 0. Setup ----
input_path <- "data/processed/lgbti_conocen_orientacion_identidad_2025.rds"
out_path   <- "outputs/figures/27_b_quienes-conocen-orientacion-identidad-lgbti-ecuador.png"

# 1. Load Data ----
plot_df <- readRDS(input_path) |>
  dplyr::mutate(
    grupo = dplyr::recode(
      grupo,
      "Compañeras/os de estudio/trabajo" = "Comp. estudio/trabajo"
    ),
    grupo = factor(grupo, levels = grupo[order(porcentaje)])
  )

# 4. Figures ----
title_raw <- wrap_title_house(
  "Los padres son quienes menos saben sobre la identidad LGBTI+ de sus hijos"
)
subtitle_raw <- paste(
  "Porcentaje que conoce la orientación sexual o identidad de",
  "género, por tipo de relación, Ecuador, ENCV LGBTI+ 2025",
  sep = "\n"
)
caption_raw <- paste(
  "Fuente: INEC, Encuesta Nacional de Condiciones de Vida de la Población LGBTI+ (ECV LGBTI+), 2025.",
  "Elaboración: Alonso Quijano-Ruiz para El Quantificador. Nota: La variable mide si la",
  "persona conoce la identidad de género u orientación sexual de la persona encuestada.",
  "La encuesta considera a la población LGBTI+ con 6.657 observaciones. Los porcentajes",
  "usan el factor de expansión de la encuesta y excluyen respuestas \"No aplica\"."
)

p_base <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = grupo, y = porcentaje)
) +
  ggplot2::geom_col(fill = "#2A9D8F", width = 0.65) +
  ggplot2::geom_text(
    ggplot2::aes(label = percent_intl(porcentaje, accuracy = 1)),
    hjust = -0.2,
    size = 2.7,
    colour = "grey20"
  ) +
  ggplot2::scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    breaks = seq(0, 0.8, by = 0.2),
    expand = ggplot2::expansion(mult = c(0, 0.18))
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title    = title_raw,
    subtitle = subtitle_raw,
    x        = NULL,
    y        = "Porcentaje (%)",
    caption  = wrap_caption_house(caption_raw, width = 90)
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.line.y  = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y  = ggplot2::element_text(size = 7.2, colour = "grey20"),
    plot.margin  = ggplot2::margin(6, 18, 6, 8)
  )

p_final <- add_logo(p_base, x = 0.89, y = 0.23)

# 5. Export ----
png_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggplot2::ggsave(
  out_path,
  plot   = p_final,
  width  = 4,
  height = 5,
  dpi    = 300,
  device = png_device,
  bg     = "white"
)

message("Guardado: ", out_path)

sessionInfo()
