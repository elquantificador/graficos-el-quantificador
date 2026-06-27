# ============================================================
# plot_lgbti_aceptacion_orientacion_identidad.R
# Genera el gráfico sobre aceptación de la orientación sexual
# o identidad de género entre personas cercanas.
# Requiere: data/processed/lgbti_aceptacion_orientacion_identidad_2025.rds
# Guarda:   outputs/figures/27_aceptacion-orientacion-identidad-lgbti-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_lgbti_aceptacion_orientacion_identidad.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales"))

input_path <- "data/processed/lgbti_aceptacion_orientacion_identidad_2025.rds"
out_path <- "outputs/figures/27_aceptacion-orientacion-identidad-lgbti-ecuador.png"

plot_df <- readRDS(input_path) |>
  dplyr::mutate(
    grupo = factor(
      grupo,
      levels = c(
        "Madre",
        "Padre",
        "Hermanas/os",
        "Amigas/os",
        "Compañeras/os de estudio/trabajo"
      )
    ),
    respuesta = factor(
      respuesta,
      levels = c("Aceptación total", "Aceptación parcial", "Rechazo total")
    )
  )

title_raw <- "La aceptación de la orientación sexual o identidad de género es más alta entre amistades y hermanas/os"
subtitle_raw <- paste(
  "Distribución de respuestas sobre aceptación entre madre, padre, hermanas/os,",
  "amigas/os y compañeras/os de estudio o trabajo, ENCV LGBTI+ 2025"
)
caption_raw <- paste(
  "Fuente: Encuesta Nacional de Condiciones de Vida de la Población LGBTI+ (ECV LGBTI+), 2025.",
  "Elaboración: Alonso Quijano-Ruiz para El Quantificador. Nota: Los porcentajes usan el factor",
  "de expansión de la encuesta y excluyen respuestas \"No aplica\" y \"No sabe\"."
)

palette_response <- c(
  "Aceptación total" = "#2A9D8F",
  "Aceptación parcial" = "#8FC9C0",
  "Rechazo total" = "#D97729"
)

p_base <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = grupo, y = porcentaje, fill = respuesta)
) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = ifelse(porcentaje >= 0.04, percent_intl(porcentaje, accuracy = 1), "")
    ),
    position = ggplot2::position_stack(vjust = 0.5),
    size = 2.7,
    colour = "white"
  ) +
  ggplot2::scale_fill_manual(values = palette_response) +
  ggplot2::scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    breaks = seq(0, 1, by = 0.2),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Porcentaje",
    fill = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
  theme_quantificador() +
  ggplot2::theme(
    legend.position = "top",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.text = ggplot2::element_text(size = 6.5, colour = "grey20"),
    legend.key.size = grid::unit(10, "pt"),
    legend.spacing.x = grid::unit(4, "pt"),
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 8, colour = "grey20"),
    plot.margin = ggplot2::margin(6, 18, 6, 12)
  )

p_final <- add_logo(p_base, x = 0.88, y = 0.15)

png_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggplot2::ggsave(
  out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = png_device,
  bg = "white"
)

message("Guardado: ", out_path)
