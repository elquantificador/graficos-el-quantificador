# ============================================================
# plot_inec_canasta_ingreso.R
# Adapta al house style la comparación de canasta e ingresos para un hogar tipo.
# Requiere: data/processed/inec_canasta_ingreso.rds
# Guarda:   outputs/figures/43_canasta-basica-ingreso-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_inec_canasta_ingreso.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/inec_canasta_ingreso.rds"
out_path <- "outputs/figures/43_canasta-basica-ingreso-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_inec_canasta_ingreso.R")
}

processed <- readRDS(data_path)
df <- processed$data

last_year <- max(df$anio)
last_row <- df |>
  dplyr::filter(.data$anio == last_year)

line_df <- dplyr::bind_rows(
  df |>
    dplyr::transmute(
      anio = .data$anio,
      valor = .data$canasta_basica_usd,
      serie = "Costo de la canasta básica",
      tipo = "Referencia normativa"
    ),
  df |>
    dplyr::transmute(
      anio = .data$anio,
      valor = .data$ingreso_familiar_usd,
      serie = "Ingresos de 1,6 perceptores, hogar tipo",
      tipo = "Referencia normativa"
    ),
  df |>
    dplyr::transmute(
      anio = .data$anio,
      valor = .data$ingreso_familiar_mediano_referencia_equiv_usd,
      serie = "Ingreso observado, hogar de referencia",
      tipo = "Observado"
    )
)

label_df <- dplyr::bind_rows(
  last_row |>
    dplyr::transmute(
      x = .data$anio - 0.08,
      y = .data$canasta_basica_usd + 20,
      label = paste0("Canasta: ", label_dollar_intl(accuracy = 1)(.data$canasta_basica_usd)),
      serie = "Costo de la canasta básica"
    ),
  last_row |>
    dplyr::transmute(
      x = .data$anio - 0.08,
      y = .data$ingreso_familiar_usd + 22,
      label = paste0("Ingresos de 1,6\nperceptores: ", label_dollar_intl(accuracy = 1)(.data$ingreso_familiar_usd)),
      serie = "Ingresos de 1,6 perceptores, hogar tipo"
    ),
  last_row |>
    dplyr::transmute(
      x = .data$anio - 0.08,
      y = .data$ingreso_familiar_mediano_referencia_equiv_usd + 18,
      label = paste0("Ingreso observado\nHogar de referencia: ", label_dollar_intl(accuracy = 1)(.data$ingreso_familiar_mediano_referencia_equiv_usd)),
      serie = "Ingreso observado, hogar de referencia"
    )
)

title_raw <- "¿Alcanza el ingreso familiar para la canasta básica? Realmente, no"
subtitle_raw <- "Canasta Familiar Básica e ingresos mensuales comparables para un hogar de 4 personas, Ecuador, 2018-2026"
caption_raw <- paste(
  "Fuente: INEC, IPC y ENEMDU.",
  "Elaboración: Karel Lázaro González Ruíz; adaptación: El Quantificador.",
  "Nota: la mediana observada es la mediana ponderada de ingpc en hogares de 4 personas con 2 adultos y 2 hijos menores de 18 años; se multiplica por cuatro para expresarla en escala de hogar. Los cortes son diciembre de 2018-2025; para 2026 se usa el I trimestre."
)

p_base <- ggplot2::ggplot(df, ggplot2::aes(x = .data$anio)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = pmin(.data$canasta_basica_usd, .data$ingreso_familiar_mediano_referencia_equiv_usd),
      ymax = pmax(.data$canasta_basica_usd, .data$ingreso_familiar_mediano_referencia_equiv_usd)
    ),
    fill = "#f3d6bb",
    alpha = 0.9
  ) +
  ggplot2::geom_line(
    data = line_df,
    ggplot2::aes(y = .data$valor, colour = .data$serie, linetype = .data$tipo),
    linewidth = 0.9
  ) +
  ggplot2::geom_point(
    data = line_df,
    ggplot2::aes(y = .data$valor, colour = .data$serie, shape = .data$tipo),
    size = 1.7
  ) +
  ggplot2::geom_text(
    data = label_df,
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$label, colour = .data$serie),
    hjust = 1,
    size = 2.45,
    fontface = "bold",
    show.legend = FALSE
  ) +
  ggplot2::scale_colour_manual(
    values = c(
        "Costo de la canasta básica" = "#d97729",
        "Ingresos de 1,6 perceptores, hogar tipo" = "#2D7DB3",
        "Ingreso observado, hogar de referencia" = "#4F5D75"
    )
  ) +
  ggplot2::scale_linetype_manual(
    values = c("Referencia normativa" = "solid", "Observado" = "dashed")
  ) +
  ggplot2::scale_shape_manual(
    values = c("Referencia normativa" = 16, "Observado" = 1)
  ) +
  ggplot2::scale_x_continuous(
    breaks = df$anio,
    expand = ggplot2::expansion(mult = c(0.02, 0.04))
  ) +
  ggplot2::scale_y_continuous(
    labels = label_dollar_intl(accuracy = 1),
    breaks = seq(400, 1000, by = 200),
    limits = c(400, 1000),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "USD mensuales por hogar de referencia (4 personas)",
    caption = wrap_caption_house(caption_raw)
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  theme_quantificador() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = 7.5, angle = 45, hjust = 1),
    axis.title.y = ggplot2::element_text(hjust = 0.5, vjust = 0.5),
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_line(colour = "grey90", linetype = "dashed"),
    plot.margin = ggplot2::margin(6, 16, 6, 16)
  )

spec <- house_spec("portrait")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  filename = out_path,
  plot = house_apply_logo(p_base, "portrait", y = 0.18),
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
