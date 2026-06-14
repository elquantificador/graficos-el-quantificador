# ============================================================
# plot_exportaciones_eeuu.R
# Genera el gráfico del top 10 de exportaciones de Ecuador
# hacia Estados Unidos, comparando 2024 vs 2025.
# Requiere:
#   - data/processed/exportaciones_eeuu_2024_2025.rds
# Guarda:
#   - outputs/figures/12_exportaciones_eeuu_top10_2024-2025.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_exportaciones_eeuu.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "ggplot2", "scales", "stringr", "ragg"))

series <- readRDS("data/processed/exportaciones_eeuu_2024_2025.rds")
top10 <- series$top10

formato_miles <- scales::label_number(
  accuracy = 0.1,
  big.mark = ".",
  decimal.mark = ","
)

orden_productos <- top10 |>
  dplyr::arrange(valor_total_millones_usd) |>
  dplyr::pull(producto_agrupado)

top10_long <- top10 |>
  dplyr::select(
    producto_agrupado,
    fob_total_millones_usd_2024,
    fob_total_millones_usd_2025,
    valor_total_millones_usd
  ) |>
  tidyr::pivot_longer(
    cols = c(fob_total_millones_usd_2024, fob_total_millones_usd_2025),
    names_to = "anio",
    values_to = "fob_millones"
  ) |>
  dplyr::mutate(
    anio = dplyr::recode(
      anio,
      "fob_total_millones_usd_2024" = "2024",
      "fob_total_millones_usd_2025" = "2025"
    ),
    producto_display = stringr::str_to_sentence(
      stringr::str_to_lower(producto_agrupado, locale = "es"),
      locale = "es"
    ),
    producto_display = dplyr::if_else(
      producto_agrupado == "PETRÓLEO CRUDO Y DERIVADOS",
      "Petróleo crudo y derivados*",
      producto_display
    ),
    producto_display = stringr::str_wrap(producto_display, width = 22),
    producto_agrupado = factor(
      producto_display,
      levels = stringr::str_wrap(
        dplyr::if_else(
          orden_productos == "PETRÓLEO CRUDO Y DERIVADOS",
          "Petróleo crudo y derivados*",
          stringr::str_to_sentence(
            stringr::str_to_lower(orden_productos, locale = "es"),
            locale = "es"
          )
        ),
        width = 22
      )
    ),
    etiqueta = formato_miles(fob_millones)
  )

caption_raw <- paste(
  "Fuente: Banco Central del Ecuador (BCE), Estadísticas de Comercio Exterior de Bienes.",
  "Nota: FOB significa Free On Board y corresponde al valor de la mercancía puesta a bordo",
  "en el puerto de salida, sin incluir flete ni seguro internacional.",
  "*El BCE reporta que, según los registros de Petroecuador y firmas privadas, la mayor parte del crudo",
  "se declara con destino final Panamá a partir de 2025.",
  "Elaboración: El Quantificador de Laboratorio LIDE."
)

title_raw <- "El camarón y el petróleo son los productos más exportados a Estados Unidos"

portrait_path <- "outputs/figures/12_exportaciones_eeuu_top10_2024-2025.png"

build_chart <- function(orientation) {
  spec <- house_spec(orientation)
  # Portrait conserva el título de dos líneas (salto manual) y el caption a 95
  # para no alterar el PNG publicado; landscape reacomoda al ancho apaisado.
  title_txt <- if (orientation == "landscape") {
    stringr::str_wrap(title_raw, width = spec$title_wrap)
  } else {
    "El camarón y el petróleo son los productos\nmás exportados a Estados Unidos"
  }
  # El caption se renderiza a 5,5 pt (más pequeño que la casa), así que en
  # landscape se envuelve a un ancho que llena el lienzo a ese tamaño.
  caption_w <- if (orientation == "landscape") landscape_wrap_for_size(5.5) else 95

  ggplot2::ggplot(
    top10_long,
    ggplot2::aes(x = producto_agrupado, y = fob_millones, fill = anio)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.72),
      width = 0.62
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = etiqueta),
      position = ggplot2::position_dodge(width = 0.72),
      hjust = -0.08,
      size = 2.2,
      colour = "black"
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_manual(
      values = c("2024" = "#1696b5", "2025" = "#e7a04b")
    ) +
    ggplot2::scale_y_continuous(
      labels = formato_miles,
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::labs(
      title = title_txt,
      subtitle = "Top 10 productos exportados a E.E.U.U., 2024-2025",
      x = NULL,
      y = "Valor FOB (millones de USD)",
      caption = stringr::str_wrap(caption_raw, width = caption_w)
    ) +
    theme_quantificador(orientation) +
    ggplot2::theme(
      legend.position = if (orientation == "landscape") c(0.86, 0.22) else c(0.78, 0.17),
      legend.justification = c(0, 0.5),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 7),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(size = 9, lineheight = 1.1, hjust = 0),
      plot.caption = ggplot2::element_text(size = 5.5, lineheight = 1.15, hjust = 0, margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(
        colour = "grey20",
        hjust = 0.5,
        vjust = 0.5,
        margin = ggplot2::margin(r = 10, b = 8)
      ),
      legend.key.width = grid::unit(4, "mm"),
      legend.key.height = grid::unit(4, "mm"),
      plot.margin = if (orientation == "landscape") ggplot2::margin(10, 24, 8, 16) else ggplot2::margin(10, 46, 8, 16)
    )
}

dir.create("outputs/figures", showWarnings = FALSE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  # Sin logo en landscape; portrait mantiene el logo en su posición de la casa.
  p_final <- house_apply_logo(build_chart(orientation), orientation,
                              x = 0.88, y = 0.18, width = 0.09, height = 0.09)
  out_path <- house_out_path(portrait_path, orientation)
  ggplot2::ggsave(
    out_path,
    plot = p_final,
    width = spec$width,
    height = spec$height,
    units = "in",
    dpi = spec$dpi,
    device = ragg::agg_png,
    bg = "white"
  )
  message("Guardado: ", out_path)
}

