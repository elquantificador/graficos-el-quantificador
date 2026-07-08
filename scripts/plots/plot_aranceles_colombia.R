# ============================================================
# plot_aranceles_colombia.R
# Genera el gráfico de mayores incrementos arancelarios aplicados
# por Ecuador, reproduciendo la visualización de referencia.
# Requiere: data/processed/aranceles_colombia.rds
# Guarda:   outputs/figures/30_aranceles-colombia-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_aranceles_colombia.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "stringr", "tidyr"))

out_path <- "outputs/figures/30_aranceles-colombia-ecuador.png"

plot_df <- readRDS("data/processed/aranceles_colombia.rds") |>
  dplyr::mutate(
    producto = dplyr::case_when(
      stringr::str_detect(descripcion, "^Que contengan") ~
        "Extractos y preparaciones\nvegetales",
      stringr::str_detect(descripcion, "^Preparaciones a base") ~
        "Preparaciones\na base de café",
      stringr::str_detect(descripcion, "^Extractos, esencias") ~
        "Extractos de café",
      stringr::str_detect(descripcion, "^Azúcar y melaza") ~
        "Azúcar y melaza",
      stringr::str_detect(descripcion, "^Agua, incluidas") ~
        "Agua",
      TRUE ~ stringr::str_wrap(descripcion, width = 18)
    )
  ) |>
  dplyr::arrange(dplyr::desc(arancel_base), dplyr::desc(arancel_nuevo)) |>
  dplyr::mutate(
    producto = factor(producto, levels = rev(unique(producto)))
  )

bars_df <- plot_df |>
  dplyr::select(producto, arancel_base, arancel_nuevo) |>
  tidyr::pivot_longer(
    cols = c(arancel_base, arancel_nuevo),
    names_to = "tipo",
    values_to = "arancel"
  ) |>
  dplyr::mutate(
    tipo = factor(
      tipo,
      levels = c("arancel_nuevo", "arancel_base"),
      labels = c("Nuevo arancel (TSCA)", "Antiguo arancel")
    )
  )

title_raw <- "Café, agua y azúcar fueron afectados por la\nguerra comercial con Colombia de 2026"
subtitle_raw <- paste(
  "Comparación entre el arancel anterior y la Tasa por Servicio de Control",
  "Aduanero (TSCA) para cinco productos importados desde Colombia"
)
caption_raw <- paste(
  "Fuente: COMEX, SENAE, Arancel Nacional Integrado y Primicias. Elaboración: Ángel",
  "Alava para El Quantificador. Nota: La Tasa por Servicio de Control Aduanero",
  "(TSCA) correspondió a mayo, antes de su eliminación anunciada el 30 de",
  "mayo de 2026."
)

palette <- c(
  "Nuevo arancel (TSCA)" = "steelblue",
  "Antiguo arancel" = "orange"
)

build_chart <- function() {
  ggplot(
    bars_df,
    aes(
      x = producto,
      y = arancel,
      fill = tipo
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.5),
      width = 0.5
    ) +
    geom_text(
      aes(label = scales::number(arancel, accuracy = 1, suffix = "%")),
      position = position_dodge(width = 0.5),
      hjust = -0.1,
      size = 2.8,
      colour = "black"
    ) +
    coord_flip() +
    scale_fill_manual(values = palette) +
    scale_y_continuous(
      limits = c(0, 88),
      breaks = seq(0, 80, 20),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = title_raw,
      subtitle = wrap_subtitle_house(subtitle_raw),
      x = NULL,
      y = "Porcentaje de arancel",
      fill = "Tipo",
      caption = wrap_caption_house(caption_raw)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_quantificador() +
    theme(
      axis.line = element_line(color = "#CCCCCC", linewidth = 0.5),
      axis.ticks = element_line(color = "#777777", linewidth = 0.6),
      axis.ticks.length = unit(0.15, "cm"),
      axis.text.x = element_text(color = "gray30", size = 7.5),
      axis.text.y = element_text(color = "gray30", size = 7.5, lineheight = 0.95),
      axis.title.x = element_text(color = "black", size = 7, margin = margin(t = 7), hjust = 0),
      axis.title.y = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0, size = 12.5, color = "black", lineheight = 1.05),
      plot.subtitle = element_text(hjust = 0, size = 9, color = "gray30", lineheight = 1.1),
      plot.caption = element_text(
        size = 6.2,
        hjust = 0,
        color = "gray30",
        lineheight = 1.1,
        margin = margin(t = 6, r = 0, b = 0, l = 0)
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6.4, margin = margin(l = 1)),
      legend.key.size = unit(8, "pt"),
      legend.key.width = unit(8, "pt"),
      legend.spacing.x = unit(3, "pt"),
      legend.margin = margin(t = 3, r = 0, b = 1, l = 0),
      plot.margin = margin(6, 24, 6, 16)
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")
p_final <- house_apply_logo(build_chart(), "portrait", x = 0.88, y = 0.14)

ggsave(
  filename = out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
