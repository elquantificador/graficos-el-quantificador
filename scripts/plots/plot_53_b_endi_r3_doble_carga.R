# ============================================================
# plot_53_b_endi_r3_doble_carga.R
# Desnutrición crónica y sobrepeso en menores de 5 años por área,
# ENDI rondas 2 y 3.
# Requiere: data/processed/endi_r3_desnutricion_sobrepeso.rds
# Guarda:   outputs/figures/53_b_desnutricion-sobrepeso-area-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_53_b_endi_r3_doble_carga.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/endi_r3_desnutricion_sobrepeso.rds"
out_path <- "outputs/figures/53_b_desnutricion-sobrepeso-area-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_endi_r3_desnutricion_sobrepeso.R")
}

colores <- c(
  "Desnutrición crónica.R2" = "#A9CAD6",
  "Desnutrición crónica.R3" = "#006B8F",
  "Sobrepeso y obesidad.R2" = "#F7D5B1",
  "Sobrepeso y obesidad.R3" = "#E08A2E"
)

signo_pp <- function(x) {
  paste0(ifelse(x > 0, "+", "-"), scales::number(abs(x), accuracy = 0.1, decimal.mark = ","))
}

plot_df <- readRDS(data_path)$doble_carga |>
  dplyr::mutate(
    area = factor(area, levels = rev(levels(area))),
    ronda = factor(ronda, levels = c("R3", "R2")),
    relleno = paste(indicador, ronda, sep = "."),
    etiqueta = dplyr::if_else(
      ronda == "R3",
      paste0(percent_intl(valor, accuracy = 0.1), "   ", signo_pp(cambio_pp),
             dplyr::if_else(sig, "*", "")),
      percent_intl(valor, accuracy = 0.1)
    ),
    color_txt = dplyr::case_when(
      ronda == "R2" ~ "grey40",
      sig ~ "#A8363D",
      TRUE ~ "grey20"
    ),
    cara = dplyr::if_else(ronda == "R3", "bold", "plain")
  )

dodge <- ggplot2::position_dodge(width = 0.8)

title_raw <- "El sobrepeso infantil subió en áreas urbanas y rurales"
subtitle_raw <- paste(
  "Menores de 5 años por área. Barras claras: 2023-2024;",
  "barras oscuras: 2025-2026"
)
caption_raw <- paste(
  "Fuente: INEC, ENDI, tabulados de las rondas 2 (2023-2024) y 3 (2025-2026).",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador.",
  "Nota: el número junto a la barra oscura es el cambio en puntos porcentuales frente a la ronda 2.",
  "Cambios calculados antes de redondear. * Cambio significativo al 95 %. Ecuador continental."
)

p_base <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = area, y = valor, fill = relleno, group = ronda)
) +
  ggplot2::geom_col(position = dodge, width = 0.76) +
  ggplot2::geom_text(
    ggplot2::aes(label = etiqueta, colour = color_txt, fontface = cara),
    position = dodge, hjust = -0.08, size = 3
  ) +
  ggplot2::facet_wrap(~indicador, ncol = 1) +
  ggplot2::scale_fill_manual(values = colores) +
  ggplot2::scale_colour_identity() +
  ggplot2::scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    limits = c(0, 0.25),
    breaks = seq(0, 0.25, by = 0.05),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Porcentaje de niñas y niños menores de 5 años",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(colour = "grey20", size = 9, face = "bold", hjust = 0),
    panel.spacing = grid::unit(1.2, "lines")
  )

spec <- house_spec("portrait")
p_final <- add_logo(p_base)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  filename = out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)
message("Guardado: ", out_path)
