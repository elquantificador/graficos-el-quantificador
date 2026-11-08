# ============================================================
# plot_homicidios_menores_b_evolucion.R
# Víctimas menores de 18 años de muertes violentas intencionales,
# enero a septiembre de cada año, 2014-2026.
# Requiere: data/processed/homicidios_menores.rds
# Guarda:   outputs/figures/draft_homicidios-menores-b-evolucion-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_homicidios_menores_b_evolucion.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/homicidios_menores.rds"
out_path <- "outputs/figures/draft_homicidios-menores-b-evolucion-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_homicidios_menores.R")
}

anual <- readRDS(data_path)$anual

colores <- c("2014-2021" = "grey65", "2022-2025" = "#006B8F", "2026" = "#A8363D")

v25 <- anual$menores[anual$anio == 2025]
v26 <- anual$menores[anual$anio == 2026]
variacion <- v26 / v25 - 1
sin_edad_26 <- anual$pct_edad_sin_dato[anual$anio == 2026]

title_raw <- paste0(
  format(v26, big.mark = ".", decimal.mark = ","), " menores de edad fueron víctimas de homicidio, ",
  round(variacion * 100), "% más que en 2025"
)
subtitle_raw <- "Víctimas menores de 18 años, enero a septiembre, 2014-2026"
caption_raw <- paste(
  "Fuente: Ministerio del Interior, registros de muertes violentas intencionales (asesinato, homicidio,",
  "femicidio y sicariato), enero a septiembre de cada año, 2014 a 2026.",
  "Elaboración: Odalis Clemente para El Quantificador de Laboratorio LIDE.",
  "Nota: se cuentan víctimas de 0 a 17 años. Se excluyen los registros sin edad válida",
  paste0("(", percent_intl(sin_edad_26, accuracy = 1), " de los casos de 2026).")
)

p_base <- ggplot2::ggplot(anual, ggplot2::aes(x = anio, y = menores, fill = grupo)) +
  ggplot2::geom_col(width = 0.72) +
  ggplot2::geom_text(
    ggplot2::aes(label = label_number_intl()(menores)),
    vjust = -0.5, size = 2.6, colour = "grey20"
  ) +
  ggplot2::annotate(
    "text", x = 2026, y = v26 * 1.2,
    label = paste0("+", round(variacion * 100), "%"),
    size = 3.5, fontface = "bold", colour = "#A8363D"
  ) +
  ggplot2::scale_fill_manual(values = colores) +
  ggplot2::scale_x_continuous(breaks = seq(2014, 2026, by = 2)) +
  ggplot2::scale_y_continuous(
    labels = label_number_intl(),
    expand = ggplot2::expansion(mult = c(0, 0.12))
  ) +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Número de víctimas",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador()

spec <- house_spec("portrait")
p_final <- add_logo(p_base, y = 0.23)

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
