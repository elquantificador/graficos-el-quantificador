# ============================================================
# plot_56_b_homicidios_menores_provincias.R
# Víctimas menores de 18 años de muertes violentas intencionales
# por provincia, enero a septiembre de cada año, 2020-2026 acumulado.
# Requiere: data/processed/homicidios_menores.rds
# Guarda:   outputs/figures/56_b_homicidios-menores-provincias-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_56_b_homicidios_menores_provincias.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/homicidios_menores.rds"
out_path <- "outputs/figures/56_b_homicidios-menores-provincias-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_homicidios_menores.R")
}

prov <- readRDS(data_path)$provincias

plot_df <- prov |>
  dplyr::arrange(dplyr::desc(victimas)) |>
  dplyr::slice_head(n = 10) |>
  dplyr::mutate(
    destacar = dplyr::row_number() == 1,
    etiqueta = paste0(label_number_intl()(victimas), " (", percent_intl(porcentaje, accuracy = 0.1), ")")
  )

title_raw <- "Guayas concentra casi la mitad de los homicidios de menores"
subtitle_raw <- "Víctimas menores de 18 años por provincia, enero a septiembre, 2020-2026"
caption_raw <- paste(
  "Fuente: Ministerio del Interior, registros de muertes violentas intencionales (asesinato, homicidio,",
  "femicidio y sicariato), enero a septiembre de cada año, 2020 a 2026.",
  "Elaboración: Odalis Clemente para El Quantificador de Laboratorio LIDE.",
  "Nota: se cuentan víctimas de 0 a 17 años, acumuladas en los siete años. El porcentaje es la participación",
  "de cada provincia en el total nacional de víctimas menores con provincia registrada.",
  "Se muestran las diez provincias con más víctimas. Son conteos, no tasas:",
  "Guayas es la provincia más poblada."
)

p_base <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = stats::reorder(provincia, victimas), y = victimas, fill = destacar)
) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_text(
    ggplot2::aes(label = etiqueta),
    hjust = -0.08, size = 3, colour = "grey20"
  ) +
  ggplot2::scale_fill_manual(values = c("TRUE" = "#A8363D", "FALSE" = "#006B8F")) +
  ggplot2::scale_y_continuous(
    labels = label_number_intl(),
    expand = ggplot2::expansion(mult = c(0, 0.42))
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Número de víctimas",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador()

spec <- house_spec("portrait")
p_final <- add_logo(p_base, y = 0.28)

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
