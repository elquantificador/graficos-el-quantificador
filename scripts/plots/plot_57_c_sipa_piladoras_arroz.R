# ============================================================
# plot_57_c_sipa_piladoras_arroz.R
# Precios de compra de arroz en cáscara y de venta de arroz pilado en
# piladoras, grano largo, enero de 2022 a octubre de 2026.
# Requiere: data/processed/sipa_piladoras_arroz.rds
# Guarda:   outputs/figures/57_c_arroz-precios-piladoras-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_sipa_piladoras_arroz.R
#   Rscript scripts/plots/plot_57_c_sipa_piladoras_arroz.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

out_path <- "outputs/figures/57_c_arroz-precios-piladoras-ecuador.png"
serie_compra <- "Compra de arroz en cáscara (saca de 200 lb)"
serie_venta <- "Venta de arroz pilado (quintal de 100 lb)"
colores <- c("#D97729", "#2D7DB3")
names(colores) <- c(serie_compra, serie_venta)

plot_data <- readRDS("data/processed/sipa_piladoras_arroz.rds") |>
  dplyr::filter(fecha >= as.Date("2022-01-01"))

ultimo <- dplyr::filter(plot_data, fecha == max(fecha))
minimos_2026 <- plot_data |>
  dplyr::filter(anio == 2026, fecha < max(fecha)) |>
  dplyr::group_by(serie) |>
  dplyr::slice_min(precio_promedio_usd, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()
septiembre <- dplyr::filter(plot_data, anio == 2026, mes == 9)

format_usd <- function(x) paste0("USD ", formatC(x, format = "f", digits = 2, decimal.mark = ","))
format_pct <- function(x) paste0(formatC(x * 100, format = "f", digits = 0, decimal.mark = ","), "%")

cambio <- function(s, base) {
  ultimo$precio_promedio_usd[ultimo$serie == s] / base$precio_promedio_usd[base$serie == s] - 1
}

title_text <- "En octubre, el arroz se encareció en las piladoras tras dos años de caída"
subtitle_text <- "Precio promedio de compra de arroz en cáscara y de venta de arroz pilado en piladoras, grano largo, enero de 2022 a octubre de 2026"
caption_text <- wrap_caption_house(paste(
  "Fuente: Ministerio de Desarrollo Económico y Productivo, Sistema de Información",
  "Pública Agropecuaria (SIPA), precios mensuales de piladoras, actualización de",
  "noviembre de 2026.",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador.",
  "Nota: Promedio simple de los cantones con reporte en cada mes, en dólares por",
  "unidad de venta; el número de cantones cambia entre meses. La compra de arroz en",
  "cáscara se reporta solo en piladoras de Guayas, entre 2 y 8 cantones por mes en 2026. Entre septiembre y octubre de 2026,",
  "el precio de compra del arroz en cáscara subió",
  format_pct(cambio(serie_compra, septiembre)),
  "y el de venta del arroz pilado,",
  paste0(format_pct(cambio(serie_venta, septiembre)), ".")
))

p_base <- ggplot(plot_data, aes(x = fecha, y = precio_promedio_usd, colour = serie)) +
  geom_line(linewidth = 0.7) +
  geom_point(data = ultimo, size = 1.8) +
  geom_text(
    data = ultimo,
    aes(label = format_usd(precio_promedio_usd)),
    hjust = 1.1,
    vjust = c(-0.8, 1.8)[match(ultimo$serie, c(serie_compra, serie_venta))],
    size = 2.8,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = colores, labels = function(x) x) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = label_dollar_intl(accuracy = 1),
                     limits = c(20, NA)) +
  coord_cartesian(clip = "off") +
  labs(
    title = wrap_title_house(title_text),
    subtitle = wrap_subtitle_house(subtitle_text),
    caption = caption_text,
    x = NULL,
    y = "Dólares por unidad",
    colour = NULL
  ) +
  guides(colour = guide_legend(ncol = 1)) +
  theme_quantificador() +
  theme(legend.position = "top", legend.justification = "left",
        legend.text = element_text(size = 7))

p_final <- add_logo(p_base)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300, device = ragg::agg_png)
message("Guardado: ", out_path)
