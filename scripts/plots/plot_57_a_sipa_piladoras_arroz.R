# ============================================================
# plot_57_a_sipa_piladoras_arroz.R
# Precios de compra de arroz en cáscara y de venta de arroz pilado en
# piladoras, grano largo, enero de 2022 a octubre de 2026.
# Requiere: data/processed/sipa_piladoras_arroz.rds
# Guarda:   outputs/figures/57_a_arroz-precios-piladoras-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_sipa_piladoras_arroz.R
#   Rscript scripts/plots/plot_57_a_sipa_piladoras_arroz.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tibble", "ggplot2", "scales", "ragg"))

out_path <- "outputs/figures/57_a_arroz-precios-piladoras-ecuador.png"
serie_compra <- "Compra de arroz en cáscara (saca de 200 lb)"
serie_venta <- "Venta de arroz pilado (quintal de 100 lb)"
colores <- c("#D97729", "#2D7DB3")
names(colores) <- c(serie_compra, serie_venta)

plot_data <- readRDS("data/processed/sipa_piladoras_arroz.rds") |>
  dplyr::filter(fecha >= as.Date("2022-01-01"))

ultimo <- dplyr::filter(plot_data, fecha == max(fecha))

format_usd <- function(x) paste0("$", formatC(x, format = "f", digits = 2, decimal.mark = ","))

# Eventos: fechas de NOAA (El Niño), prensa y MAG (cierre de Colombia, inundaciones)
eventos <- tibble::tribble(
  ~evento,                ~inicio,        ~fin,           ~ymin, ~ymax, ~fill,
  "Inundaciones en la Costa", "2023-01-01",   "2023-05-31",   -Inf,  Inf,   "grey85",
  "El Niño\n2023-24",     "2023-06-01",   "2024-05-31",   -Inf,  Inf,   "#DCEAF5",
  "Cierre de\nColombia",  "2026-02-24",   "2026-07-25",   -Inf,  Inf,   "#F6DDD0",
  "El Niño\n2026",        "2026-06-11",   "2026-10-31",   -Inf,  Inf,   "#DCEAF5"
) |>
  dplyr::mutate(inicio = as.Date(inicio), fin = as.Date(fin),
                medio = inicio + (fin - inicio) / 2,
                x_etiqueta = c(medio[1], medio[2], inicio[3], medio[4]),
                y_etiqueta = c(58.5, 56.5, 56.5, 56.5),
                hjust_etiqueta = c(1, 0.5, 1.05, 0.5),
                angulo = c(90, 0, 0, 0))

title_text <- "En octubre, el arroz se encareció en las piladoras tras dos años de caída"
subtitle_text <- "Precio en piladoras, arroz grano largo, 2022-2026"
caption_text <- wrap_caption_house(paste(
  "Fuente: MDEP, SIPA, precios de piladoras; NOAA.",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador.",
  "Nota: Promedio simple de los cantones que reportan cada mes; compra en cáscara",
  "solo en Guayas. Cierre de Colombia: 24 de febrero al 25 de julio de 2026.",
  "Inundaciones: fechas aproximadas."
))

p_base <- ggplot(plot_data, aes(x = fecha, y = precio_promedio_usd, colour = serie)) +
  geom_rect(
    data = eventos,
    aes(xmin = inicio, xmax = fin, ymin = ymin, ymax = ymax, fill = fill),
    inherit.aes = FALSE, alpha = 0.6
  ) +
  geom_text(
    data = eventos,
    aes(x = x_etiqueta, y = y_etiqueta, label = evento, hjust = hjust_etiqueta, angle = angulo),
    inherit.aes = FALSE, size = 2.3, colour = "grey30", lineheight = 0.85, vjust = 0
  ) +
  scale_fill_identity() +
  geom_line(linewidth = 0.7) +
  geom_point(data = ultimo, size = 1.8) +
  geom_text(
    data = ultimo,
    aes(label = format_usd(precio_promedio_usd)),
    hjust = -0.12,
    size = 2.8,
    show.legend = FALSE
  ) +
  annotate("text", x = as.Date("2024-08-15"), y = 50, hjust = 0, size = 2.7,
           lineheight = 0.9, colour = colores[[serie_compra]],
           label = "Compra de arroz en cáscara\n(saca de 200 lb)") +
  annotate("text", x = as.Date("2022-02-01"), y = 22.5, hjust = 0, size = 2.7,
           lineheight = 0.9, colour = colores[[serie_venta]],
           label = "Venta de arroz pilado\n(quintal de 100 lb)") +
  scale_colour_manual(values = colores, guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = label_dollar_intl(accuracy = 1),
                     breaks = seq(20, 50, by = 10)) +
  coord_cartesian(ylim = c(20, 60), clip = "off") +
  labs(
    title = wrap_title_house(title_text),
    subtitle = wrap_subtitle_house(subtitle_text),
    caption = caption_text,
    x = NULL,
    y = "Dólares por unidad"
  ) +
  theme_quantificador()

p_final <- add_logo(p_base)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300, device = ragg::agg_png)
message("Guardado: ", out_path)
