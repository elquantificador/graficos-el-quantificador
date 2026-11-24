# ============================================================
# plot_57_c_inec_ipc_arroz_indice.R
# Índice nacional de precios al consumidor del arroz y general, enero de 2015
# a junio de 2026 (base 2014 = 100).
# Requiere: data/processed/inec_ipc_arroz.rds
# Guarda:   outputs/figures/57_c_arroz-indice-precios-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_inec_ipc_arroz.R
#   Rscript scripts/plots/plot_57_c_inec_ipc_arroz_indice.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tibble", "ggplot2", "scales", "ragg"))

out_path <- "outputs/figures/57_c_arroz-indice-precios-ecuador.png"
color_arroz <- "#D97729"

indice <- readRDS("data/processed/inec_ipc_arroz.rds")$indice

arroz <- dplyr::filter(indice, serie == "Arroz")
pico <- dplyr::slice_max(arroz, indice_base_2014, n = 1)
ultimo <- dplyr::slice_max(arroz, fecha, n = 1)

eventos <- tibble::tribble(
  ~evento,                 ~inicio,       ~fin,          ~fill,
  "El Niño\n2015-16",      "2015-03-01",  "2016-05-31",  "#DCEAF5",
  "Inundaciones\n2023",    "2023-01-01",  "2023-05-31",  "grey85",
  "El Niño\n2023-24",      "2023-06-01",  "2024-05-31",  "#DCEAF5",
  "Cierre de\nColombia",   "2026-02-24",  "2026-06-30",  "#F6DDD0"
) |>
  dplyr::mutate(inicio = as.Date(inicio), fin = as.Date(fin),
                x_etiqueta = c(inicio[1] + (fin[1] - inicio[1]) / 2, inicio[2],
                               fin[3] + 40, fin[4]),
                hjust_etiqueta = c(0.5, 1.05, 0, 1),
                y_etiqueta = c(140, 140, 140, 92))

format_indice <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

title_text <- "Antes del alza de octubre, el arroz llevaba dos años abaratándose"
subtitle_text <- "IPC del arroz y general, nacional, enero 2015 a junio 2026"
caption_text <- wrap_caption_house(paste(
  "Fuente: INEC, IPC, base 2014 = 100; NOAA.",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador.",
  "Nota: La serie termina en junio de 2026 porque en julio el INEC cambió el",
  "año base del IPC. El Niño según NOAA; inundaciones: fechas aproximadas."
))

p_base <- ggplot(indice, aes(x = fecha, y = indice_base_2014, colour = serie)) +
  geom_rect(data = eventos, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, alpha = 0.6) +
  geom_text(data = eventos,
            aes(x = x_etiqueta, y = y_etiqueta, label = evento, hjust = hjust_etiqueta),
            inherit.aes = FALSE, size = 2.3, colour = "grey30", lineheight = 0.85, vjust = 1) +
  scale_fill_identity() +
  geom_hline(yintercept = 100, colour = "grey80", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(data = pico, size = 1.8) +
  geom_point(data = ultimo, size = 1.8) +
  annotate("text", x = pico$fecha - 60, y = pico$indice_base_2014,
           label = format_indice(pico$indice_base_2014), size = 3, colour = color_arroz,
           hjust = 1) +
  annotate("text", x = ultimo$fecha, y = ultimo$indice_base_2014 - 4,
           label = format_indice(ultimo$indice_base_2014), size = 3,
           colour = color_arroz, hjust = 0.7) +
  annotate("text", x = as.Date("2016-01-01"), y = 126, label = "Arroz",
           colour = color_arroz, size = 3, fontface = "bold", hjust = 0) +
  annotate("text", x = as.Date("2019-06-01"), y = 110, label = "IPC general",
           colour = "grey45", size = 3, fontface = "bold", hjust = 0) +
  scale_colour_manual(values = c(Arroz = color_arroz, `IPC general` = "grey55")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.03))) +
  scale_y_continuous(labels = label_number_intl(accuracy = 1)) +
  coord_cartesian(clip = "off") +
  labs(
    title = wrap_title_house(title_text),
    subtitle = wrap_subtitle_house(subtitle_text),
    caption = caption_text,
    x = NULL,
    y = "Índice (2014 = 100)"
  ) +
  theme_quantificador()

p_final <- add_logo(p_base)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300, device = ragg::agg_png)
message("Guardado: ", out_path)
