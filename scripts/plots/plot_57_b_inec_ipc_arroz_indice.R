# ============================================================
# plot_57_b_inec_ipc_arroz_indice.R
# Índice nacional de precios al consumidor del arroz y general, enero de 2015
# a junio de 2026 (base 2014 = 100).
# Requiere: data/processed/inec_ipc_arroz.rds
# Guarda:   outputs/figures/57_b_arroz-indice-precios-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_inec_ipc_arroz.R
#   Rscript scripts/plots/plot_57_b_inec_ipc_arroz_indice.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

out_path <- "outputs/figures/57_b_arroz-indice-precios-ecuador.png"
color_arroz <- "#D97729"

indice <- readRDS("data/processed/inec_ipc_arroz.rds")$indice

arroz <- dplyr::filter(indice, serie == "Arroz")
pico <- dplyr::slice_max(arroz, indice_base_2014, n = 1)
ultimo <- dplyr::slice_max(arroz, fecha, n = 1)
caida <- ultimo$indice_base_2014 / pico$indice_base_2014 - 1

format_indice <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

title_text <- "Antes del alza de octubre, el arroz llevaba dos años abaratándose"
subtitle_text <- "Índice de precios al consumidor del arroz y general, nacional, enero de 2015 a junio de 2026"
caption_text <- wrap_caption_house(paste(
  "Fuente: INEC, Índice de Precios al Consumidor, base 2014 = 100,",
  "índices nacionales por subclase hasta junio de 2026.",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador.",
  "Nota: Entre febrero de 2024 y junio de 2026, el índice del arroz bajó",
  paste0(formatC(abs(caida) * 100, format = "f", digits = 1, decimal.mark = ","), "%."),
  "La serie termina en junio de 2026 porque en julio el INEC cambió el año",
  "base del IPC. En la nueva base, el arroz subió 14,95% en octubre frente a",
  "septiembre de 2026; ese dato no se muestra porque los meses de julio a",
  "septiembre en la nueva base no están en esta serie."
))

p_base <- ggplot(indice, aes(x = fecha, y = indice_base_2014, colour = serie)) +
  geom_hline(yintercept = 100, colour = "grey80", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(data = pico, size = 1.8) +
  geom_point(data = ultimo, size = 1.8) +
  annotate("text", x = pico$fecha, y = pico$indice_base_2014 + 3,
           label = format_indice(pico$indice_base_2014), size = 3, colour = color_arroz) +
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
