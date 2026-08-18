# ============================================================
# plot_homicidios_hora.R
# Renderiza la distribución horaria de los asesinatos en Ecuador.
# Requiere: data/processed/homicidios_hora.rds
# Guarda:   outputs/figures/38_hora-asesinatos-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_homicidios_hora.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg"))

in_path <- "data/processed/homicidios_hora.rds"
out_path <- "outputs/figures/38_hora-asesinatos-ecuador.png"

processed <- readRDS(in_path)
df <- processed$data

peak <- df %>%
  slice_max(order_by = asesinatos, n = 1, with_ties = FALSE)

period_label <- paste0(
  format(as.Date(processed$metadata$period[[1]]), "%Y"),
  "–",
  format(as.Date(processed$metadata$period[[2]]), "%Y")
)
peak_label <- paste0(peak$etiqueta_hora, " (", percent_intl(peak$participacion, accuracy = 0.1), ")")

caption_raw <- paste0(
  "Fuente: Ministerio del Interior, Registro de muertes violentas, archivo de homicidios intencionales. ",
  "Elaboración: Ángel Alava para El Quantificador. ",
  "Nota: Se consideran registros tipificados como asesinato entre ", period_label,
  "; se excluyen las horas sin dato. El pico corresponde a ", peak_label, "."
)

df <- df %>%
  mutate(
    hora_factor = factor(hora, levels = 0:23),
    color_barra = if_else(hora == peak$hora, "#D96C2C", "#146C94")
  )

max_count <- max(df$asesinatos)
radial_breaks <- pretty(c(0, max_count), n = 4)
radial_breaks <- radial_breaks[radial_breaks >= 0 & radial_breaks <= max_count]
peak_center <- data.frame(
  hora_factor = factor(0, levels = 0:23),
  y = 0,
  label = paste0("Pico\n", peak$etiqueta_hora)
)

p_base <- ggplot(df, aes(x = hora_factor, y = asesinatos)) +
  geom_col(aes(fill = color_barra), width = 1, color = "white", linewidth = 0.2) +
  scale_fill_identity() +
  geom_label(
    data = peak_center,
    aes(
      x = hora_factor,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    color = "#D96C2C",
    fill = "white",
    linewidth = 0,
    fontface = "bold",
    size = 3.4,
    lineheight = 0.9
  ) +
  coord_polar(start = -pi / 24, clip = "off") +
  scale_x_discrete(
    breaks = as.character(seq(0, 23, by = 3)),
    labels = sprintf("%02d:00", seq(0, 23, by = 3))
  ) +
  scale_y_continuous(
    breaks = radial_breaks,
    labels = label_number_intl(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = wrap_title_house(paste0("La mayor cantidad de asesinatos en Ecuador se registra a las ", peak$etiqueta_hora)),
    subtitle = wrap_subtitle_house("Asesinatos por hora del d\u00EDa en Ecuador, 2017-2025"),
    caption = wrap_caption_house(caption_raw),
    x = NULL,
    y = "N\u00FAmero de asesinatos"
  ) +
  theme_quantificador() +
  theme(
    axis.text.x = element_text(size = 7, color = "grey20"),
    axis.text.y = element_text(size = 6.5, color = "grey30"),
    axis.title.y = element_text(size = 7, color = "grey30"),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.35, linetype = "dashed"),
    panel.grid.minor = element_blank(),
    # Excepción autorizada para aprovechar el espacio vertical del lienzo.
    plot.margin = margin(-12, 36, -12, 16)
  )

spec <- house_spec("portrait")
p_final <- house_apply_logo(p_base, "portrait")

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(
  out_path,
  plot = p_final,
  width = spec$width,
  # Excepción autorizada: lienzo más corto para eliminar bandas vacías.
  height = 4.2,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)
message("Guardado: ", out_path)
