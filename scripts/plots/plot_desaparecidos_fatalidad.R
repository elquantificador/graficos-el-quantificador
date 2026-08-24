# plot_desaparecidos_fatalidad.R
# Renderiza la proporción anual de denuncias que permanecen sin resolver.
# Requiere: data/processed/desaparecidos_fatalidad.rds
# Guarda: outputs/figures/39_a_desapariciones-sin-resolver-ecuador.png

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg"))

in_path <- "data/processed/desaparecidos_fatalidad.rds"
out_path <- "outputs/figures/39_a_desapariciones-sin-resolver-ecuador.png"

processed <- readRDS(in_path)
plot_data <- processed$data |>
  filter(situacion_actual == "DESAPARECIDO") |>
  mutate(
    estado = "Permanece desaparecido"
  )

end_labels <- plot_data |>
  filter(anio == max(anio)) |>
  mutate(
    x_label = anio + 0.30,
    y_label = porcentaje + 0.0018,
    label = paste0(
      "Permanece\ndesaparecido\n",
      percent_intl(porcentaje, accuracy = 0.1)
    )
  )

caption_raw <- paste0(
  "Fuente: Portal de Datos Abiertos y Subsecretaría de Estudios y Estadística de la Seguridad del Ministerio del Interior. ",
  "Elaboración: Eddie Tomalá para El Quantificador. ",
  "Nota: el porcentaje se calcula sobre el total de denuncias ingresadas al sistema en cada año con estado disponible; " ,
  "los casos ENCONTRADO permanecen en el denominador. " ,
  "Se usan años completos de 2017 a 2025."
)

p <- ggplot(plot_data, aes(x = anio, y = porcentaje, color = estado, group = estado)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.5) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.1, shape = 16) +
  geom_text(
    data = end_labels,
    aes(x = x_label, y = y_label, label = label, color = estado),
    hjust = 0,
    size = 2.7,
    fontface = "bold",
    lineheight = 0.95,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Permanece desaparecido" = "#146C94"
    )
  ) +
  scale_x_continuous(
    breaks = 2017:2025,
    limits = c(2017, 2026.2)
  ) +
  scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    limits = c(0, 0.12),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = wrap_title_house("Cada vez más denuncias de desaparición permanecen sin resolver"),
    subtitle = wrap_subtitle_house("Denuncias anuales que permanecen en estado desaparecido, 2017–2025"),
    x = "A\u00F1o de desaparici\u00F3n",
    y = "Porcentaje del total de denuncias",
    caption = wrap_caption_house(caption_raw)
  ) +
  coord_cartesian(clip = "off") +
  theme_quantificador() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey85", linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 7),
    plot.margin = margin(8, 42, 6, 16)
  )

spec <- house_spec("portrait")
p_final <- house_apply_logo(p, "portrait", x = 0.88, y = 0.07)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
ggsave(
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
