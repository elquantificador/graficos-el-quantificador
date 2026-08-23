# plot_crimen_desapariciones.R
# Renderiza muertes intencionales y presupuesto policial.
# Requiere: data/processed/crimen_desapariciones.rds
# Guarda: outputs/figures/39_crimen-desapariciones-ecuador.png

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg"))

in_path <- "data/processed/crimen_desapariciones.rds"
out_path <- "outputs/figures/39_crimen-desapariciones-ecuador.png"

processed <- readRDS(in_path)
annual <- processed$data

left_axis_max <- 10000
right_axis_max <- 1.8e9
scale_factor <- left_axis_max / right_axis_max

cases <- tidyr::pivot_longer(
  annual,
  cols = muertes_intencionales,
  names_to = "serie",
  values_to = "casos"
) %>%
  mutate(
    serie = dplyr::recode(
      serie,
      muertes_intencionales = "Muertes intencionales"
    )
  )

budget <- annual %>%
  mutate(
    serie = "Gasto",
    casos = presupuesto_policial * scale_factor
  )

plot_data <- dplyr::bind_rows(cases, budget)

caption_raw <- paste0(
  "Fuente: Ministerio del Interior, registros de muertes intencionales; " ,
  "Ministerio de Economía y Finanzas, presupuesto liquidado de la Policía Nacional. ",
  "Elaboración: Ángel Alava para El Quantificador. ",
  "Nota: las muertes intencionales incluyen asesinatos, homicidios, femicidios y sicariatos. " ,
  "El presupuesto usa el eje derecho y se expresa en dólares corrientes. " ,
  "Se usan años completos de 2017 a 2025."
)

p <- ggplot(plot_data, aes(x = anio, y = casos, color = serie, linetype = serie)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_color_manual(
    values = c(
      "Muertes intencionales" = "#146C94",
      "Gasto" = "#555555"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Muertes intencionales" = "solid",
      "Gasto" = "dashed"
    )
  ) +
  scale_x_continuous(breaks = c(2017, 2019, 2021, 2023, 2025)) +
  scale_y_continuous(
    name = "Casos registrados",
    breaks = seq(0, left_axis_max, by = 2000),
    limits = c(0, left_axis_max),
    labels = label_number_intl(accuracy = 1),
    expand = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Presupuesto policial (miles de millones de USD)",
      breaks = seq(0, right_axis_max, by = 0.3e9),
      labels = scales::label_number(
        scale = 1e-9,
        accuracy = 0.1,
        decimal.mark = ","
      )
    )
  ) +
  labs(
    title = wrap_title_house("Aunque el presupuesto de la Policía aumentó 34%, las muertes intencionales aumentaron 10 veces"),
    subtitle = wrap_subtitle_house("Casos registrados y presupuesto liquidado de la Policía Nacional, 2017–2025"),
    x = "A\u00F1o",
    color = NULL,
    linetype = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just = "center",
    legend.text = element_text(size = 6.8),
    legend.key.width = grid::unit(0.55, "cm"),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    plot.subtitle = element_text(margin = margin(b = 12)),
    axis.title.y.left = element_text(hjust = 0.5),
    axis.title.y.right = element_text(size = 7, color = "grey30", hjust = 0.5, margin = margin(l = 6)),
    panel.grid.major.y = element_line(color = "grey85", linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 7),
    plot.margin = margin(8, 36, 8, 16)
  ) +
  guides(
    linetype = "none",
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linetype = c("solid", "dashed"),
        linewidth = c(1, 1)
      )
    )
  )

spec <- house_spec("portrait")
p_final <- house_apply_logo(p, "portrait", x = 0.88, y = 0.28)

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
