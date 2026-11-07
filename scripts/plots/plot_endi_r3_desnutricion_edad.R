# ============================================================
# plot_endi_r3_desnutricion_edad.R
# Desnutrición crónica infantil por grupo de edad, ENDI rondas 1 a 3.
# Requiere: data/processed/endi_r3_desnutricion_sobrepeso.rds
# Guarda:   outputs/figures/draft_desnutricion-cronica-edad-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_endi_r3_desnutricion_edad.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/endi_r3_desnutricion_sobrepeso.rds"
out_path <- "outputs/figures/draft_desnutricion-cronica-edad-ecuador.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_endi_r3_desnutricion_sobrepeso.R")
}

plot_df <- readRDS(data_path)$dci_edad

colores <- c(
  "Menores de 2 años"        = "#006B8F",
  "De 2 a menores de 5 años" = "#A8363D",
  "Menores de 5 años"        = "grey55"
)
etiquetas_cortas <- c(
  "Menores de 2 años"        = "Menores de 2",
  "De 2 a menores de 5 años" = "De 2 a 5 años",
  "Menores de 5 años"        = "Menores de 5"
)

plot_df <- plot_df |>
  dplyr::mutate(
    grupo_label = factor(grupo_label, levels = names(colores)),
    estrella = dplyr::if_else(dplyr::coalesce(sig_r2_r3, 0) == 1, "*", ""),
    valor_txt = paste0(percent_intl(valor, accuracy = 0.1), estrella)
  )

rondas <- plot_df |>
  dplyr::distinct(ronda, periodo, fecha_media) |>
  dplyr::arrange(fecha_media) |>
  dplyr::mutate(eje = paste0("Ronda ", sub("R", "", ronda), "\n", sub("-20", "-", periodo)))

ultima <- max(plot_df$fecha_media)

# Etiquetas al final de cada línea, separadas si quedan muy juntas
fin_df <- plot_df |>
  dplyr::filter(fecha_media == ultima) |>
  dplyr::arrange(valor) |>
  dplyr::mutate(
    y_lab = valor,
    nombre = unname(etiquetas_cortas[as.character(grupo_label)])
  )
gap_min <- 0.0042
for (i in seq_len(nrow(fin_df))[-1]) {
  if (fin_df$y_lab[i] - fin_df$y_lab[i - 1] < gap_min) {
    fin_df$y_lab[i] <- fin_df$y_lab[i - 1] + gap_min
  }
}

# Valores de rondas 1 y 2 solo para la serie de menores de 2 años
previas_df <- plot_df |>
  dplyr::filter(fecha_media < ultima, grupo_label == "Menores de 2 años")

sin_dato <- data.frame(
  xmin = as.Date("2024-09-01"), xmax = as.Date("2025-07-31")
)

title_raw <- "La desnutrición crónica bajó en los menores de 2 años, pero no en los niños de 2 a 5"
subtitle_raw <- "Desnutrición crónica infantil, grupo de edad, 2022-2026"
caption_raw <- paste(
  "Fuente: INEC, Encuesta Nacional sobre Desnutrición Infantil (ENDI), rondas 1 (julio 2022 a julio 2023),",
  "2 (julio 2023 a agosto 2024) y 3 (agosto 2025 a septiembre 2026), boletín técnico de octubre de 2026.",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador.",
  "Nota: desnutrición crónica es una talla para la edad menor a -2 desviaciones estándar del patrón de la OMS.",
  "* Diferencia entre las rondas 2 y 3 significativa al 95 % según el INEC; las otras dos no lo son.",
  "La franja gris marca el período sin levantamiento. Ecuador continental."
)

p_base <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = fecha_media, y = valor, colour = grupo_label)
) +
  ggplot2::geom_rect(
    data = sin_dato,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE, fill = "grey93"
  ) +
  ggplot2::annotate(
    "text", x = as.Date("2025-02-14"), y = 0.214,
    label = "Sin\nlevantamiento", size = 2.6, colour = "grey40", lineheight = 0.95
  ) +
  ggplot2::geom_line(
    ggplot2::aes(linetype = grupo_label, linewidth = grupo_label),
    lineend = "round"
  ) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::geom_text(
    data = previas_df,
    ggplot2::aes(label = valor_txt),
    vjust = -0.9, size = 3, show.legend = FALSE
  ) +
  ggplot2::geom_text(
    data = fin_df,
    ggplot2::aes(y = y_lab, label = paste0(valor_txt, "  ", nombre)),
    hjust = 0, nudge_x = 45, size = 3, fontface = "bold", show.legend = FALSE
  ) +
  ggplot2::scale_colour_manual(values = colores) +
  ggplot2::scale_linetype_manual(values = c("solid", "solid", "22")) +
  ggplot2::scale_linewidth_manual(values = c(1.1, 0.8, 0.6)) +
  ggplot2::scale_x_date(
    breaks = rondas$fecha_media,
    labels = rondas$eje,
    expand = ggplot2::expansion(mult = c(0.05, 0.62))
  ) +
  ggplot2::scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    limits = c(0.14, 0.22),
    breaks = seq(0.14, 0.22, by = 0.02),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Porcentaje de niñas y niños",
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
