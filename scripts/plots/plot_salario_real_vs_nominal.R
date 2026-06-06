# ============================================================
# plot_salario_real_vs_nominal.R
# Genera el gráfico de sueldo promedio nominal y real.
# Requiere: data/processed/salario_ipc_series.rds
# Guarda:   outputs/figures/11_salarios_publico-privado_inflacion-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_salario_real_vs_nominal.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "ggplot2", "scales", "ragg"))

series <- readRDS("data/processed/salario_ipc_series.rds")

salario_plot <- series$salario_sector_real |>
  dplyr::select(fecha, sector, sueldo_nominal, sueldo_real) |>
  tidyr::pivot_longer(
    cols = c(sueldo_nominal, sueldo_real),
    names_to = "tipo",
    values_to = "valor"
  ) |>
  dplyr::mutate(
    serie = dplyr::case_when(
      sector == "Empleo privado" & tipo == "sueldo_real" ~ "Sueldo privado ajustado por inflación",
      sector == "Empleo público" & tipo == "sueldo_real" ~ "Sueldo público ajustado por inflación",
      sector == "Empleo privado" & tipo == "sueldo_nominal" ~ "Sueldo privado",
      sector == "Empleo público" & tipo == "sueldo_nominal" ~ "Sueldo público"
    ),
    serie = factor(
      serie,
      levels = c(
        "Sueldo privado ajustado por inflación",
        "Sueldo público ajustado por inflación",
        "Sueldo privado",
        "Sueldo público"
      )
    ),
    line_alpha = dplyr::case_when(
      serie %in% c("Sueldo privado", "Sueldo público") ~ 0.72,
      TRUE ~ 0.72
    ),
    line_width = dplyr::case_when(
      serie %in% c("Sueldo privado", "Sueldo público") ~ 0.55,
      TRUE ~ 0.85
    ),
    line_type = dplyr::case_when(
      serie %in% c("Sueldo privado", "Sueldo público") ~ "31",
      TRUE ~ "solid"
    )
  )

label_df <- salario_plot |>
  dplyr::group_by(serie) |>
  dplyr::filter(fecha == max(fecha, na.rm = TRUE)) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    label = dplyr::case_when(
      serie == "Sueldo privado ajustado por inflación" ~ "Salario privado\najustado por inflación",
      serie == "Sueldo público ajustado por inflación" ~ "Salario público\najustado por inflación",
      serie == "Sueldo privado" ~ "Salario privado\nnominal",
      serie == "Sueldo público" ~ "Salario público\nnominal"
    ),
    fecha_label = fecha,
    fecha_label = dplyr::case_when(
      serie == "Sueldo privado ajustado por inflación" ~ fecha + 20,
      TRUE ~ fecha
    ),
    valor_label = dplyr::case_when(
      serie == "Sueldo privado ajustado por inflación" ~ valor - 12,
      serie == "Sueldo público ajustado por inflación" ~ valor - 12,
      serie == "Sueldo privado" ~ valor + 22,
      serie == "Sueldo público" ~ valor - 14
    )
  )

caption_txt <- paste0(
  "Fuente: INEC (IPC) y Registro Estadístico de Empleo en la Seguridad Social (REESS). Ajuste por\n",
  "inflación con IPC general. Elaboración: El Quantificador de Laboratorio LIDE."
)

p_base <- ggplot2::ggplot(
  salario_plot,
  ggplot2::aes(
    x = fecha,
    y = valor,
    color = serie,
    alpha = line_alpha,
    linewidth = line_width,
    linetype = line_type
  )
) +
  ggplot2::geom_line() +
  ggplot2::geom_text(
    data = label_df,
    ggplot2::aes(x = fecha_label, y = valor_label, label = label),
    hjust = -0.03,
    vjust = 0.5,
    size = 2.2,
    fontface = "bold",
    lineheight = 1,
    color = "black",
    show.legend = FALSE
  ) +
  ggplot2::scale_color_manual(values = c(
    "Sueldo privado ajustado por inflación" = "#ef9f4e",
    "Sueldo público ajustado por inflación" = "#2D7DB3",
    "Sueldo privado" = "#ef9f4e",
    "Sueldo público" = "#2D7DB3"
  )) +
  ggplot2::scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_linewidth_identity() +
  ggplot2::scale_linetype_identity() +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",", prefix = "$", accuracy = 1),
    breaks = scales::breaks_width(100),
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    title = "Los empleados públicos ganan más,\npero también pierden más por la inflación",
    subtitle = "Comparación de sueldos promedio públicos y privados,\ncon ajustes para la inflación, 2019-2026",
    x = "",
    y = "Salario promedio (USD)",
    caption = caption_txt
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  theme_quantificador() +
  ggplot2::theme(
    plot.subtitle = ggplot2::element_text(size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = ggplot2::element_text(size = 6, lineheight = 1.1, hjust = 0, margin = ggplot2::margin(t = 2)),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.title.y = ggplot2::element_text(
      colour = "grey20",
      hjust = 0.5,
      vjust = 0.5,
      margin = ggplot2::margin(r = 10, b = 8)
    ),
    plot.margin = ggplot2::margin(10, 78, 0, 12)
  )

dir.create("outputs/figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.11)
ggplot2::ggsave(
  "outputs/figures/11_salarios_publico-privado_inflacion-ecuador.png",
  p_final,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  device = ragg::agg_png
)

message("Guardado: outputs/figures/11_salarios_publico-privado_inflacion-ecuador.png")

