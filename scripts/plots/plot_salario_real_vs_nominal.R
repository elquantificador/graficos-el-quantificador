# ============================================================
# plot_salario_real_vs_nominal.R
# Genera el gráfico de sueldo promedio nominal y real.
# Requiere: data/processed/salario_ipc_series.rds
# Guarda:   figures/salario_real_vs_nominal.png
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
      serie %in% c("Sueldo privado", "Sueldo público") ~ 0.32,
      TRUE ~ 0.72
    ),
    line_width = dplyr::case_when(
      serie %in% c("Sueldo privado", "Sueldo público") ~ 0.55,
      TRUE ~ 0.85
    )
  )

caption_txt <- paste0(
  "Fuente: INEC, Índice de Precios al Consumidor (IPC) y Registro Estadístico de Empleo en la Seguridad Social\n",
  "(REESS). Ajuste por inflación con IPC general. Elaboración: El Quantificador de Laboratorio LIDE."
)

p_base <- ggplot2::ggplot(
  salario_plot,
  ggplot2::aes(
    x = fecha,
    y = valor,
    color = serie,
    alpha = line_alpha,
    linewidth = line_width
  )
) +
  ggplot2::geom_line() +
  ggplot2::scale_color_manual(values = c(
    "Sueldo privado ajustado por inflación" = "#ef9f4e",
    "Sueldo público ajustado por inflación" = "#2D7DB3",
    "Sueldo privado" = "#ef9f4e",
    "Sueldo público" = "#2D7DB3"
  )) +
  ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_linewidth_identity() +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",", prefix = "$", accuracy = 1),
    breaks = scales::breaks_width(100),
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    title = "Los empleados públicos ganan más,\npero también pierden más por la inflación",
    subtitle = "Comparación de sueldos promedio públicos y privados,\ncon ajustes para la inflación, 2019–2026",
    x = "",
    y = "Sueldo promedio (USD)",
    color = "",
    caption = caption_txt
  ) +
  theme_quantificador() +
  ggplot2::theme(
    plot.subtitle = ggplot2::element_text(size = 9.5, lineheight = 1.1, hjust = 0),
    plot.caption = ggplot2::element_text(size = 5.5, lineheight = 1.05, hjust = 0, margin = ggplot2::margin(t = 3)),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.title.y = ggplot2::element_text(
      hjust = 0,
      vjust = 1.8,
      margin = ggplot2::margin(r = 10, b = 8)
    ),
    legend.position = "bottom",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.byrow = TRUE,
    legend.text = ggplot2::element_text(size = 6.2, colour = "grey20"),
    legend.key.width = grid::unit(4, "mm"),
    legend.key.height = grid::unit(4, "mm"),
    legend.spacing.x = grid::unit(3, "mm"),
    legend.box.margin = ggplot2::margin(-22, 0, 4, -52),
    plot.margin = ggplot2::margin(12, 32, 4, 16)
  ) +
  ggplot2::guides(
    color = ggplot2::guide_legend(nrow = 2, byrow = TRUE, override.aes = list(linewidth = 1.1, alpha = 1))
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.90, y = 0.24, width = 0.09, height = 0.09)
ggplot2::ggsave(
  "figures/salario_real_vs_nominal.png",
  p_final,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  device = ragg::agg_png
)

message("Guardado: figures/salario_real_vs_nominal.png")
