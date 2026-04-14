# ============================================================
# plot_salario_vs_ipc.R
# Genera el gráfico de crecimiento interanual del sueldo
# promedio frente al costo de vida.
# Requiere: data/processed/salario_ipc_series.rds
# Guarda:   figures/salario_vs_ipc.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_salario_vs_ipc.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "forcats"))

series <- readRDS("data/processed/salario_ipc_series.rds")
plot_order <- c(
  "IPC general",
  "Alimentos y bebidas",
  "Alojamiento",
  "Transporte",
  "Gasolina y lubricantes",
  "Sueldo promedio"
)

plot_data <- series$salario_vs_ipc_index |>
  dplyr::filter(categoria %in% plot_order) |>
  dplyr::mutate(
    categoria = factor(categoria, levels = plot_order)
  )

caption_txt <- paste0(
  "Fuente: Instituto Nacional de Estadística y Censos (INEC), Índice de Precios al Consumidor (IPC), y Registro Estadístico de Empleo en la Seguridad Social (REESS).\n",
  "Periodo: 2019–", format(max(plot_data$fecha, na.rm = TRUE), "%Y"),
  ". Índice estandarizado con base enero 2019 = 100.\n",
  "Elaboración: El Quantificador de Laboratorio LIDE."
)

p_base <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = fecha, y = valor, color = categoria, group = categoria)
) +
  ggplot2::geom_hline(
    yintercept = 100,
    linetype = "dotted",
    colour = "grey60",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 1.05) +
  ggplot2::scale_color_manual(values = c(
    "IPC general" = "#2A9D8F",
    "Alimentos y bebidas" = "#E63946",
    "Alojamiento" = "#F4A261",
    "Transporte" = "#457B9D",
    "Gasolina y lubricantes" = "#7B2CBF",
    "Sueldo promedio" = "#1D3557"
  )) +
  ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(accuracy = 1),
    breaks = scales::pretty_breaks(n = 7),
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    title = "¿El costo de vida en Ecuador crece más rápido que los sueldos?",
    subtitle = "Índice base enero 2019 = 100 para IPC general, rubros clave y sueldo promedio nominal.",
    x = "",
    y = "Índice (enero 2019 = 100)",
    color = "",
    caption = caption_txt
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.title.y = ggplot2::element_text(hjust = 0),
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 6.5),
    legend.key.width = grid::unit(3, "mm"),
    legend.key.height = grid::unit(3, "mm"),
    legend.box.margin = ggplot2::margin(0, 0, 0, -20),
    plot.margin = ggplot2::margin(14, 28, 12, 16)
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.08, width = 0.1, height = 0.1)
ggplot2::ggsave(
  "figures/salario_vs_ipc.png",
  p_final,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  device = ragg::agg_png
)

message("Guardado: figures/salario_vs_ipc.png")
