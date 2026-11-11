# ============================================================
# plot_54_siniestros_transito.R
# Compara el cambio de siniestros, fallecidos y lesionados entre 2016 y 2025.
# Requiere: data/raw/siniestros/totales_nacionales_2016_2025.csv
# Guarda:   outputs/figures/54_siniestros-transito-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_54_siniestros_transito.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "readr", "ragg", "scales"))

input_file <- "data/raw/siniestros/totales_nacionales_2016_2025.csv"
out_path <- "outputs/figures/54_siniestros-transito-ecuador.png"

totales <- readr::read_csv(input_file, show_col_types = FALSE)
base <- dplyr::filter(totales, .data$year == 2016)
actual <- dplyr::filter(totales, .data$year == 2025)

indicadores <- tibble::tibble(
  indicador = c("Siniestros registrados", "Fallecidos", "Lesionados"),
  total_2016 = c(base$siniestros, base$fallecidos, base$lesionados),
  total_2025 = c(actual$siniestros, actual$fallecidos, actual$lesionados)
) |>
  dplyr::mutate(
    cambio_pct = (.data$total_2025 / .data$total_2016 - 1) * 100,
    indicador = factor(.data$indicador, levels = rev(.data$indicador)),
    etiqueta = paste0(
      ifelse(.data$cambio_pct > 0, "+", ""),
      scales::number(.data$cambio_pct, accuracy = 0.1, decimal.mark = ","),
      "%"
    )
  )

caption_raw <- paste(
  "Fuente: INEC, estadística de siniestros de tránsito, tabulado provincial 11.",
  "Elaboración: Carolina Espinosa para El Quantificador de Laboratorio LIDE.",
  "Nota: los datos de 2025 son provisionales. Cada indicador se compara con su total de 2016."
)

p_base <- ggplot2::ggplot(
  indicadores,
  ggplot2::aes(x = .data$cambio_pct, y = .data$indicador)
) +
  ggplot2::geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
  ggplot2::geom_col(width = 0.48, fill = "#2D6A9F") +
  ggplot2::geom_text(
    ggplot2::aes(
      x = ifelse(.data$cambio_pct < 0, .data$cambio_pct + 1.2, .data$cambio_pct - 1.2),
      label = .data$etiqueta,
      hjust = ifelse(.data$cambio_pct < 0, 0, 1)
    ),
    colour = "white",
    size = 3,
    fontface = "bold"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(-40, 30, by = 10),
    labels = scales::label_percent(
      accuracy = 1,
      scale = 1,
      suffix = "%",
      decimal.mark = ","
    ),
    limits = c(-45, 35),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    title = wrap_title_house(
      "Desde 2016, en Ecuador hay menos accidentes de tránsito, pero más personas fallecen en ellos"
    ),
    subtitle = wrap_subtitle_house(
      "Cambio de los totales nacionales entre 2016 y 2025"
    ),
    x = "Variación respecto a 2016 (%)",
    y = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador()

p_final <- add_logo(p_base, y = 0.15)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
print(indicadores |> dplyr::select(indicador, total_2016, total_2025, cambio_pct))
