# ============================================================
# plot_58_fred_sbu_precio_capital.R
# Salario básico unificado frente al precio productor de maquinaria,
# computadoras y software, 2007 a 2026 (índice, 2007 = 100).
# Requiere: data/processed/fred_precio_capital.rds
# Guarda:   outputs/figures/58_salario-basico-precio-capital-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_fred_precio_capital.R
#   Rscript scripts/plots/plot_58_fred_sbu_precio_capital.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "stringr", "ragg"))

out_path <- "outputs/figures/58_salario-basico-precio-capital-ecuador.png"
azul <- "#2D7DB3"
gris <- "grey45"

precio_capital <- readRDS("data/processed/fred_precio_capital.rds")

# Cada serie se nombra al final de su línea, con su valor de 2026.
etiquetas <- precio_capital |>
  dplyr::filter(anio == 2026) |>
  dplyr::mutate(
    etiqueta = stringr::str_wrap(
      paste0(serie, ": ", format(round(indice, 1), nsmall = 1, decimal.mark = ",")),
      14
    )
  )

p_base <- ggplot(
  precio_capital,
  aes(x = anio, y = indice, colour = serie, linewidth = serie)
) +
  annotate(
    "segment",
    x = 2007, xend = 2026, y = 100, yend = 100,
    colour = "grey70", linetype = "22", linewidth = 0.3
  ) +
  geom_line() +
  geom_text(
    data = etiquetas,
    aes(label = etiqueta),
    hjust = 0, nudge_x = 0.5, size = 3, lineheight = 0.95
  ) +
  scale_colour_manual(
    values = c(
      "Salario básico" = azul,
      "Maquinaria industrial" = gris,
      "Software" = gris,
      "Computadoras" = gris
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "Salario básico" = 1,
      "Maquinaria industrial" = 0.6,
      "Software" = 0.6,
      "Computadoras" = 0.6
    )
  ) +
  scale_x_continuous(
    breaks = c(2007, 2012, 2017, 2022, 2026),
    limits = c(2007, 2034)
  ) +
  scale_y_continuous(breaks = c(0, 100, 200, 300), limits = c(0, 300)) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      "Desde 2007, el salario básico casi se triplicó\n",
      "y las computadoras se abarataron 54%"
    ),
    subtitle = "Evolución de precios y salario básico, 2007 = 100",
    caption = wrap_caption_house(
      paste(
        "Fuente: U.S. Bureau of Labor Statistics vía FRED (WPU11,",
        "PCU334111334111, PCU511210511210); Ministerio del Trabajo.",
        "Elaboración: Daniel Sánchez para El Quantificador de Laboratorio",
        "LIDE. Nota: precios productor de Estados Unidos, en dólares;",
        "promedios anuales, con 2026 de enero a octubre. Cada serie vale 100",
        "en 2007: 150 significa 50% más que en 2007 y 50, la mitad."
      )
    ),
    x = NULL,
    y = NULL
  ) +
  theme_quantificador() +
  theme(
    plot.title = element_text(size = 10, lineheight = 1.05),
    plot.subtitle = element_text(size = 7.5)
  )

p_final <- add_logo(p_base, y = 0.17)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(
  out_path, plot = p_final, width = 4, height = 5, dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)
