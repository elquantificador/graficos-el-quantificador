# ============================================================
# BCE trade with Colombia, monthly nominal values
# Author: Daniel Sanchez
# Purpose: Compare monthly imports from and exports to Colombia.
# Inputs:  data/processed/bce_comercio_colombia.rds
# Outputs: outputs/figures/51_importaciones-colombia-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_51_bce_importaciones_colombia.R
# ============================================================

# 0. Setup ----

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "lubridate", "ragg", "scales"))

input_path <- "data/processed/bce_comercio_colombia.rds"
out_path <- "outputs/figures/51_importaciones-colombia-ecuador.png"
event_date <- ymd("2026-02-01")
spanish_months <- c(
  "ene", "feb", "mar", "abr", "may", "jun",
  "jul", "ago", "sep", "oct", "nov", "dic"
)

# 1. Read inputs ----

comercio_mensual <- readRDS(input_path)

# 2. Prepare data ----

comercio_grafico <- comercio_mensual |>
  filter(.data$serie %in% c("Importaciones", "Exportaciones")) |>
  mutate(
    serie = factor(
      .data$serie,
      levels = c("Importaciones", "Exportaciones")
    )
  )

fechas_marcas <- seq.Date(
  from = floor_date(min(comercio_grafico$fecha), unit = "quarter"),
  to = max(comercio_grafico$fecha),
  by = "3 months"
)
etiquetas_fechas <- paste0(
  spanish_months[month(fechas_marcas)],
  " ",
  year(fechas_marcas)
)

etiquetas_series <- tibble(
  serie = c("Importaciones", "Exportaciones"),
  fecha = ymd("2025-06-01"),
  valor_millones_usd = c(180, 100)
)

title_raw <- paste(
  "La guerra comercial redujo las",
  "importaciones desde Colombia en 31.9%",
  "y las exportaciones en un 10%",
  sep = "\n"
)
subtitle_raw <- paste(
  "Comercio exterior con Colombia, exportaciones e importaciones,",
  "enero 2024 a julio 2026"
)
caption_raw <- paste(
  "Fuente: Banco Central del Ecuador (BCE), comercio bilateral con Colombia.",
  "Caídas: enero-julio de 2026 frente al mismo período de 2025.",
  "Elaboración: Daniel Sánchez-Pazmiño para El Quantificador."
)

# 3. Write output ----

p_base <- ggplot(
  comercio_grafico,
  aes(
    x = .data$fecha,
    y = .data$valor_millones_usd,
    colour = .data$serie,
    group = .data$serie
  )
) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_text(
    data = etiquetas_series,
    aes(label = .data$serie),
    hjust = 0.5,
    size = 3.1,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_vline(
    xintercept = event_date,
    colour = "grey45",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = event_date - days(7),
    y = Inf,
    label = "Inicio de la tasa\n1 feb. 2026",
    hjust = 1,
    vjust = 1.2,
    size = 2.5,
    colour = "grey30"
  ) +
  scale_colour_manual(
    values = c(
      "Importaciones" = "#2D7DB3",
      "Exportaciones" = "#E68632"
    ),
    name = NULL,
    breaks = c("Importaciones", "Exportaciones")
  ) +
  scale_x_date(
    breaks = fechas_marcas,
    labels = etiquetas_fechas,
    expand = expansion(mult = c(0.015, 0.015))
  ) +
  scale_y_continuous(
    labels = label_number_intl(accuracy = 1),
    breaks = c(25, 50, 100, 150, 200, 250),
    limits = c(25, NA),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = title_raw,
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Millones de USD nominales (FOB)",
    colour = NULL,
    caption = wrap_caption_house(caption_raw)
  ) +
  coord_cartesian(clip = "off") +
  theme_quantificador() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.margin = margin(6, 36, 6, 16)
  )

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.15, width = 0.09, height = 0.09)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  device = agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
