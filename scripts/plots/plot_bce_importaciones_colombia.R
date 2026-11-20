# ============================================================
# BCE trade with Colombia, monthly nominal values
# Author: Daniel Sanchez
# Purpose: Compare monthly imports from and exports to Colombia.
# Inputs:  data/processed/bce_comercio_colombia.rds
# Outputs: outputs/figures/draft_importaciones-colombia-ecuador.png
# Datos BCE actualizados a septiembre de 2026 (consulta del 20 de noviembre de 2026).
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_bce_importaciones_colombia.R
# ============================================================

# 0. Setup ----

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "lubridate", "ragg", "scales"))

input_path <- "data/processed/bce_comercio_colombia.rds"
out_path <- "outputs/figures/draft_importaciones-colombia-ecuador.png"
event_date <- ymd("2026-02-01")
end_date <- ymd("2026-06-01")
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
  "Las importaciones desde Colombia cayeron 46% con la tasa de seguridad",
  "y ya volvieron a su nivel de 2025"
)
subtitle_raw <- paste(
  "Importaciones y exportaciones mensuales con Colombia,",
  "millones de USD FOB, enero 2024 a septiembre 2026"
)
caption_raw <- paste(
  "Fuente: Banco Central del Ecuador (BCE), comercio exterior por país, enero de 2024 a septiembre de 2026.",
  "Elaboración: Daniel Sánchez-Pazmiño para El Quantificador de Laboratorio LIDE.",
  "Nota: la tasa de seguridad rigió del 1 de febrero al 31 de mayo de 2026 (30%, 50% desde marzo y 100% en mayo).",
  "La caída compara febrero-mayo de 2026 con febrero-mayo de 2025. En agosto y septiembre",
  "de 2026 las importaciones quedaron a menos de 3% de los mismos meses de 2025."
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
  annotate(
    "rect",
    xmin = event_date,
    xmax = end_date,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey85",
    alpha = 0.5
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
  annotate(
    "text",
    x = event_date + (end_date - event_date) / 2,
    y = Inf,
    label = "Tasa de\nseguridad",
    hjust = 0.5,
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
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Millones de USD nominales (FOB)",
    colour = NULL,
    caption = wrap_caption_house(caption_raw, width = 78)  # ajuste óptico: los porcentajes ensanchan las líneas
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
p_final <- add_logo(p_base, x = 0.88, y = 0.30, width = 0.09, height = 0.09)

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
