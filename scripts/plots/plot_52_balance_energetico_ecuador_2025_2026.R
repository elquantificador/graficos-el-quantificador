# ============================================================
# Balance nacional de energía eléctrica, agosto 2025-julio 2026
# Author: Daniel Sanchez
# Purpose: Sigue la energía desde sus fuentes hasta sus destinos.
# Inputs:  data/processed/bnee_arconel_sankey_corte_2026_07.csv
# Outputs: outputs/figures/52_balance-energetico-ecuador.png
# ============================================================

# 0. Setup ----

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "readr", "ragg", "scales", "stringr"))

input_path <- "data/processed/bnee_arconel_sankey_corte_2026_07.csv"
out_path <- "outputs/figures/52_balance-energetico-ecuador.png"
total_node <- "Producción e importaciones"

source_order <- c(
  "Hidráulica",
  "No renovable",
  "Otras renovables e importaciones"
)
destination_order <- c(
  "Consumo total",
  "Pérdidas técnicas en distribución",
  "Pérdidas no técnicas en distribución",
  "No disponible para servicio público",
  "Pérdidas en transmisión",
  "Exportación"
)

source_colours <- c(
  "Hidráulica" = "#1F4E79",
  "No renovable" = "#E68632",
  "Otras renovables e importaciones" = "#7C9E3F"
)
destination_colours <- c(
  "Consumo total" = "#59636E",
  "Pérdidas técnicas en distribución" = "#C85450",
  "Pérdidas no técnicas en distribución" = "#E29A93",
  "No disponible para servicio público" = "#89939D",
  "Pérdidas en transmisión" = "#C6CDD3",
  "Exportación" = "#A58A63"
)

# 1. Read inputs ----

flow_data <- read_csv(input_path, show_col_types = FALSE)

# 2. Prepare data ----

source_data <- flow_data |>
  filter(.data$destino == total_node) |>
  mutate(
    origen = if_else(
      .data$origen %in% c("Otras renovables", "Importaciones"),
      "Otras renovables e importaciones",
      .data$origen
    )
  ) |>
  summarise(
    energia_gwh = sum(.data$energia_gwh, na.rm = FALSE),
    .by = origen
  ) |>
  mutate(origen = factor(.data$origen, levels = source_order)) |>
  arrange(.data$origen)

destination_data <- flow_data |>
  filter(.data$origen == total_node) |>
  mutate(destino = factor(.data$destino, levels = destination_order)) |>
  arrange(.data$destino)

total_gwh <- sum(source_data$energia_gwh, na.rm = FALSE)
consumption_share <- destination_data |>
  filter(.data$destino == "Consumo total") |>
  pull("energia_gwh") / total_gwh
distribution_loss_gwh <- destination_data |>
  filter(.data$destino %in% c(
    "Pérdidas técnicas en distribución",
    "Pérdidas no técnicas en distribución"
  )) |>
  summarise(value = sum(.data$energia_gwh)) |>
  pull("value")
technical_distribution_loss_gwh <- destination_data |>
  filter(.data$destino == "Pérdidas técnicas en distribución") |>
  pull("energia_gwh")
transmission_loss_gwh <- destination_data |>
  filter(.data$destino == "Pérdidas en transmisión") |>
  pull("energia_gwh")
physical_loss_share <- (technical_distribution_loss_gwh + transmission_loss_gwh) / total_gwh
technical_loss_share <- technical_distribution_loss_gwh / distribution_loss_gwh
source_gap <- total_gwh * 0.07
destination_gap <- total_gwh * 0.16
plot_height <- total_gwh * 2.0

stack_stage <- function(labels, values, gap, total_height, node_type) {
  stage_height <- sum(values, na.rm = FALSE) + gap * max(length(values) - 1, 0)
  cursor <- (total_height + stage_height) / 2
  rows <- lapply(seq_along(values), function(i) {
    ymax <- cursor
    ymin <- ymax - values[[i]]
    cursor <<- ymin - gap
    data.frame(
      label = as.character(labels[[i]]),
      value = values[[i]],
      ymin = ymin,
      ymax = ymax,
      ymid = (ymin + ymax) / 2,
      node_type = node_type,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(rows)
}

allocate_within <- function(ymin, ymax, values) {
  cursor <- ymax
  lapply(values, function(value) {
    upper <- cursor
    lower <- upper - value
    cursor <<- lower
    list(ymin = lower, ymax = upper)
  })
}

make_flow_polygon <- function(x0, x1, from_range, to_range, fill, flow_id, alpha = 0.7) {
  t <- seq(0, 1, length.out = 50)
  eased <- t^2 * (3 - 2 * t)
  x <- x0 + (x1 - x0) * t
  upper <- from_range$ymax + (to_range$ymax - from_range$ymax) * eased
  lower <- from_range$ymin + (to_range$ymin - from_range$ymin) * eased
  data.frame(
    x = c(x, rev(x)),
    y = c(upper, rev(lower)),
    fill = fill,
    alpha = alpha,
    flow_id = flow_id,
    stringsAsFactors = FALSE
  )
}

source_stage <- stack_stage(
  source_data$origen,
  source_data$energia_gwh,
  gap = source_gap,
  total_height = plot_height,
  node_type = "source"
)
total_stage <- stack_stage(
  total_node,
  total_gwh,
  gap = 0,
  total_height = plot_height,
  node_type = "total"
)
destination_stage <- stack_stage(
  destination_data$destino,
  destination_data$energia_gwh,
  gap = destination_gap,
  total_height = plot_height,
  node_type = "destination"
)

source_allocations <- allocate_within(
  total_stage$ymin[[1]], total_stage$ymax[[1]], source_data$energia_gwh
)
destination_allocations <- allocate_within(
  total_stage$ymin[[1]], total_stage$ymax[[1]], destination_data$energia_gwh
)

source_xmin <- 1.55
source_xmax <- 1.73
total_xmin <- 5.0
total_xmax <- 5.13
destination_xmin <- 8.45
destination_xmax <- 8.63
source_label_x <- source_xmin - 0.08
destination_label_x <- destination_xmax + 0.08

source_stage <- source_stage |>
  mutate(
    xmin = source_xmin,
    xmax = source_xmax,
    label_text = paste0(
      str_wrap(.data$label, width = 22),
      "\n",
      label_number_intl(accuracy = 1)(.data$value),
      " GWh | ",
      percent_intl(.data$value / total_gwh, accuracy = 0.1)
    )
  )

destination_stage <- destination_stage |>
  mutate(
    xmin = destination_xmin,
    xmax = destination_xmax,
    label_short = case_when(
      .data$label == "Consumo total" ~ "Consumo",
      .data$label == "Pérdidas técnicas en distribución" ~ "Técnicas",
      .data$label == "Pérdidas no técnicas en distribución" ~ "No técnicas",
      .data$label == "No disponible para servicio público" ~ paste("No disponible para", "servicio público", sep = "\n"),
      .data$label == "Pérdidas en transmisión" ~ "Transmisión",
      .data$label == "Exportación" ~ "Exportación",
      TRUE ~ .data$label
    ),
    label_text = if_else(
      .data$label == "Exportación",
      paste0(
        .data$label_short,
        "\n",
        label_number_intl(accuracy = 1)(.data$value),
        " GWh | ",
        percent_intl(.data$value / total_gwh, accuracy = 0.1)
      ),
      paste0(
        .data$label_short,
        "\n",
        label_number_intl(accuracy = 1)(.data$value),
        " GWh | ",
        percent_intl(.data$value / total_gwh, accuracy = 0.1)
      )
    )
  )

total_stage <- total_stage |>
  mutate(
    xmin = total_xmin,
    xmax = total_xmax,
    label_text = paste0(
      "Suministro total\n",
      label_number_intl(accuracy = 1)(.data$value),
      " GWh | 100%"
    ),
    label_y = .data$ymax + total_gwh * 0.09
  )

source_flows <- bind_rows(lapply(seq_len(nrow(source_stage)), function(i) {
  source_name <- as.character(source_data$origen[[i]])
  make_flow_polygon(
    x0 = source_xmax,
    x1 = total_xmin,
    from_range = list(
      ymin = source_stage$ymin[[i]],
      ymax = source_stage$ymax[[i]]
    ),
    to_range = source_allocations[[i]],
    fill = unname(source_colours[[source_name]]),
    flow_id = paste0("source-", i)
  )
}))

destination_flows <- bind_rows(lapply(seq_len(nrow(destination_stage)), function(i) {
  destination_name <- as.character(destination_data$destino[[i]])
  make_flow_polygon(
    x0 = total_xmax,
    x1 = destination_xmin,
    from_range = destination_allocations[[i]],
    to_range = list(
      ymin = destination_stage$ymin[[i]],
      ymax = destination_stage$ymax[[i]]
    ),
    fill = unname(destination_colours[[destination_name]]),
    flow_id = paste0("destination-", i)
  )
}))

node_rectangles <- bind_rows(
  source_stage,
  total_stage,
  destination_stage
)

title_raw <- paste0(
  "Ecuador consume el ",
  percent_intl(consumption_share, accuracy = 1),
  " de su suministro, pero desperdicia el ",
  percent_intl(physical_loss_share, accuracy = 1)
)
subtitle_raw <- paste0(
  "De las pérdidas en distribución, ",
  percent_intl(technical_loss_share, accuracy = 1),
  " son técnicas y ",
  percent_intl(1 - technical_loss_share, accuracy = 1),
  " no técnicas"
)
caption_raw <- "Fuente: ARCONEL, BNEE, agosto de 2025 a julio de 2026"

# 3. Write output ----

spec <- house_spec("portrait")
# The left-to-right Sankey needs extra horizontal space to keep all labels legible.
spec$width <- 10
title_text <- wrap_title_house(title_raw, width = 82)
subtitle_text <- wrap_subtitle_house(subtitle_raw, width = 100)
caption_text <- wrap_caption_house(caption_raw)

p_chart <- ggplot() +
  geom_polygon(
    data = source_flows,
    aes(x = .data$x, y = .data$y, group = .data$flow_id,
        fill = .data$fill, alpha = .data$alpha),
    colour = NA
  ) +
  geom_polygon(
    data = destination_flows,
    aes(x = .data$x, y = .data$y, group = .data$flow_id,
        fill = .data$fill, alpha = .data$alpha),
    colour = NA
  ) +
  geom_rect(
    data = node_rectangles,
    aes(xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax),
    fill = "#F8F9FA",
    colour = "#495057",
    linewidth = 0.3
  ) +
  geom_text(
    data = source_stage,
    aes(y = .data$ymid, label = .data$label_text),
    x = source_label_x,
    hjust = 1,
    size = 2.8,
    lineheight = 1.0,
    colour = "#212529"
  ) +
  geom_text(
    data = total_stage,
    aes(x = (.data$xmin + .data$xmax) / 2,
        y = .data$label_y, label = .data$label_text),
    size = 3.2,
    lineheight = 1.0,
    fontface = "bold",
    colour = "#212529"
  ) +
  geom_text(
    data = destination_stage,
    aes(
      x = .data$xmax + 0.08,
      y = .data$ymid,
      label = .data$label_text
    ),
    hjust = 0,
    size = 2.8,
    lineheight = 1.0,
    colour = "#212529"
  ) +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(-0.35, 11.25), ylim = c(0, plot_height), clip = "off") +
  labs(
    x = NULL,
    y = NULL,
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_quantificador() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(6, 36, 6, 16),
    plot.title = element_text(size = 12.5, lineheight = 1.02),
    plot.subtitle = element_text(size = 9, lineheight = 1.1),
    plot.caption = element_text(size = 6.5, lineheight = 1.1)
  )

dir.create(spec$dir, showWarnings = FALSE, recursive = TRUE)
p_final <- house_apply_logo(
  p_chart,
  "portrait",
  y = 0.04
)

ggsave(
  filename = out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = agg_png,
  bg = "white"
)

message("Guardado: ", out_path)

sessionInfo()
