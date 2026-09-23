# ============================================================
# Contribuciones a la inflación anual de Ecuador
# Author: Daniel Sanchez
# Purpose: Grafica siete componentes de la incidencia anual del IPC nacional.
# Inputs: data/processed/inec_inflacion_contribuciones.rds
# Outputs: outputs/figures/49_contribuciones-inflacion-ecuador.png
# ============================================================

# 0. Setup ----

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "lubridate", "ragg", "scales", "stringr"))

input_path <- "data/processed/inec_inflacion_contribuciones.rds"
out_path <- "outputs/figures/49_contribuciones-inflacion-ecuador.png"
start_date <- lubridate::ymd("2024-01-01")
end_date <- lubridate::ymd("2026-08-01")
base_change_date <- lubridate::ymd("2026-07-01")

component_labels <- c(
  "Alimentos y bebidas",
  "Vivienda y servicios básicos",
  "Combustibles y lubricantes",
  "Transporte",
  "Bienes y servicios diversos",
  "Restaurantes y hoteles",
  "Otras divisiones"
)
component_colors <- c(
  "#E83E8C",
  "#57A9C4",
  "#7C9E3F",
  "#F59F48",
  "#B07AA1",
  "#EDC948",
  "#6A3FA0"
)
names(component_colors) <- stringr::str_wrap(component_labels, width = 20)
legend_line_label <- "Tasa de inflación general"
component_colors <- c(
  component_colors,
  stats::setNames(NA_character_, legend_line_label)
)

draw_key_component_or_line <- function(data, params, size) {
  if (is.null(data$fill) || is.na(data$fill)) {
    line_colour <- if (is.null(data$colour) || is.na(data$colour)) {
      "#263238"
    } else {
      data$colour
    }
    return(
      grid::segmentsGrob(
        x0 = grid::unit(0.05, "npc"),
        x1 = grid::unit(0.95, "npc"),
        y0 = grid::unit(0.5, "npc"),
        y1 = grid::unit(0.5, "npc"),
        gp = grid::gpar(col = line_colour, lwd = 2)
      )
    )
  }
  grid::rectGrob(
    gp = grid::gpar(fill = data$fill, col = NA)
  )
}

# 1. Read inputs ----

processed <- readRDS(input_path)

# 2. Prepare data ----

incidencias <- processed$incidencias |>
  dplyr::filter(
    .data$fecha >= start_date,
    .data$fecha <= end_date
  )

combustibles <- processed$combustibles |>
  dplyr::filter(
    .data$fecha >= start_date,
    .data$fecha <= end_date
  )

component_data <- dplyr::bind_rows(
  incidencias |>
    dplyr::filter(.data$codigo == "01") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Alimentos y bebidas",
      incidencia_anual = .data$incidencia_anual
    ),
  incidencias |>
    dplyr::filter(.data$codigo == "04") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Vivienda y servicios básicos",
      incidencia_anual = .data$incidencia_anual
    ),
  combustibles |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Combustibles y lubricantes",
      incidencia_anual = .data$incidencia_anual
    ),
  incidencias |>
    dplyr::filter(.data$codigo == "07", .data$fecha < base_change_date) |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Transporte",
      incidencia_anual = .data$incidencia_anual
    ),
  incidencias |>
    dplyr::filter(.data$codigo == "07", .data$fecha >= base_change_date) |>
    dplyr::left_join(combustibles, by = "fecha") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Transporte",
      incidencia_anual = .data$incidencia_anual.x -
        .data$incidencia_anual.y
    ),
  incidencias |>
    dplyr::filter(.data$codigo == "12") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Bienes y servicios diversos",
      incidencia_anual = .data$incidencia_anual
    ),
  incidencias |>
    dplyr::filter(.data$codigo == "11") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Restaurantes y hoteles",
      incidencia_anual = .data$incidencia_anual
    ),
  incidencias |>
    dplyr::filter(
      .data$codigo %in% c(
        "02", "03", "05", "06", "08", "09", "10", "13"
      )
    ) |>
    dplyr::group_by(.data$fecha) |>
    dplyr::summarise(
      component = "Otras divisiones",
      incidencia_anual = sum(.data$incidencia_anual),
      .groups = "drop"
    )
) |>
  dplyr::mutate(
    component = factor(
      .data$component,
      levels = component_labels,
      labels = names(component_colors)[seq_along(component_labels)]
    )
  )

inflacion_anual <- processed$inflacion_anual |>
  dplyr::filter(
    .data$fecha >= start_date,
    .data$fecha <= end_date
  )

stack_bounds <- component_data |>
  dplyr::group_by(.data$fecha) |>
  dplyr::summarise(
    positive_total = sum(
      .data$incidencia_anual[.data$incidencia_anual > 0],
      na.rm = TRUE
    ),
    negative_total = sum(
      .data$incidencia_anual[.data$incidencia_anual < 0],
      na.rm = TRUE
    ),
    .groups = "drop"
  )

value_range <- range(
  c(
    stack_bounds$positive_total,
    stack_bounds$negative_total,
    inflacion_anual$inflacion_anual
  ),
  na.rm = TRUE
)
range_padding <- max(diff(value_range) * 0.08, 0.1)
y_limits <- c(
  value_range[1] - range_padding,
  value_range[2] + range_padding
)

# 3. Calculate estimates ----

title_raw <- "Tras el repunte de vivienda, los combustibles\n ganan peso en la inflación"
subtitle_raw <- "Contribuciones a la inflación (IPC), por componente, 2024-2026"
caption_raw <- paste(
  "Fuente: INEC, Índice de Precios al Consumidor, corte agosto de 2026.",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador.",
  "Las barras muestran puntos porcentuales y la línea la inflación anual.",
  "Antes de julio de 2026, transporte se muestra como división completa.",
  "Combustibles y lubricantes corresponde a la clase 0722 del INEC",
  "desde julio de 2026.",
  "Desde julio de 2026, las contribuciones se calculan con las series",
  "empalmadas y las ponderaciones de la nueva canasta; son estimaciones",
  "reconstruidas que se ajustan al total de inflación publicado."
)

p_base <- ggplot(
  component_data,
  aes(
    x = .data$fecha,
    y = .data$incidencia_anual,
    fill = .data$component
  )
) +
  geom_col(width = 25, alpha = 0.94) +
  geom_line(
    data = inflacion_anual,
    aes(
      x = .data$fecha,
      y = .data$inflacion_anual,
      group = 1
    ),
    linewidth = 0.65,
    colour = "#263238",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = data.frame(
      x = start_date,
      y = y_limits[1],
      legend = legend_line_label
    ),
    aes(
      x = .data$x,
      y = .data$y,
      fill = .data$legend
    ),
    colour = "#263238",
    shape = 21,
    size = 0,
    inherit.aes = FALSE,
    show.legend = TRUE,
    key_glyph = draw_key_component_or_line
  ) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
  geom_vline(
    xintercept = base_change_date,
    colour = "grey45",
    linetype = "dashed",
    linewidth = 0.35
  ) +
  scale_fill_manual(
    values = component_colors,
    breaks = names(component_colors),
    name = "Componente"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y",
    guide = guide_axis(minor.ticks = TRUE),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(floor(y_limits[1]), ceiling(y_limits[2]), by = 1),
    labels = label_number_intl(accuracy = 0.1, suffix = "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = title_raw,
    subtitle = wrap_subtitle_house(subtitle_raw, width = 62),
    x = NULL,
    y = "Puntos porcentuales",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  theme(
    axis.text.x = element_text(size = 6.2),
    axis.minor.ticks.x.bottom = element_line(colour = "grey45", linewidth = 0.25),
    axis.minor.ticks.length.x = grid::unit(1.2, "mm"),
    axis.text.y = element_text(size = 6.4),
    axis.title.y = element_text(size = 7),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "grey60", linewidth = 0.3),
    legend.position = "bottom",
    legend.title = element_text(size = 6.2),
    legend.text = element_text(size = 5.1, lineheight = 0.9),
    legend.key.size = grid::unit(0.25, "cm"),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.box = "vertical",
    legend.box.spacing = grid::unit(0.05, "cm"),
    legend.box.margin = margin(t = -3, unit = "pt"),
    plot.margin = margin(6, 10, 4, 8)
  ) +
  guides(
    fill = guide_legend(
      ncol = 3,
      byrow = TRUE,
      order = 1,
      override.aes = list(linewidth = 1.2)
    )
  )

# 4. Write outputs ----

spec <- house_spec("portrait")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(
  filename = out_path,
  plot = house_apply_logo(
    p_base,
    "portrait",
    x = 0.89,
    y = 0.075,
    width = 0.07,
    height = 0.07
  ),
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)
message("Guardado: ", out_path)
