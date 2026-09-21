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
start_date <- lubridate::ymd("2022-01-01")
end_date <- lubridate::ymd("2026-08-01")
base_change_date <- lubridate::ymd("2026-07-01")

component_labels <- c(
  "Alimentos y bebidas",
  "Vivienda y servicios básicos",
  "Gasolina",
  "Resto del transporte",
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

gasolina <- processed$gasolina |>
  dplyr::filter(
    .data$fecha >= start_date,
    .data$fecha <= end_date
  )

component_data <- dplyr::bind_rows(
  incidencias |>
    dplyr::filter(.data$codigo_ccif == "01") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Alimentos y bebidas",
      incidencia_anual = .data$incidencia_anual
    ),
  incidencias |>
    dplyr::filter(.data$codigo_ccif == "04") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Vivienda y servicios básicos",
      incidencia_anual = .data$incidencia_anual
    ),
  gasolina |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Gasolina",
      incidencia_anual = .data$incidencia_gasolina
    ),
  incidencias |>
    dplyr::filter(.data$codigo_ccif == "07") |>
    dplyr::left_join(gasolina, by = "fecha") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Resto del transporte",
      incidencia_anual = .data$incidencia_anual -
        .data$incidencia_gasolina
    ),
  incidencias |>
    dplyr::filter(.data$codigo_ccif == "12") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Bienes y servicios diversos",
      incidencia_anual = .data$incidencia_anual
    ),
  incidencias |>
    dplyr::filter(.data$codigo_ccif == "11") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Restaurantes y hoteles",
      incidencia_anual = .data$incidencia_anual
    ),
  incidencias |>
    dplyr::filter(
      .data$codigo_ccif %in% c(
        "02", "03", "05", "06", "08", "09", "10"
      )
    ) |>
    dplyr::group_by(.data$fecha) |>
    dplyr::summarise(
      component = "Otras divisiones",
      incidencia_anual = sum(.data$incidencia_anual),
      .groups = "drop"
    )
)

component_data <- component_data |>
  dplyr::mutate(
    component = factor(
      .data$component,
      levels = component_labels,
      labels = names(component_colors)[seq_along(component_labels)]
    )
  )

new_incidencias <- processed$incidencias_nueva |>
  dplyr::filter(
    .data$fecha >= base_change_date,
    .data$fecha <= end_date
  )

new_gasolina <- processed$gasolina_nueva |>
  dplyr::filter(
    .data$fecha >= base_change_date,
    .data$fecha <= end_date
  )

new_component_data <- dplyr::bind_rows(
  new_incidencias |>
    dplyr::filter(.data$codigo_ccif == "01") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Alimentos y bebidas",
      incidencia_anual = .data$incidencia_anual
    ),
  new_incidencias |>
    dplyr::filter(.data$codigo_ccif == "04") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Vivienda y servicios básicos",
      incidencia_anual = .data$incidencia_anual
    ),
  new_gasolina |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Gasolina",
      incidencia_anual = .data$incidencia_gasolina
    ),
  new_incidencias |>
    dplyr::filter(.data$codigo_ccif == "07") |>
    dplyr::left_join(new_gasolina, by = "fecha") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Resto del transporte",
      incidencia_anual = .data$incidencia_anual -
        .data$incidencia_gasolina
    ),
  new_incidencias |>
    dplyr::filter(.data$codigo_ccif == "12") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Bienes y servicios diversos",
      incidencia_anual = .data$incidencia_anual
    ),
  new_incidencias |>
    dplyr::filter(.data$codigo_ccif == "11") |>
    dplyr::transmute(
      fecha = .data$fecha,
      component = "Restaurantes y hoteles",
      incidencia_anual = .data$incidencia_anual
    ),
  new_incidencias |>
    dplyr::filter(
      .data$codigo_ccif %in% c(
        "02", "03", "05", "06", "08", "09", "10", "13"
      )
    ) |>
    dplyr::group_by(.data$fecha) |>
    dplyr::summarise(
      component = "Otras divisiones",
      incidencia_anual = sum(.data$incidencia_anual),
      .groups = "drop"
    )
)

new_line <- processed$inflacion_anual |>
  dplyr::filter(
    .data$fecha >= base_change_date,
    .data$fecha <= end_date
  )

new_component_data <- new_component_data |>
  dplyr::left_join(new_line, by = "fecha") |>
  dplyr::group_by(.data$fecha) |>
  dplyr::mutate(
    incidencia_anual = dplyr::if_else(
      .data$component == "Otras divisiones",
      .data$incidencia_anual + .data$inflacion_anual - sum(
        .data$incidencia_anual
      ),
      .data$incidencia_anual
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-inflacion_anual) |>
  dplyr::mutate(
    component = factor(
      .data$component,
      levels = component_labels,
      labels = names(component_colors)[seq_along(component_labels)]
    )
  )

component_data <- dplyr::bind_rows(
  component_data,
  new_component_data
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

title_raw <- "¿Qué explica la inflación anual en Ecuador?"
subtitle_raw <- paste(
  "Siete componentes de la inflación anual, con gasolina separada,",
  "enero de 2022 a agosto de 2026"
)
caption_raw <- paste(
  "Fuente: INEC, Índice de Precios al Consumidor, serie oficial de",
  "incidencias y series empalmadas, corte agosto de 2026. Elaboración:",
  "Daniel Sánchez Pazmiño para El Quantificador. Las barras muestran puntos",
  "porcentuales aportados por cada componente y la línea muestra la inflación",
  "anual. Hasta junio de 2026 se usa la base 2014 = 100; desde julio se usa",
  "la nueva base julio 2025 - junio 2026 = 100. La línea vertical marca el",
  "cambio de base y canasta. Gasolina se calcula con productos del INEC hasta",
  "junio de 2026 y se aproxima desde julio distribuyendo la clase 0722 según",
  "el peso de los productos de gasolina de la nueva canasta. Las otras",
  "divisiones agrupan los rubros no desagregados. Otras divisiones incorpora",
  "el residual de la nueva base para cerrar con la línea."
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
  geom_vline(
    xintercept = base_change_date,
    colour = "grey35",
    linewidth = 0.35,
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
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
  scale_fill_manual(
    values = component_colors,
    breaks = names(component_colors),
    name = "Componente"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    minor_breaks = seq(
      from = start_date,
      to = end_date,
      by = "month"
    ),
    date_labels = "%Y",
    guide = ggplot2::guide_axis(minor.ticks = TRUE),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(floor(y_limits[1]), ceiling(y_limits[2]), by = 1),
    labels = label_number_intl(accuracy = 0.1, suffix = "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    x = NULL,
    y = "Puntos porcentuales",
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  theme(
    axis.text.x = element_text(size = 6.2),
    axis.text.y = element_text(size = 6.4),
    axis.title.y = element_text(size = 7),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "grey60", linewidth = 0.3),
    axis.ticks.x = element_line(colour = "grey55", linewidth = 0.3),
    axis.minor.ticks.x.bottom = element_line(
      colour = "grey45",
      linewidth = 0.45
    ),
    axis.ticks.length.x = grid::unit(2.5, "pt"),
    axis.minor.ticks.length.x.bottom = grid::unit(2, "pt"),
    legend.position = "bottom",
    legend.title = element_text(size = 6.2),
    legend.text = element_text(size = 5.1, lineheight = 0.9),
    legend.key.size = grid::unit(0.25, "cm"),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.box = "vertical",
    legend.box.spacing = grid::unit(0.05, "cm"),
    legend.box.margin = margin(t = 2, unit = "pt"),
    plot.margin = margin(6, 10, 8, 8)
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
