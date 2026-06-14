# ============================================================
# plot_enemdu_transicion_laboral_desempleo_zona.R
# Genera un Sankey de transición laboral de personas
# desempleadas por zona de residencia en la ENEMDU.
# Requiere: data/processed/enemdu_transicion_laboral_desempleo_zona_2022_2023.rds
# Guarda:   outputs/figures/22_transicion-laboral_desempleo-zona-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_transicion_laboral_desempleo_zona.R
# ============================================================

options(scipen = 999)

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "stringr"))

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

input_path <- "data/processed/enemdu_transicion_laboral_desempleo_zona_2022_2023.rds"
out_path <- "outputs/figures/22_transicion-laboral_desempleo-zona-ecuador.png"

fmt_n <- function(x) {
  label_number_intl(accuracy = 1)(round(x))
}

fmt_pct <- function(x) {
  percent_intl(x, accuracy = 0.1)
}

stack_stage <- function(labels, values, gap, total_height, node_type) {
  stage_height <- sum(values) + gap * max(length(values) - 1, 0)
  cursor <- total_height - (total_height - stage_height) / 2

  rows <- lapply(seq_along(values), function(i) {
    ymax <- cursor
    ymin <- ymax - values[[i]]
    cursor <<- ymin - gap

    data.frame(
      label = labels[[i]],
      value = values[[i]],
      ymin = ymin,
      ymax = ymax,
      ymid = (ymin + ymax) / 2,
      node_type = node_type,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
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

make_flow_polygon <- function(x0, x1, from_range, to_range, fill, flow_id, alpha = 0.82) {
  t <- seq(0, 1, length.out = 50)
  s <- t^2 * (3 - 2 * t)
  xs <- x0 + (x1 - x0) * t
  upper <- from_range$ymax + (to_range$ymax - from_range$ymax) * s
  lower <- from_range$ymin + (to_range$ymin - from_range$ymin) * s

  data.frame(
    x = c(xs, rev(xs)),
    y = c(upper, rev(lower)),
    fill = fill,
    alpha = alpha,
    flow_id = flow_id,
    stringsAsFactors = FALSE
  )
}

chart_data <- readRDS(input_path)
flows_df <- chart_data$flows %>%
  mutate(
    zona = factor(.data$zona, levels = c("Urbano", "Rural")),
    destino = factor(
      .data$destino,
      levels = c(
        "Obtuvo empleo en 2023",
        "Continuó desempleado en 2023",
        "Salió de la fuerza laboral en 2023"
      )
    )
  )

root_count <- chart_data$root$count[[1]]
zone_df <- chart_data$zones %>% arrange(match(.data$zona, c("Urbano", "Rural")))
outcome_df <- chart_data$outcomes %>%
  arrange(match(.data$destino, c(
    "Obtuvo empleo en 2023",
    "Continuó desempleado en 2023",
    "Salió de la fuerza laboral en 2023"
  )))

zone_gap <- root_count * 0.03
outcome_gap <- root_count * 0.02
plot_height <- max(
  root_count,
  sum(zone_df$count) + zone_gap * (nrow(zone_df) - 1),
  sum(outcome_df$count) + outcome_gap * (nrow(outcome_df) - 1)
) * 1.03

root_stage <- stack_stage("Desempleados 2022", root_count, gap = 0, total_height = plot_height, node_type = "root")
zone_stage <- stack_stage(zone_df$zona, zone_df$count, gap = zone_gap, total_height = plot_height, node_type = "zone")
outcome_stage <- stack_stage(outcome_df$destino, outcome_df$count, gap = outcome_gap, total_height = plot_height, node_type = "outcome")

root_allocs <- allocate_within(root_stage$ymin[[1]], root_stage$ymax[[1]], zone_df$count)
zone_targets <- lapply(seq_len(nrow(zone_stage)), function(i) list(ymin = zone_stage$ymin[[i]], ymax = zone_stage$ymax[[i]]))
zone_splits <- lapply(zone_df$zona, function(z) {
  values <- flows_df %>%
    filter(.data$zona == z) %>%
    arrange(.data$destino) %>%
    pull(.data$count)
  y_min <- zone_stage$ymin[[which(zone_stage$label == z)]]
  y_max <- zone_stage$ymax[[which(zone_stage$label == z)]]
  allocate_within(y_min, y_max, values)
})
names(zone_splits) <- zone_df$zona

outcome_allocs <- lapply(outcome_df$destino, function(dest) {
  values <- flows_df %>%
    filter(.data$destino == dest) %>%
    arrange(.data$zona) %>%
    pull(.data$count)
  y_min <- outcome_stage$ymin[[which(outcome_stage$label == dest)]]
  y_max <- outcome_stage$ymax[[which(outcome_stage$label == dest)]]
  allocate_within(y_min, y_max, values)
})
names(outcome_allocs) <- outcome_df$destino

root_xmin <- 1.22
root_xmax <- 1.60
zone_xmin <- 3.22
zone_xmax <- 3.60
outcome_xmin <- 6.06
outcome_xmax <- 6.44
outcome_label_x <- 6.48

zone_palette <- c("Urbano" = "#9FC3DE", "Rural" = "#F2B36A")
outcome_palette <- c(
  "Obtuvo empleo en 2023" = "#F0A145",
  "Continuó desempleado en 2023" = "#5AA6D6",
  "Salió de la fuerza laboral en 2023" = "#77B37A"
)

root_flows <- dplyr::bind_rows(lapply(seq_len(nrow(zone_df)), function(i) {
  make_flow_polygon(
    x0 = root_xmax,
    x1 = zone_xmin,
    from_range = root_allocs[[i]],
    to_range = zone_targets[[i]],
    fill = unname(zone_palette[[zone_df$zona[[i]]]]),
    flow_id = paste0("root-", i)
  )
}))

zone_to_outcome_flows <- dplyr::bind_rows(lapply(levels(flows_df$zona), function(z) {
  zone_rows <- flows_df %>%
    filter(.data$zona == z) %>%
    arrange(.data$destino)

  dplyr::bind_rows(lapply(seq_len(nrow(zone_rows)), function(i) {
    dest <- as.character(zone_rows$destino[[i]])
    target_index <- which(levels(flows_df$zona) == z)
    outcome_index <- which(levels(flows_df$zona) == z)
    alloc_index <- which(levels(flows_df$destino) == dest)

    make_flow_polygon(
      x0 = zone_xmax,
      x1 = outcome_xmin,
      from_range = zone_splits[[z]][[i]],
      to_range = outcome_allocs[[dest]][[which(levels(flows_df$zona) == z)]],
      fill = unname(outcome_palette[[dest]]),
      flow_id = paste0("zone-", z, "-", i)
    )
  }))
}))

node_rects <- dplyr::bind_rows(
  dplyr::mutate(root_stage, xmin = root_xmin, xmax = root_xmax),
  dplyr::mutate(zone_stage, xmin = zone_xmin, xmax = zone_xmax),
  dplyr::mutate(outcome_stage, xmin = outcome_xmin, xmax = outcome_xmax)
)

root_label <- paste0(
  "Desempleados\n",
  fmt_n(root_count), " | 100%"
)

zone_labels <- zone_df %>%
  transmute(
    label = .data$zona,
    label_text = paste0(.data$zona, "\n", fmt_n(.data$count), "\n", fmt_pct(.data$share_total))
  )

zone_stage <- zone_stage %>%
  left_join(zone_labels, by = "label")

outcome_labels <- outcome_df %>%
  transmute(
    label = .data$destino,
    label_text = dplyr::case_when(
      .data$destino == "Obtuvo empleo en 2023" ~ paste0(
        "Obtuvo empleo\n",
        fmt_n(.data$count), " | ", fmt_pct(.data$share_total)
      ),
      .data$destino == "Continuó desempleado en 2023" ~ paste0(
        "Siguió desempleado\n",
        fmt_n(.data$count), " | ", fmt_pct(.data$share_total)
      ),
      TRUE ~ paste0(
        "Salió de la fuerza laboral\n",
        fmt_n(.data$count), " | ", fmt_pct(.data$share_total)
      )
    )
  )

outcome_stage <- outcome_stage %>%
  left_join(outcome_labels, by = "label")

caption_raw <- "Fuente: INEC, Matrices de Transición Laboral de la ENEMDU, trimestre IV 2022 y trimestre IV 2023. Elaboración: El Quantificador de Laboratorio LIDE. Nota: El ancho de los flujos representa la cantidad de personas que transitaron entre estados laborales. La población que salió de la fuerza laboral no trabaja y no está disponible para trabajar por cualquier motivo. El gráfico muestra la transición de quienes estaban desempleados en 2022, desagregada por zona de residencia."

build_chart <- function(orientation) {
  spec <- house_spec(orientation)
  title_txt <- if (orientation == "landscape") {
    wrap_title_house("La mayoría de desempleados no encuentra trabajo después de un año", width = spec$title_wrap)
  } else {
    "La mayoría de desempleados no encuentra\ntrabajo después de un año"
  }
  subtitle_txt <- wrap_subtitle_house("Transiciones desde el desempleo, por zona, 2022-2023", width = spec$subtitle_wrap)
  caption_txt  <- if (orientation == "landscape") {
    stringr::str_wrap(caption_raw, width = landscape_wrap_for_size(5.5))
  } else {
    wrap_caption_house(caption_raw, width = 82)
  }

  ggplot() +
  geom_polygon(
    data = root_flows,
    aes(x = .data$x, y = .data$y, group = .data$flow_id, fill = .data$fill, alpha = .data$alpha),
    colour = NA
  ) +
  geom_polygon(
    data = zone_to_outcome_flows,
    aes(x = .data$x, y = .data$y, group = .data$flow_id, fill = .data$fill, alpha = .data$alpha),
    colour = NA
  ) +
  geom_rect(
    data = node_rects,
    aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
    fill = "#F8F9FA",
    colour = "#495057",
    linewidth = 0.35
  ) +
  annotate(
    "text",
    x = (root_xmin + root_xmax) / 2,
    y = root_stage$ymid[[1]],
    label = root_label,
    size = 3.0,
    lineheight = 1.05,
    colour = "#212529"
  ) +
  geom_text(
    data = zone_stage,
    aes(x = (zone_xmin + zone_xmax) / 2, y = .data$ymid, label = .data$label_text),
    size = 2.9,
    lineheight = 1.02,
    colour = "#212529"
  ) +
  geom_text(
    data = outcome_stage,
    aes(x = outcome_label_x, y = .data$ymid, label = .data$label_text),
    hjust = 0,
    size = 2.8,
    lineheight = 1.02,
    colour = "#212529"
  ) +
  coord_cartesian(xlim = c(0.80, 7.92), ylim = c(0, plot_height), clip = "off") +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = NULL,
    caption = caption_txt
  ) +
  theme_classic(base_size = 9) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = if (orientation == "landscape") margin(8, 16, 6, 12) else margin(8, 60, 6, 12),
    plot.title = element_text(colour = "grey20", size = 12.5, face = "bold", hjust = 0, lineheight = 1.02),
    plot.subtitle = element_text(colour = "grey30", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "grey30", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 6)),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  p_final <- house_apply_logo(build_chart(orientation), orientation, y = 0.15)
  dest <- house_out_path(out_path, orientation)
  ggsave(
    filename = dest,
    plot = p_final,
    width = spec$width,
    height = spec$height,
    dpi = spec$dpi,
    device = ragg::agg_png,
    bg = "white"
  )
  message("Guardado: ", dest)
}
