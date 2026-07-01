# ============================================================
# plot_enemdu_transicion_laboral_desempleo_zona.R
# Genera un Sankey de transición laboral de personas
# desempleadas en la ENEMDU.
# Requiere: data/processed/enemdu_transicion_laboral_desempleo_zona_2022_2023.rds
# Guarda:   outputs/figures/28_transicion-laboral_desempleo-zona-ecuador.png
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
out_path <- "outputs/figures/28_transicion-laboral_desempleo-zona-ecuador.png"

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
    destino = factor(
      .data$destino,
      levels = c(
        "Obtuvo empleo en 2023",
        "Continuó desempleado en 2023",
        "Ya no busca trabajo en 2023"
      )
    )
  ) %>%
  arrange(.data$destino)

root_count <- chart_data$root$count[[1]]
outcome_df <- chart_data$outcomes %>%
  arrange(match(.data$destino, c(
    "Obtuvo empleo en 2023",
    "Continuó desempleado en 2023",
    "Ya no busca trabajo en 2023"
  )))

outcome_gap <- root_count * 0.02
plot_height <- max(
  root_count,
  sum(outcome_df$count) + outcome_gap * (nrow(outcome_df) - 1)
) * 1.03

root_stage <- stack_stage("Desempleados 2022", root_count, gap = 0, total_height = plot_height, node_type = "root")
outcome_stage <- stack_stage(outcome_df$destino, outcome_df$count, gap = outcome_gap, total_height = plot_height, node_type = "outcome")

root_allocs <- allocate_within(root_stage$ymin[[1]], root_stage$ymax[[1]], outcome_df$count)
outcome_targets <- lapply(seq_len(nrow(outcome_stage)), function(i) list(ymin = outcome_stage$ymin[[i]], ymax = outcome_stage$ymax[[i]]))

x_shift <- 0.18

root_xmin <- 1.70 + x_shift
root_xmax <- 2.08 + x_shift
outcome_xmin <- 5.55 + x_shift
outcome_xmax <- 5.93 + x_shift
outcome_label_x <- 5.97 + x_shift

outcome_palette <- c(
  "Obtuvo empleo en 2023" = "#F0A145",
  "Continuó desempleado en 2023" = "#5AA6D6",
  "Ya no busca trabajo en 2023" = "#77B37A"
)

root_flows <- dplyr::bind_rows(lapply(seq_len(nrow(outcome_df)), function(i) {
  dest <- as.character(outcome_df$destino[[i]])
  make_flow_polygon(
    x0 = root_xmax,
    x1 = outcome_xmin,
    from_range = root_allocs[[i]],
    to_range = outcome_targets[[i]],
    fill = unname(outcome_palette[[dest]]),
    flow_id = paste0("root-", i)
  )
}))

node_rects <- dplyr::bind_rows(
  dplyr::mutate(root_stage, xmin = root_xmin, xmax = root_xmax),
  dplyr::mutate(outcome_stage, xmin = outcome_xmin, xmax = outcome_xmax)
)

root_label <- paste0("Desempleados\n", fmt_n(root_count), " | 100%")

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
        "Ya no busca trabajo\n",
        fmt_n(.data$count), " | ", fmt_pct(.data$share_total)
      )
    )
  )

outcome_stage <- outcome_stage %>%
  left_join(outcome_labels, by = "label")

caption_raw <- "Fuente: INEC, Matrices de Transición Laboral de la ENEMDU, trimestre IV 2022 y trimestre IV 2023. Elaboración: Angel Alava para el Quantificador. Nota: El ancho de los flujos representa la cantidad de personas que transitaron entre estados laborales. Ya no busca trabajo incluye a quienes dejaron de buscar empleo y salieron de la fuerza laboral. El gráfico muestra la transición de quienes estaban desempleados en 2022."

build_chart <- function() {
  spec <- house_spec("portrait")
  title_txt <- wrap_title_house(
    "La mayoría de desempleados no encuentra trabajo después de un año",
    width = 46
  )
  subtitle_txt <- wrap_subtitle_house("Transiciones desde el desempleo, 2022-2023", width = spec$subtitle_wrap)
  caption_txt <- wrap_caption_house(caption_raw, width = 110)

  ggplot() +
  geom_polygon(
    data = root_flows,
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
    data = outcome_stage,
    aes(x = outcome_label_x, y = .data$ymid, label = .data$label_text),
    hjust = 0,
    size = 2.8,
    lineheight = 1.02,
    colour = "#212529"
  ) +
  coord_cartesian(xlim = c(1.10, 7.50), ylim = c(0, plot_height), clip = "off") +
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
    plot.margin = margin(8, 60, 6, 12),
    plot.title = element_text(colour = "grey20", size = 12.5, face = "bold", hjust = 0, lineheight = 1.02),
    plot.subtitle = element_text(colour = "grey30", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "grey30", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 6)),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")
p_final <- house_apply_logo(build_chart(), "portrait", y = 0.15)
ggsave(
  filename = out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)
message("Guardado: ", out_path)
