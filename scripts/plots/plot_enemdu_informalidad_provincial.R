# ============================================================
# plot_enemdu_informalidad_provincial.R
# Author: Juan Diego Sotomayor Jiménez; adaptación de El Quantificador
# Purpose: Grafica la relación entre informalidad y empleo no remunerado y el ranking provincial de informalidad.
# Requiere: data/processed/enemdu_informalidad_provincial.rds
# Guarda:   outputs/figures/36_a_ranking-informalidad_provincia-ecuador.png
#           outputs/figures/36_b_informalidad-empleo-no-remunerado_provincia-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_informalidad_provincial.R
# ============================================================

# 0. Setup ----
source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))
# set.seed(42)

data_path <- "data/processed/enemdu_informalidad_provincial.rds"
scatter_out_path <- "outputs/figures/36_b_informalidad-empleo-no-remunerado_provincia-ecuador.png"
ranking_out_path <- "outputs/figures/36_a_ranking-informalidad_provincia-ecuador.png"
spec <- house_spec("portrait")
point_colour <- "#2D7DB3"
major_grid_colour <- "grey88"
major_grid_width <- 0.35

# 1. Load data ----
if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_enemdu_informalidad_provincial.R")
}

chart_data <- readRDS(data_path)
plot_data <- chart_data$provincias
national_rate <- chart_data$metadata$national_informality_2025
correlation_2025 <- chart_data$metadata$correlation_2025

# 2. Prepare annotations and text ----
label_data <- plot_data |>
  filter(
    provincia %in% c(
      "Galápagos",
      "Guayas",
      "Morona Santiago",
      "Napo",
      "Pichincha"
    )
  ) |>
  mutate(
    label_x = case_when(
      provincia == "Galápagos" ~ 18.0,
      provincia == "Guayas" ~ 53.5,
      provincia == "Morona Santiago" ~ 86.5,
      provincia == "Napo" ~ 80.5,
      provincia == "Pichincha" ~ 27.0
    ),
    label_y = case_when(
      provincia == "Galápagos" ~ 1.5,
      provincia == "Guayas" ~ 1.4,
      provincia == "Morona Santiago" ~ 44.0,
      provincia == "Napo" ~ 32.4,
      provincia == "Pichincha" ~ 6.2
    ),
    label_hjust = case_when(
      provincia %in% c("Morona Santiago", "Napo", "Pichincha") ~ 1,
      TRUE ~ 0
    )
  )

ranking_data <- plot_data |>
  arrange(informalidad_2025) |>
  mutate(
    provincia_rank = factor(provincia, levels = provincia),
    value_label = label_number_intl(
      accuracy = 0.1,
      suffix = "%"
    )(informalidad_2025)
  )

scatter_title_txt <- wrap_title_house(
  "Las provincias con más empleo informal tienen más empleo no remunerado"
)

scatter_subtitle_txt <- wrap_subtitle_house(
  "Informalidad y empleo no remunerado, por provincia, 2025"
)

ranking_title_txt <- wrap_title_house(
  "Casi el 90% de Morona Santiago trabaja en la informalidad"
)

ranking_subtitle_txt <- wrap_subtitle_house(
  "Porcentaje de personas con empleo en el sector informal, por provincia, 2025"
)

correlation_label <- label_number_intl(accuracy = 0.01)(correlation_2025)

scatter_caption_txt <- wrap_caption_house(
  paste(
    "Fuente: INEC, ENEMDU anual 2025, Boletín Técnico Nro. 03-2026, tablas 8 y 9.",
    "Elaboración: Juan Diego Sotomayor Jiménez; adaptación de El Quantificador.",
    "Nota: Cada punto representa una provincia; la correlación provincial es",
    correlation_label,
    "y no implica causalidad.",
    "Informalidad como porcentaje del empleo; empleo no remunerado como porcentaje de la PEA."
  )
)

ranking_caption_txt <- wrap_caption_house(
  paste(
    "Fuente: INEC, ENEMDU anual 2025, tabla 9.",
    "Elaboración: Juan Diego Sotomayor Jiménez; adaptación de El Quantificador.",
    "Nota: La tasa nacional de 51,5% es un promedio ponderado."
  )
)

# 3. Build scatter chart ----
p_scatter_base <- ggplot(
  plot_data,
  aes(x = informalidad_2025, y = no_remunerado_2025)
) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.7,
    linetype = "dashed",
    colour = "grey60"
  ) +
  geom_vline(
    xintercept = national_rate,
    linewidth = 0.5,
    linetype = "dotted",
    colour = "grey40"
  ) +
  geom_point(
    size = 2.2,
    shape = 21,
    stroke = 0.5,
    fill = point_colour,
    colour = "white"
  ) +
  geom_segment(
    data = label_data,
    aes(
      x = informalidad_2025,
      y = no_remunerado_2025,
      xend = label_x,
      yend = label_y
    ),
    inherit.aes = FALSE,
    linewidth = 0.3,
    colour = "grey65"
  ) +
  geom_text(
    data = label_data,
    aes(x = label_x, y = label_y, label = provincia, hjust = label_hjust),
    inherit.aes = FALSE,
    size = 3,
    colour = "grey20"
  ) +
  annotate(
    "text",
    x = national_rate + 1.2,
    y = 41,
    label = "Nacional: 51,5%",
    hjust = 0,
    vjust = 0.5,
    size = 3,
    colour = "grey30",
    lineheight = 1
  ) +
  scale_x_continuous(
    breaks = seq(20, 80, by = 20),
    labels = label_number_intl(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = seq(0, 40, by = 10),
    labels = label_number_intl(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(10, 94), ylim = c(0, 45), clip = "on") +
  labs(
    title = scatter_title_txt,
    subtitle = scatter_subtitle_txt,
    x = "Empleo en el sector informal (% del empleo)",
    y = "Empleo no remunerado (% de la PEA)",
    caption = scatter_caption_txt
  ) +
  theme_quantificador() +
  theme(
    panel.grid.major.x = element_line(
      colour = major_grid_colour,
      linewidth = major_grid_width,
      linetype = "dashed"
    ),
    panel.grid.major.y = element_line(
      colour = major_grid_colour,
      linewidth = major_grid_width,
      linetype = "dashed"
    ),
    panel.grid.minor = element_blank()
  )

p_scatter_final <- house_apply_logo(p_scatter_base, "portrait")

# 4. Build ranking chart ----
p_ranking_base <- ggplot(
  ranking_data,
  aes(x = informalidad_2025, y = provincia_rank)
) +
  geom_col(
    width = 0.68,
    fill = point_colour,
    colour = "white",
    linewidth = 0.2
  ) +
  geom_vline(
    xintercept = national_rate,
    linewidth = 0.5,
    linetype = "dotted",
    colour = "grey40"
  ) +
  geom_text(
    aes(label = value_label),
    hjust = -0.15,
    size = 3,
    colour = "grey20"
  ) +
  annotate(
    "text",
    x = national_rate + 1.2,
    y = 0.65,
    label = "Nacional: 51,5%",
    hjust = 0,
    vjust = 0.5,
    size = 3,
    colour = "grey30"
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20),
    labels = label_number_intl(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(
    name = NULL,
    expand = expansion(add = c(1, 0.4))
  ) +
  coord_cartesian(xlim = c(0, 105), clip = "on") +
  labs(
    title = ranking_title_txt,
    subtitle = ranking_subtitle_txt,
    x = "Empleo en el sector informal (% del empleo)",
    caption = ranking_caption_txt
  ) +
  theme_quantificador() +
  theme(
    panel.grid.major.x = element_line(
      colour = major_grid_colour,
      linewidth = major_grid_width,
      linetype = "dashed"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

p_ranking_final <- house_apply_logo(p_ranking_base, "portrait")

# 5. Export ----
dir.create(dirname(scatter_out_path), showWarnings = FALSE, recursive = TRUE)
ggsave(
  filename = scatter_out_path,
  plot = p_scatter_final,
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)
ggsave(
  filename = ranking_out_path,
  plot = p_ranking_final,
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)
message("Guardados: ", scatter_out_path, " y ", ranking_out_path)
invisible(sessionInfo())
