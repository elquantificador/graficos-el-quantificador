# ============================================================
# plot_enemdu_nini_razones_sexo.R
# Author: Valeria Lizeth Marcayata Ojeda; adaptación de El Quantificador
# Purpose: Grafica las razones para no estudiar ni trabajar entre jóvenes NINI, por sexo y nivel educativo.
# Inputs:  data/processed/enemdu_nini_razones_sexo.rds
# Outputs: outputs/figures/37_ninis-dos-realidades-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_nini_razones_sexo.R
# ============================================================

# 0. Setup ----
source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "stringr", "tibble"))
# set.seed(42)

data_path <- file.path(
  "data",
  "processed",
  "enemdu_nini_razones_sexo.rds"
)
out_path <- file.path(
  "outputs",
  "figures",
  "37_ninis-dos-realidades-ecuador.png"
)

spec <- house_spec("portrait")
center_gap <- 28
x_limit <- 98
study_start_y <- 14
work_start_y <- 5.7
study_header_y <- 14.9
work_header_y <- 6.6
sex_header_y <- 15.8
callout_y <- 16.7
bar_half_height <- 0.32

education_palette <- c(
  "Ninguno / Alfabetización" = "#DBEBED",
  "Educación básica" = "#88BABE",
  "Bachillerato" = "#2C6E78",
  "Superior" = "#0E2E36"
)

sex_colours <- c(
  "Hombres" = "#1E4A7B",
  "Mujeres" = "#C2548A"
)

# 1. Load data ----
if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_enemdu_nini_razones_sexo.R")
}

chart_data <- readRDS(data_path)

# 2. Prepare chart data ----
# La categoría de recursos tecnológicos representa menos de 0,1% y se integra
# en "Otra razón" para mantener legible el gráfico vertical.
study_data <- chart_data$razones_estudio |>
  mutate(
    razon = if_else(
      razon == "Recursos tecnológicos",
      "Otra razón",
      razon
    )
  ) |>
  summarise(
    porcentaje = sum(porcentaje, na.rm = TRUE),
    .by = c(sexo, nivel_educativo, razon)
  ) |>
  mutate(seccion = "Estudio")

work_data <- chart_data$razones_trabajo |>
  transmute(
    sexo,
    nivel_educativo,
    razon,
    porcentaje,
    seccion = "Trabajo"
  )

plot_data <- bind_rows(study_data, work_data)

study_order <- study_data |>
  summarise(
    porcentaje = sum(porcentaje, na.rm = TRUE),
    .by = razon
  ) |>
  arrange(desc(porcentaje)) |>
  pull(razon)

work_order <- work_data |>
  summarise(
    porcentaje = sum(porcentaje, na.rm = TRUE),
    .by = razon
  ) |>
  arrange(desc(porcentaje)) |>
  pull(razon)

reason_positions <- bind_rows(
  tibble(
    seccion = "Estudio",
    razon = study_order,
    y = study_start_y - seq_along(study_order) + 1
  ),
  tibble(
    seccion = "Trabajo",
    razon = work_order,
    y = work_start_y - seq_along(work_order) + 1
  )
) |>
  mutate(
    razon_etiqueta = case_when(
      razon == "Enfermedad o discapacidad" ~
        "Enferm./discap.",
      razon == "Sin deseos o necesidad" ~
        "Sin deseo/neces.",
      razon == "Enfermedad o incapacidad" ~
        "Enferm./incap.",
      razon == "Quehaceres del hogar" ~
        "Quehaceres",
      TRUE ~ razon
    )
  )

plot_data <- plot_data |>
  inner_join(
    reason_positions |> select(seccion, razon, y),
    by = join_by(seccion, razon),
    relationship = "many-to-one",
    unmatched = "error"
  ) |>
  arrange(seccion, razon, sexo, nivel_educativo) |>
  mutate(
    acumulado = cumsum(porcentaje),
    acumulado_anterior = lag(acumulado, default = 0),
    .by = c(seccion, razon, sexo)
  ) |>
  mutate(
    xmin = if_else(
      sexo == "Mujeres",
      center_gap + acumulado_anterior,
      -center_gap - acumulado
    ),
    xmax = if_else(
      sexo == "Mujeres",
      center_gap + acumulado,
      -center_gap - acumulado_anterior
    ),
    ymin = y - bar_half_height,
    ymax = y + bar_half_height
  )

bar_totals <- plot_data |>
  summarise(
    porcentaje = sum(porcentaje, na.rm = TRUE),
    y = first(y),
    .by = c(seccion, razon, sexo)
  ) |>
  mutate(
    x = if_else(
      sexo == "Mujeres",
      center_gap + porcentaje + 1.5,
      -center_gap - porcentaje - 1.5
    ),
    hjust = if_else(sexo == "Mujeres", 0, 1),
    etiqueta = case_when(
      porcentaje < 0.1 ~ "<0,1%",
      porcentaje < 1 ~
        label_number_intl(accuracy = 0.1, suffix = "%")(porcentaje),
      TRUE ~ label_number_intl(accuracy = 1, suffix = "%")(porcentaje)
    )
  )

section_headers <- tibble(
  x = -x_limit + 2,
  y = c(study_header_y, work_header_y),
  etiqueta = c(
    "¿Por qué dejaron de estudiar?",
    "¿Por qué no trabajan?"
  )
)

sex_headers <- tibble(
  sexo = factor(
    c("Hombres", "Mujeres"),
    levels = c("Hombres", "Mujeres")
  ),
  x = c(-61, 61),
  y = sex_header_y,
  etiqueta = c("Hombres", "Mujeres")
)

total_nini_label <- label_number_intl(accuracy = 1)(
  chart_data$metadata$total_nini
)
women_share_label <- label_number_intl(accuracy = 1, suffix = "%")(
  chart_data$metadata$women_share
)
callout_label <- str_c(
  total_nini_label,
  " jóvenes no estudian ni trabajan; ",
  women_share_label,
  " son mujeres."
) |>
  str_wrap(width = 75)

title_txt <- wrap_title_house(
  "NINIs: una misma condición, dos realidades"
)

subtitle_txt <- wrap_subtitle_house(
  paste(
    "Distribución porcentual de las razones para no estudiar ni trabajar,",
    "por sexo y nivel educativo, Ecuador, 2025"
  )
)

caption_txt <- wrap_caption_house(
  paste(
    "Fuente: INEC, ENEMDU anual 2025.",
    "Elaboración: Valeria Lizeth Marcayata Ojeda; adaptación de El Quantificador.",
    "Nota: NINI se refiere a personas de 15 a 29 años que no asisten a clases",
    "y no tienen empleo. Cada lado y sección suma 100%; las respuestas se",
    "agruparon para facilitar la lectura."
  )
)

# 3. Build chart ----
p_base <- ggplot() +
  geom_vline(
    xintercept = c(-center_gap, center_gap),
    colour = "grey70",
    linewidth = 0.35
  ) +
  geom_rect(
    data = plot_data,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = nivel_educativo
    ),
    colour = "white",
    linewidth = 0.25
  ) +
  geom_text(
    data = reason_positions,
    aes(x = 0, y = y, label = razon_etiqueta),
    size = 3,
    lineheight = 0.95,
    colour = "grey20"
  ) +
  geom_text(
    data = bar_totals,
    aes(x = x, y = y, label = etiqueta, hjust = hjust),
    size = 3,
    fontface = "bold",
    colour = "grey20"
  ) +
  geom_text(
    data = section_headers,
    aes(x = x, y = y, label = etiqueta),
    hjust = 0,
    size = 3,
    fontface = "bold",
    colour = "grey20"
  ) +
  geom_text(
    data = sex_headers,
    aes(x = x, y = y, label = etiqueta, colour = sexo),
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = -x_limit + 2,
    y = callout_y,
    label = callout_label,
    hjust = 0,
    vjust = 0.5,
    size = 3,
    fontface = "bold",
    colour = "grey30"
  ) +
  scale_fill_manual(
    values = education_palette,
    breaks = names(education_palette),
    labels = c(
      "Sin nivel / alfab.",
      "Educación básica",
      "Bachillerato",
      "Superior"
    ),
    name = "Nivel educativo alcanzado",
    drop = FALSE
  ) +
  scale_colour_manual(values = sex_colours) +
  scale_x_continuous(
    breaks = c(
      -center_gap - 60,
      -center_gap - 40,
      -center_gap - 20,
      -center_gap,
      center_gap,
      center_gap + 20,
      center_gap + 40,
      center_gap + 60
    ),
    labels = c("60%", "40%", "20%", "0%", "0%", "20%", "40%", "60%"),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(1, 17.1),
    breaks = NULL,
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(-x_limit, x_limit), clip = "off") +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = NULL,
    caption = caption_txt
  ) +
  theme_quantificador() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6.5),
    legend.key.width = grid::unit(0.42, "cm"),
    legend.key.height = grid::unit(0.3, "cm"),
    legend.spacing.x = grid::unit(0.08, "cm")
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE,
      title.position = "top"
    )
  )

p_final <- house_apply_logo(p_base, "portrait")

# 4. Export ----
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
ggsave(
  filename = out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)
message("Guardado: ", out_path)
invisible(sessionInfo())
