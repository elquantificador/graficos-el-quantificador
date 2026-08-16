# ============================================================
# plot_enemdu_nini_razones_sexo.R
# Author: Valeria Lizeth Marcayata Ojeda; adaptación de El Quantificador
# Purpose: Grafica las razones para no estudiar y no trabajar entre jóvenes NINI, por sexo y nivel educativo.
# Inputs:  data/processed/enemdu_nini_razones_sexo.rds
# Outputs: outputs/figures/37_a_ninis-razones-estudio-ecuador.png
#          outputs/figures/37_b_ninis-razones-trabajo-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_nini_razones_sexo.R
# ============================================================

# 0. Setup ----
source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "stringr", "tibble"))

data_path <- file.path(
  "data",
  "processed",
  "enemdu_nini_razones_sexo.rds"
)
study_out_path <- file.path(
  "outputs",
  "figures",
  "37_a_ninis-razones-estudio-ecuador.png"
)
work_out_path <- file.path(
  "outputs",
  "figures",
  "37_b_ninis-razones-trabajo-ecuador.png"
)

spec <- house_spec("portrait")
center_gap <- 31
x_limit <- 103
first_reason_y <- 13.6
sex_header_y <- 14.75
bar_half_height <- 0.36

normalize_utf8 <- function(x) {
  iconv(as.character(x), from = "UTF-8", to = "UTF-8")
}

education_palette <- c(
  "Ninguno / Alfabetización" = "#D9E8EB",
  "Educación básica" = "#8CCFE3",
  "Bachillerato" = "#2D7DB3",
  "Superior" = "#2F4B7C"
)
names(education_palette) <- normalize_utf8(names(education_palette))
education_legend_labels <- normalize_utf8(c(
  "Sin nivel / alfab.",
  "Educación básica",
  "Bachillerato",
  "Superior"
))

sex_colours <- c(
  "Hombres" = "#2D7DB3",
  "Mujeres" = "#D97729"
)

# 1. Load data ----
if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_enemdu_nini_razones_sexo.R")
}

chart_data <- readRDS(data_path)

# 2. Prepare data ----
# La categoría de recursos tecnológicos representa menos de 0,1% y se integra
# en "Otra razón" para mantener legible el gráfico vertical.
study_data <- chart_data$razones_estudio |>
  mutate(
    nivel_educativo = normalize_utf8(nivel_educativo),
    razon = normalize_utf8(razon),
    razon = if_else(
      razon == normalize_utf8("Recursos tecnológicos"),
      normalize_utf8("Otra razón"),
      razon
    )
  ) |>
  summarise(
    porcentaje = sum(porcentaje, na.rm = TRUE),
    .by = c(sexo, nivel_educativo, razon)
  )

work_data <- chart_data$razones_trabajo |>
  mutate(
    nivel_educativo = normalize_utf8(nivel_educativo),
    razon = normalize_utf8(razon)
  ) |>
  select(sexo, nivel_educativo, razon, porcentaje)

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

study_caption <- paste(
  "Fuente: INEC, ENEMDU anual 2025.",
  "Elaboración: Valeria Lizeth Marcayata Ojeda; adaptación de El Quantificador.",
  "Nota: NINI se refiere a personas de 15 a 29 años que no asisten a clases",
  "y no tienen empleo. Cada lado suma 100%; recursos tecnológicos se integró",
  "en 'Otra razón' por representar menos de 0,1%."
)

work_caption <- paste(
  "Fuente: INEC, ENEMDU anual 2025.",
  "Elaboración: Valeria Lizeth Marcayata Ojeda; adaptación de El Quantificador.",
  "Nota: NINI se refiere a personas de 15 a 29 años que no asisten a clases",
  "y no tienen empleo. Cada lado suma 100%; las respuestas se agruparon",
  "para facilitar la lectura."
)

# 3. Build one companion chart ----
build_nini_plot <- function(plot_data,
                            reason_order,
                            title,
                            subtitle,
                            caption,
                            out_path) {
  reason_positions <- tibble(
    razon = reason_order,
    y = first_reason_y - seq_along(reason_order) + 1
  ) |>
    mutate(
      razon_etiqueta = case_when(
        razon == "Enfermedad o discapacidad" ~ "Enfermedad o\ndiscapacidad",
        razon == "Sin deseos o necesidad" ~ "Sin deseos o\nnecesidad",
        razon == "Enfermedad o incapacidad" ~ "Enfermedad o\nincapacidad",
        razon == "Quehaceres del hogar" ~ "Quehaceres",
        TRUE ~ razon
      )
    )

  plot_data <- plot_data |>
    inner_join(
      reason_positions |> select(razon, y),
      by = join_by(razon),
      relationship = "many-to-one",
      unmatched = "error"
    ) |>
    arrange(razon, sexo, nivel_educativo) |>
    mutate(
      acumulado = cumsum(porcentaje),
      acumulado_anterior = lag(acumulado, default = 0),
      .by = c(razon, sexo)
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
      .by = c(razon, sexo)
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

  sex_headers <- tibble(
    sexo = factor(
      c("Hombres", "Mujeres"),
      levels = c("Hombres", "Mujeres")
    ),
    x = c(-61, 61),
  y = sex_header_y,
    etiqueta = c("Hombres", "Mujeres")
  )

  y_min <- min(reason_positions$y) - 1.35
  y_max <- sex_header_y + 0.55

  p_base <- ggplot() +
    geom_vline(
      xintercept = c(-center_gap, center_gap),
      colour = "grey82",
      linewidth = 0.3
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
      size = 3.2,
      lineheight = 0.95,
      colour = "grey20"
    ) +
    geom_text(
      data = bar_totals,
      aes(x = x, y = y, label = etiqueta, hjust = hjust),
      size = 3.2,
      fontface = "bold",
      colour = "grey20"
    ) +
    geom_text(
      data = sex_headers,
      aes(x = x, y = y, label = etiqueta, colour = sexo),
      size = 3.2,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = education_palette,
      breaks = names(education_palette),
      labels = education_legend_labels,
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
      limits = c(y_min, y_max),
      breaks = NULL,
      expand = expansion(mult = c(0, 0))
    ) +
    coord_cartesian(xlim = c(-x_limit, x_limit), clip = "off") +
    labs(
      title = wrap_title_house(title),
      subtitle = wrap_subtitle_house(subtitle),
      x = NULL,
      y = NULL,
      caption = wrap_caption_house(caption)
    ) +
    theme_quantificador() +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 8),
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
        nrow = 1,
        byrow = TRUE,
        title.position = "top"
      )
    )

  p_final <- house_apply_logo(p_base, "portrait")

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
}

# 4. Export ----
dir.create(dirname(study_out_path), recursive = TRUE, showWarnings = FALSE)

build_nini_plot(
  plot_data = study_data,
  reason_order = study_order,
  title = "NINIs: ¿por qué dejaron de estudiar?",
  subtitle = "Jóvenes de 15 a 29 años, por sexo y nivel educativo, Ecuador, 2025",
  caption = study_caption,
  out_path = study_out_path
)

build_nini_plot(
  plot_data = work_data,
  reason_order = work_order,
  title = "NINIs: ¿por qué no trabajan?",
  subtitle = "Jóvenes de 15 a 29 años, por sexo y nivel educativo, Ecuador, 2025",
  caption = work_caption,
  out_path = work_out_path
)

message("Guardados: ", study_out_path, " y ", work_out_path)
invisible(sessionInfo())
