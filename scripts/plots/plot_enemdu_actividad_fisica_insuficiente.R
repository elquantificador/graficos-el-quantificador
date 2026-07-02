# ============================================================
# plot_enemdu_actividad_fisica_insuficiente.R
# Genera el gráfico de actividad física insuficiente por área
# de residencia y grupo etario, ENEMDU 2024.
# Requiere: data/processed/enemdu_actividad_fisica_insuficiente.rds
# Guarda:   outputs/figures/29_actividad-fisica-insuficiente_area-edad-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_actividad_fisica_insuficiente.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg", "stringr"))

out_path <- "outputs/figures/29_actividad-fisica-insuficiente_area-edad-ecuador.png"

plot_df <- readRDS("data/processed/enemdu_actividad_fisica_insuficiente.rds") |>
  filter(anio == 2024) |>
  mutate(
    grupo_edad = factor(
      grupo_edad,
      levels = c(
        "8-17 años",
        "18-69 años"
      )
    ),
    area_residencia = factor(
      area_residencia,
      levels = c("Entorno urbano", "Entorno rural")
    )
  )

title_raw <- "La actividad física insuficiente es mucho más alta entre niños y adolescentes"
subtitle_raw <- "Prevalencia por grupo etario y área de residencia, ENEMDU módulo de actividad física, diciembre 2024"
caption_raw <- paste0(
  "Fuente: INEC, Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU), módulos de actividad física ",
  "de diciembre 2024. Cálculos de Joan Mogro Ponce para El Quantificador. ",
  "Nota: Para niñas, niños y adolescentes, se considera actividad física insuficiente cuando reportan menos de 7 días ",
  "de actividad física en la última semana. Para personas de 18 a 69 años, se usa el umbral semanal de 150 minutos ",
  "moderados equivalentes, sumando caminata y actividad moderada, y ponderando la actividad vigorosa por dos."
)

palette <- c(
  "Entorno urbano" = "#4D79E6",
  "Entorno rural" = "#A282E8"
)

build_chart <- function() {
  ggplot(plot_df, aes(x = grupo_edad, y = prevalencia, fill = area_residencia)) +
    geom_col(
      position = position_dodge(width = 0.72),
      width = 0.62
    ) +
    scale_fill_manual(values = palette) +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 1),
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = wrap_title_house(title_raw),
      subtitle = wrap_subtitle_house(subtitle_raw),
      x = NULL,
      y = "Prevalencia de actividad física insuficiente",
      fill = NULL,
      caption = wrap_caption_house(caption_raw)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_quantificador() +
    theme(
      axis.text = element_text(colour = "black", size = 8),
      axis.text.x = element_text(size = 7, lineheight = 0.9),
      axis.title.y = element_text(size = 7, margin = margin(r = 6), colour = "black"),
      plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
      plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption = element_text(colour = "grey30", size = 5.8, lineheight = 1.1, hjust = 0, margin = margin(t = 8)),
      legend.position = "bottom",
      legend.justification = "center",
      legend.text = element_text(size = 7),
      legend.key.width = unit(10, "pt"),
      legend.spacing.x = unit(4, "pt"),
      legend.box = "horizontal",
      plot.margin = margin(6, 18, 6, 16)
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")
p_final <- house_apply_logo(build_chart(), "portrait", x = 0.88, y = 0.13)

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
