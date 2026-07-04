# ============================================================
# plot_enemdu_actividad_fisica_insuficiente.R
# Genera el gráfico de actividad física insuficiente por área
# de residencia y grupos de edad, ENEMDU 2024.
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
  mutate(
    grupo_edad = factor(
      grupo_edad,
      levels = c(
        "18-29 años",
        "30-44 años",
        "45-69 años"
      )
    ),
    area_residencia = factor(
      area_residencia,
      levels = c("Entorno urbano", "Entorno rural")
    )
  )

title_raw <- "Menos movimiento, más sedentarismo: la brecha\nen la ciudad y el campo ecuatoriano"
subtitle_raw <- "Prevalencia de actividad física insuficiente, por zona de residencia\n y tres grupos de edad, diciembre 2024"
caption_raw <- paste(
  "Fuente: INEC, Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU), módulo de actividad",
  "física de diciembre de 2024. Elaboración de Joan Mogro Ponce para El Quantificador de Laboratorio",
  "LIDE. Nota: Se muestra únicamente población de 18 a 69 años. Actividad insuficiente = menos de",
  "150 minutos moderados equivalentes semanales. La actividad vigorosa pondera por dos, según la OMS",
  "en adultos.",
  sep = "\n"
)

palette <- c(
  "18-29 años" = "#2F6DB3",
  "30-44 años" = "#F0A145",
  "45-69 años" = "#7B8D97"
)

build_chart <- function() {
  ggplot(
    plot_df,
    aes(
      x = area_residencia,
      y = prevalencia,
      fill = grupo_edad
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.82),
      width = 0.58
    ) +
    geom_text(
      aes(label = paste0(
        number(prevalencia, accuracy = 0.1, decimal.mark = ","),
        "%"
      )),
      position = position_dodge(width = 0.82),
      vjust = -0.35,
      size = 2.5,
      colour = "black"
    ) +
    scale_fill_manual(values = palette) +
    scale_y_continuous(
      labels = label_percent(scale = 1, accuracy = 1),
      limits = c(0, 30),
      breaks = seq(0, 30, 10),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_cartesian(ylim = c(0, 32), clip = "off") +
    labs(
      title = title_raw,
      subtitle = subtitle_raw,
      x = NULL,
      y = "Prevalencia (%)",
      fill = NULL,
      caption = caption_raw
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_quantificador() +
    theme(
      axis.text = element_text(colour = "black", size = 8),
      axis.text.x = element_text(size = 7, lineheight = 0.9),
      axis.title.y = element_text(size = 7.5, margin = margin(r = 8), colour = "black"),
      plot.title = element_text(colour = "black", size = 11.5, face = "bold", hjust = 0),
      plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption = element_text(colour = "grey30", size = 5.8, lineheight = 1.12, hjust = 0, margin = margin(t = 8)),
      legend.position = "bottom",
      legend.justification = "center",
      legend.text = element_text(size = 6.4),
      legend.key.width = unit(8, "pt"),
      legend.spacing.x = unit(2, "pt"),
      legend.box = "horizontal",
      plot.margin = margin(6, 14, 6, 14)
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")
p_final <- house_apply_logo(build_chart(), "portrait", x = 0.88, y = 0.18)

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
