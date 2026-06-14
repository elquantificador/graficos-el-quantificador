# ============================================================
# plot_endi_cuidador_principal.R
# Genera el gráfico del principal cuidador entre semana para
# niños y niñas en la ENDI R2.
# Requiere: data/processed/endi_r2_cuidador_principal.rds
# Guarda:   outputs/figures/15_cuidador-principal_infancia-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_endi_cuidador_principal.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "scales", "ragg", "stringr"))

out_path <- "outputs/figures/15_cuidador-principal_infancia-ecuador.png"
plot_df <- readRDS("data/processed/endi_r2_cuidador_principal.rds") %>%
  mutate(
    cuidador_label = case_when(
      cuidador_principal == "Padre o abuelos" ~ "Padre o abuelos",
      cuidador_principal == "Centro de Desarrollo Infantil (CDI)" ~ "Centro de Desarrollo\nInfantil (CDI)",
      TRUE ~ as.character(cuidador_principal)
    ),
    cuidador_label = factor(
      cuidador_label,
      levels = c(
        "Madre",
        "Padre o abuelos",
        "Centro de Desarrollo\nInfantil (CDI)",
        "Otros"
      )
    )
  )

title_raw <- "La infancia ecuatoriana crece bajo el cuidado, cariño y dedicación de sus madres"
subtitle_raw <- "Proporción de niños y niñas menores de 5 años, por cuidador principal, ENDI Ronda 2 2023-2024"
caption_raw <- paste(
  "Fuente: INEC, Encuesta Nacional sobre Desnutrición Infantil (ENDI), Ronda 2 2023-2024.",
  "Elaborado por: Daniel Sánchez para El Quantificador del Laboratorio LIDE.",
  "Nota: El principal cuidador del niño de hasta 5 años es quien permanece con el niño la mayor parte del tiempo de lunes a viernes.",
  "La categoría Otros agrupa tíos/tías, otros miembros del hogar, otros familiares, vecinos, amigos, empleada o niñera y la opción 'se queda solo'.",
  "Las proporciones son ponderadas por el factor de expansión de la encuesta."
)

portrait_path <- out_path

build_chart <- function(orientation) {
  spec <- house_spec(orientation)
  # Portrait conserva los saltos manuales de título/subtítulo y el caption a 92;
  # landscape reacomoda al ancho apaisado.
  if (orientation == "landscape") {
    title_txt    <- stringr::str_wrap(title_raw, width = spec$title_wrap)
    subtitle_txt <- stringr::str_wrap(subtitle_raw, width = spec$subtitle_wrap)
    # Caption a 5,8 pt: se envuelve para llenar el lienzo a ese tamaño.
    caption_txt  <- stringr::str_wrap(caption_raw, width = landscape_wrap_for_size(5.8))
  } else {
    title_txt    <- "La infancia ecuatoriana crece bajo el\ncuidado, cariño y dedicación de sus madres"
    subtitle_txt <- "Proporción de niños y niñas menores de 5 años, por cuidador\nprincipal, ENDI Ronda 2 2023-2024"
    caption_txt  <- stringr::str_wrap(caption_raw, width = 92)
  }

  ggplot(
    plot_df,
    aes(x = cuidador_label, y = share)
  ) +
    geom_col(fill = "#EF9F4E", width = 0.56) +
    geom_text(
      aes(label = percent_intl(share, accuracy = 0.1)),
      hjust = -0.10,
      size = 2.6
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.85),
      breaks = seq(0, 0.8, by = 0.2),
      expand = expansion(mult = c(0, 0.08))
    ) +
    scale_x_discrete(limits = rev(levels(plot_df$cuidador_label))) +
    coord_flip(clip = "off") +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL,
      y = NULL,
      caption = caption_txt
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(colour = "black", size = 8),
      axis.text.x = element_text(colour = "black", size = 7),
      plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
      plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption = element_text(colour = "black", size = 5.8, lineheight = 1.1, hjust = 0, margin = margin(t = 5)),
      axis.line.y = element_line(colour = "black"),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(colour = "black"),
      plot.margin = if (orientation == "landscape") margin(6, 18, 6, 18) else margin(6, 32, 6, 18),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid = element_blank()
    )
}

dir.create("outputs/figures", showWarnings = FALSE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  # Sin logo en landscape; portrait mantiene el logo en su posición de la casa.
  p_final <- house_apply_logo(build_chart(orientation), orientation,
                              x = 0.895, y = 0.13, width = 0.09, height = 0.09)
  dest <- house_out_path(portrait_path, orientation)
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

