# ============================================================
# plot_wvs_religion_importance.R
# Genera un gráfico de barras horizontales con el porcentaje
# de personas que consideran que la religión es muy importante.
# Requiere: data/processed/wvs_religion_importance.rds
# Guarda:   outputs/figures/09_religion_importancia_sudamerica.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "forcats", "ragg", "stringr"))

data_path <- "data/processed/wvs_religion_importance.rds"
out_path <- "outputs/figures/09_religion_importancia_sudamerica.png"

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_wvs_religion_importance.R")
}

if (!file.exists(data_path)) {
  stop("No se pudo crear el archivo requerido: ", data_path, call. = FALSE)
}

country_labels_es <- c(
  "Argentina" = "Argentina",
  "Bolivia" = "Bolivia",
  "Brazil" = "Brasil",
  "Chile" = "Chile",
  "Colombia" = "Colombia",
  "Ecuador" = "Ecuador",
  "Peru" = "Perú",
  "Uruguay" = "Uruguay",
  "Venezuela" = "Venezuela"
)

plot_df <- readRDS(data_path) |>
  mutate(country_es = recode(as.character(geography), !!!country_labels_es)) |>
  filter(response == "Very important", geography != "Total") |>
  mutate(
    tier = case_when(
      country_es %in% c("Colombia", "Bolivia", "Ecuador") ~ "destacado",
      TRUE ~ "resto"
    )
  ) |>
  arrange(share) |>
  mutate(country_es = forcats::fct_inorder(country_es))

# saltos manuales históricos para no alterar el PNG publicado.
title_raw <- "Colombia, Bolivia y Ecuador son los países más religiosos de Sudamérica"
subtitle_raw <- paste(
  "Porcentaje de personas para cada país que respondió que la religión es “Muy importante”",
  "en su vida. Encuesta Mundial de Valores (Ronda 7, 2017-2022)."
)
caption_raw <- paste(
  "Fuente: Encuesta Mundial de Valores (Ronda 7, 2017-2022). Año de levantamiento: Argentina y Bolivia (2017); Brasil, Chile, Colombia, Ecuador y Perú (2018).",
  "Venezuela (2021); Uruguay (2022). Elaboración: El Quantificador por Laboratorio LIDE."
)


build_chart <- function() {
  spec <- house_spec("portrait")

    title_txt <- paste(
      "Colombia, Bolivia y Ecuador son los países más",
      "religiosos de Sudamérica",
      sep = "\n"
    )
    subtitle_txt <- paste(
      "Porcentaje de personas para cada país que respondió que la religión es “Muy importante”",
      "en su vida. Encuesta Mundial de Valores (Ronda 7, 2017-2022).",
      sep = "\n"
    )
    caption_txt <- paste(
      "Fuente: Encuesta Mundial de Valores (Ronda 7, 2017-2022). Año de levantamiento: Argentina y Bolivia (2017); Brasil, Chile, Colombia, Ecuador y Perú (2018).",
      "Venezuela (2021); Uruguay (2022). Elaboración: El Quantificador por Laboratorio LIDE.",
      sep = "\n"
    )

  ggplot(plot_df, aes(x = country_es, y = share, fill = tier)) +
    geom_col(width = 0.68) +
    geom_text(
      aes(label = percent_intl(share, accuracy = 0.1)),
      hjust = -0.10,
      size = 2.7,
      colour = "grey20"
    ) +
    scale_fill_manual(
      values = c("destacado" = "#d97729", "resto" = "#efc27d"),
      guide = "none"
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.65),
      breaks = seq(0, 0.6, by = 0.1),
      expand = expansion(mult = c(0, 0.10))
    ) +
    coord_flip(clip = "off") +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL,
      y = NULL,
      caption = caption_txt
    ) +
    theme_quantificador() +
    theme(
      axis.text.y = element_text(size = 8, colour = "grey20"),
      axis.text.x = element_text(size = 7, colour = "grey20"),
      axis.title.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(size = 10.7, face = "bold", colour = "grey20", hjust = 0, lineheight = 1.02),
      plot.subtitle = element_text(size = 6.8, colour = "grey30", lineheight = 1.04, hjust = 0),
      plot.caption = element_text(size = 3.9, colour = "grey30", lineheight = 1.1, hjust = 0),
      plot.margin = margin(8, 28, 8, 12)
    )
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

  spec <- house_spec("portrait")
  p_final <- house_apply_logo(
    build_chart(),
    "portrait",
    logo_path = "quantificador.png",
    x = 0.892,
    y = 0.122,
    width = 0.085,
    height = 0.085
  )
  dest <- out_path
  ggsave(
    filename = dest,
    plot = p_final,
    width = spec$width,
    height = spec$height,
    units = "in",
    dpi = spec$dpi,
    bg = "white",
    device = ragg::agg_png
  )
  message("Guardado: ", dest)

