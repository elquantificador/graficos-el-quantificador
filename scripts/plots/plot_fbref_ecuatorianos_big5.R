# ============================================================
# plot_fbref_ecuatorianos_big5.R
# Genera el gráfico de minutos jugados por ecuatorianos en las
# cinco grandes ligas de Europa por temporada.
# Requiere: data/processed/fbref_ecuatorianos_big5.rds
# Guarda:   outputs/figures/23_ecuatorianos-big5_minutos-europa.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_fbref_ecuatorianos_big5.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "stringr", "scales", "ragg"))

out_path <- "outputs/figures/23_ecuatorianos-big5_minutos-europa.png"
df <- readRDS("data/processed/fbref_ecuatorianos_big5.rds")

normalize_key <- function(x) iconv(x, from = "", to = "ASCII//TRANSLIT")

df <- df |>
  dplyr::filter(temporada %in% c("21/22", "22/23", "23/24", "24/25", "25/26")) |>
  dplyr::mutate(
    jugador_key = normalize_key(jugador),
    temporada = factor(
      as.character(temporada),
      levels = c("21/22", "22/23", "23/24", "24/25", "25/26")
    )
  )

player_order <- df |>
  dplyr::group_by(jugador, jugador_key) |>
  dplyr::summarise(total_minutos = sum(minutos), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(total_minutos), jugador) |>
  dplyr::pull(jugador_key)

legend_breaks <- df |>
  dplyr::group_by(jugador_key) |>
  dplyr::summarise(total_minutos = sum(minutos), .groups = "drop") |>
  dplyr::mutate(share_total = total_minutos / sum(total_minutos)) |>
  dplyr::filter(share_total >= 0.05) |>
  dplyr::arrange(dplyr::desc(total_minutos)) |>
  dplyr::pull(jugador_key)

player_colors <- c(
  "Moises Caicedo" = "#D04A3E",
  "Pervis Estupinan" = "#00A8CB",
  "Piero Hincapie" = "#6A4C93",
  "Willian Pacho" = "#7B8D97",
  "Gonzalo Plata" = "#BFD9DE",
  "Jackson Porozo" = "#F28E2B",
  "John Yeboah" = "#2F6F7E",
  "Kendry Paez" = "#EDC948",
  "Nilson Angulo" = "#86B6D8",
  "Jeremy Arevalo" = "#C7CDD4"
)

player_labels <- c(
  "Moises Caicedo" = "Caicedo",
  "Piero Hincapie" = "Hincapié",
  "Pervis Estupinan" = "Estupiñán",
  "Willian Pacho" = "Pacho",
  "Gonzalo Plata" = "Plata",
  "Jackson Porozo" = "Porozo",
  "John Yeboah" = "Yeboah",
  "Kendry Paez" = "Páez",
  "Nilson Angulo" = "Angulo",
  "Jeremy Arevalo" = "Arévalo"
)

df_plot <- df |>
  dplyr::mutate(
    jugador_key = factor(jugador_key, levels = player_order)
  )

title_raw <- "Como Ecuador conquistó las cinco grandes ligas de fútbol de Europa"
subtitle_raw <- "Minutos jugados en las grandes cinco ligas de fútbol de Europa, por jugador ecuatoriano, 2021/22 a 2025/26"
caption_raw <- paste(
  "Fuente: FBref.",
  "Elaboración: Eddie Tomalá para El Quantificador de Laboratorio LIDE.",
  "Nota: el gráfico muestra únicamente a futbolistas ecuatorianos convocados a la selección nacional",
  "que registraron minutos en alguna de las cinco grandes ligas europeas",
  "durante el período analizado: Premier League (Inglaterra), La Liga (España),",
  "Serie A (Italia), Bundesliga (Alemania) y Ligue 1 (Francia).",
  "Otros futbolistas con participaciones menores: Jackson Porozo, John Yeboah,",
  "Kendry Páez, Nilson Angulo y Jeremy Arévalo."
)

build_chart <- function(orientation) {
  spec <- house_spec(orientation)

  ggplot(df_plot, aes(x = temporada, y = minutos, fill = jugador_key)) +
    geom_col(width = 0.68, color = "white", linewidth = 0.2) +
    scale_fill_manual(
      values = player_colors,
      labels = player_labels[legend_breaks],
      breaks = legend_breaks
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_y_continuous(
      labels = label_number_intl(),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title = wrap_title_house(title_raw, width = spec$title_wrap),
      subtitle = wrap_subtitle_house(subtitle_raw, width = spec$subtitle_wrap),
      x = "Temporada",
      y = "Minutos jugados",
      caption = wrap_caption_house(caption_raw, width = spec$caption_wrap),
      fill = NULL
    ) +
    theme_quantificador(orientation) +
    theme(
      legend.position = if (orientation == "landscape") c(0.48, 0.997) else c(0.48, 0.983),
      legend.justification = c(0.5, 1),
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 5.4, colour = "grey20"),
      legend.key.height = unit(2.5, "mm"),
      legend.key.width = unit(2.5, "mm"),
      legend.key = element_blank(),
      legend.margin = margin(0, 0, 2, 0),
      legend.spacing.x = unit(0.8, "mm"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.box.margin = margin(0, 0, 0, 0),
      axis.text.x = element_text(size = 7.2, colour = "grey20"),
      axis.text.y = element_text(size = 7.2, colour = "grey20"),
      axis.title.x = element_text(size = 7, colour = "grey30", hjust = 0.5),
      axis.title.y = element_text(size = 7, colour = "grey30", hjust = 0.5),
      plot.margin = if (orientation == "landscape") margin(14, 28, 8, 16) else margin(14, 36, 8, 16)
    )
}

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  p_final <- house_apply_logo(
    build_chart(orientation),
    orientation,
    x = 0.88, y = 0.14, width = 0.09, height = 0.09
  )
  dest <- house_out_path(out_path, orientation)
  ggsave(
    filename = dest,
    plot = p_final,
    width = spec$width,
    height = spec$height,
    units = "in",
    dpi = spec$dpi,
    device = ragg::agg_png,
    bg = "white"
  )
  message("Guardado: ", dest)
}
