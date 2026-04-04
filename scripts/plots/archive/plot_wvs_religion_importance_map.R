# ============================================================
# plot_wvs_religion_importance_map.R
# Archived map version for WVS religion importance.
# ============================================================

get_script_path <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      if (!is.null(frame$ofile)) frame$ofile else NA_character_
    },
    character(1)
  )
  frame_files <- frame_files[!is.na(frame_files)]

  if (length(frame_files) > 0) {
    return(normalizePath(frame_files[length(frame_files)], winslash = "/", mustWork = FALSE))
  }

  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)

  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

script_path <- get_script_path()
project_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = FALSE)

source(file.path(project_root, "scripts", "utils.R"))
source(file.path(project_root, "scripts", "packages.R"))
ensure_packages(c("dplyr", "ggplot2", "scales", "maps", "ragg", "stringr"))

data_path <- file.path(project_root, "data", "processed", "wvs_religion_importance.rds")
out_path <- file.path(project_root, "figures", "wvs_religion_importance_map.png")
draft_out_path <- file.path(project_root, "figures", "wvs_religion_importance_map_draft.png")

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source(file.path(project_root, "scripts", "data-cleaning", "clean_wvs_religion_importance.R"))
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
  select(geography, country_es, share, sample_size)

south_america <- ggplot2::map_data("world") |>
  filter(region %in% names(country_labels_es))

map_df <- south_america |>
  left_join(plot_df, by = c("region" = "geography")) |>
  filter(!is.na(share))

label_df <- map_df |>
  group_by(region, country_es, share, sample_size) |>
  summarise(
    x = mean(range(long, na.rm = TRUE)),
    y = mean(range(lat, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(
    x = case_when(
      region == "Chile" ~ x + 1.0,
      region == "Uruguay" ~ x + 1.2,
      TRUE ~ x
    ),
    y = case_when(
      region == "Chile" ~ y + 1.4,
      region == "Uruguay" ~ y - 0.5,
      TRUE ~ y
    ),
    label = case_when(
      region == "Chile" ~ paste0(country_es, " ", percent(share, accuracy = 0.1)),
      TRUE ~ paste0(country_es, "\n", percent(share, accuracy = 0.1))
    )
  )

caption_txt <- stringr::str_wrap(
  paste0(
    "En Ecuador, 53,6% dice que la religión es muy importante en su vida; ",
    "solo Colombia y Bolivia registran porcentajes más altos en esta muestra sudamericana. ",
    "Fuente: Encuesta Mundial de Valores (Ronda 7, 2017-2022). ",
    "Campo por país: Argentina y Bolivia (2017); Brasil, Chile, Colombia, Ecuador y Perú (2018); ",
    "Venezuela (2021); Uruguay (2022)."
  ),
  width = 74
)

fill_colors <- c("#fff2d9", "#f8cf8b", "#ef9f4e", "#d97729", "#a84d16")
fill_values <- scales::rescale(c(0.18, 0.30, 0.40, 0.50, 0.60))

p_base <- ggplot(map_df) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = share),
    color = "white",
    linewidth = 0.5
  ) +
  geom_polygon(
    data = south_america,
    aes(x = long, y = lat, group = group),
    fill = NA,
    color = "grey80",
    linewidth = 0.25
  ) +
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = label),
    size = 2.1,
    lineheight = 0.95,
    color = "grey15"
  ) +
  scale_fill_gradientn(
    colours = fill_colors,
    values = fill_values,
    limits = c(0.18, 0.60),
    labels = percent_format(accuracy = 1),
    breaks = c(0.2, 0.3, 0.4, 0.5, 0.6),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0,
      barwidth = grid::unit(58, "mm"),
      barheight = grid::unit(3.5, "mm")
    )
  ) +
  coord_fixed(ratio = 1.35, xlim = c(-84, -34), ylim = c(-58, 15), expand = FALSE) +
  labs(
    title = "Colombia, Bolivia y Ecuador\nson los países más religiosos\nde Sudamérica",
    subtitle = paste(
      "Porcentaje que respondió “Muy importante” en la",
      "Encuesta Mundial de Valores (Ronda 7, 2017-2022)",
      sep = "\n"
    ),
    fill = "Porcentaje",
    caption = caption_txt
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 6.5, face = "bold", colour = "grey20"),
    legend.text = element_text(size = 6.2, colour = "grey20"),
    legend.justification = "center",
    legend.box.margin = margin(t = -2, b = 1),
    plot.title = element_text(colour = "grey20", size = 10.8, face = "bold", hjust = 0, lineheight = 1.02),
    plot.subtitle = element_text(colour = "grey30", size = 7.3, lineheight = 1.03, hjust = 0),
    plot.caption = element_text(
      colour = "grey30",
      size = 4.1,
      lineheight = 1.1,
      hjust = 0,
      margin = margin(t = 5)
    ),
    plot.margin = margin(8, 4, 8, 4),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = NA)
  )

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
p_final <- add_logo(
  p_base,
  logo_path = file.path(project_root, "quantificador.png"),
  x = 0.87,
  y = 0.14,
  width = 0.08,
  height = 0.08
)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white",
  device = ragg::agg_png
)

ggsave(
  filename = draft_out_path,
  plot = p_base,
  width = 4,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white",
  device = ragg::agg_png
)

message("Guardado: ", out_path)
message("Guardado borrador: ", draft_out_path)
