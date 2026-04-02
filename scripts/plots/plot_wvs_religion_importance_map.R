# ============================================================
# plot_wvs_religion_importance_map.R
# Genera un mapa coroplético de América del Sur con el porcentaje
# de personas que consideran que Dios es muy importante en su vida.
# Requiere: data/processed/wvs_religion_importance.rds
# Guarda:   figures/wvs_religion_importance_map.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_wvs_religion_importance_map.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "sf", "rnaturalearth", "ragg", "stringr"))

out_path <- "figures/wvs_religion_importance_map.png"

response_labels_es <- c(
  "Very important" = "Muy importante",
  "Rather important" = "Algo importante",
  "Not very important" = "Poco importante",
  "Not at all important" = "Nada importante",
  "Don't know" = "No sabe",
  "No answer" = "No responde"
)

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

plot_df <- readRDS("data/processed/wvs_religion_importance.rds") |>
  mutate(
    country_es = recode(as.character(geography), !!!country_labels_es)
  ) |>
  filter(response == "Very important", geography != "Total") |>
  select(geography, country_es, share, sample_size)

south_america <- rnaturalearth::ne_countries(
  scale = "medium",
  continent = "South America",
  returnclass = "sf"
) |>
  select(admin, name_es, geometry)

map_df <- south_america |>
  left_join(plot_df, by = c("admin" = "geography")) |>
  filter(!is.na(share))

label_points <- suppressWarnings(
  map_df |>
    sf::st_transform(3857) |>
    sf::st_point_on_surface() |>
    sf::st_transform(4326)
)

label_coords <- sf::st_coordinates(label_points)

label_df <- map_df |>
  st_drop_geometry() |>
  mutate(
    x = label_coords[, 1],
    y = label_coords[, 2],
    x = case_when(
      admin == "Chile" ~ x + 1.2,
      admin == "Uruguay" ~ x + 1.5,
      TRUE ~ x
    ),
    y = case_when(
      admin == "Chile" ~ y + 1.8,
      admin == "Uruguay" ~ y - 1,
      TRUE ~ y
    ),
    label = case_when(
      admin == "Chile" ~ paste0(country_es, " ", percent(share, accuracy = 0.1)),
      TRUE ~ paste0(country_es, "\n", percent(share, accuracy = 0.1))
    )
  )

caption_txt <- stringr::str_wrap(paste0(
  "En Ecuador, 53,6% dice que la religión es muy importante en su vida; ",
  "solo Colombia y Bolivia registran porcentajes más altos en esta muestra sudamericana. ",
  "Fuente: Encuesta Mundial de Valores (Ronda 7, 2017-2022), tabulación sobre cuán importante es la religión en la vida. ",
  "Campo por país: Argentina y Bolivia (2017); Brasil, Chile, Colombia, Ecuador y Perú (2018); ",
  "Venezuela (2021); Uruguay (2022). Elaboración: El Quantificador de Laboratorio LIDE."
), width = 88)

fill_colors <- c("#fff2d9", "#f8cf8b", "#ef9f4e", "#d97729", "#a84d16")
fill_values <- scales::rescale(c(0.18, 0.30, 0.40, 0.50, 0.60))

p_base <- ggplot(map_df) +
  geom_sf(aes(fill = share), color = "white", linewidth = 0.45) +
  geom_sf(data = south_america, fill = NA, color = "grey80", linewidth = 0.25) +
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = label),
    size = 2.0,
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
      barwidth = unit(52, "mm"),
      barheight = unit(3.5, "mm")
    )
  ) +
  coord_sf(xlim = c(-83, -33), ylim = c(-57, 14), expand = FALSE) +
  labs(
    title = "En casi toda Sudamérica,\nla mayoría cree que la religión es\nmuy importante en su vida",
    subtitle = "Porcentaje que respondió\n'Muy importante' a la importancia de la religión\nen la Encuesta Mundial de Valores (Ronda 7, 2017-2022)",
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
    legend.box.margin = margin(t = 2, b = 2),
    plot.title = element_text(colour = "grey20", size = 11.5, face = "bold", hjust = 0, lineheight = 1.02),
    plot.subtitle = element_text(colour = "grey30", size = 7.2, lineheight = 1.02, hjust = 0),
    plot.caption = element_text(
      colour = "grey30",
      size = 4.2,
      lineheight = 1.1,
      hjust = 0,
      margin = margin(t = 6)
    ),
    plot.margin = margin(12, 20, 12, 20),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = NA)
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.70, y = 0.14, width = 0.075, height = 0.075)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4.5,
  height = 5.625,
  units = "in",
  dpi = 300,
  device = ragg::agg_png
)

message("Guardado: ", out_path)
