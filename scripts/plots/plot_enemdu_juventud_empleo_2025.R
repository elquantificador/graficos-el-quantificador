# ============================================================
# plot_enemdu_juventud_empleo_2025.R
# Author: Cristhian Guamán Saca; adaptación de El Quantificador
# Purpose: Produce los gráficos 1 y 2 de la entrega sobre empleo juvenil.
# Requiere: data/processed/enemdu_juventud_empleo_2025.rds
# Guarda:   outputs/figures/42_a_empleo-adecuado-juvenil_sexo-ecuador.png
#           outputs/figures/42_b_empleo-adecuado-juvenil_provincia-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_enemdu_juventud_empleo_2025.R
# ============================================================

# 0. Setup ----
source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "stringr", "tidyr"))

data_path <- file.path(
  "data", "processed", "enemdu_juventud_empleo_2025.rds"
)
sex_out_path <- file.path(
  "outputs", "figures", "42_a_empleo-adecuado-juvenil_sexo-ecuador.png"
)
province_out_path <- file.path(
  "outputs", "figures", "42_b_empleo-adecuado-juvenil_provincia-ecuador.png"
)
spec <- house_spec("portrait")

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_enemdu_juventud_empleo_2025.R")
}

chart_data <- readRDS(data_path)
sex_data <- chart_data$sexo |>
  mutate(sexo = factor(sexo, levels = c("Hombres", "Mujeres")))
province_data <- chart_data$provincias |>
  mutate(provincia = iconv(as.character(provincia), from = "UTF-8", to = "UTF-8")) |>
  arrange(desc(empleo_adecuado_pct), provincia) |>
  mutate(provincia = factor(provincia, levels = rev(provincia)))

# 1. Shared labels and caption ----
sex_long <- sex_data |>
  transmute(
    sexo,
    `Empleo adecuado` = empleo_adecuado_pct,
    Desempleo = desempleo_pct
  ) |>
  tidyr::pivot_longer(
    cols = c(`Empleo adecuado`, Desempleo),
    names_to = "indicador",
    values_to = "porcentaje"
  ) |>
  mutate(
    indicador = factor(indicador, levels = c("Empleo adecuado", "Desempleo")),
    etiqueta = label_number_intl(accuracy = 0.1, suffix = "%")(porcentaje)
  )

sex_caption <- paste(
  "Fuente: INEC, ENEMDU anual 2025.",
  "Elaboración: Cristhian Guamán Saca; adaptación de El Quantificador.",
  "Nota: Jóvenes = personas de 18 a 29 años. Cada porcentaje se calcula sobre la PEA joven",
  "del grupo correspondiente y usa el factor de expansión fexp."
)

province_caption <- paste(
  "Fuente: INEC, ENEMDU anual 2025.",
  "Elaboración: Cristhian Guamán Saca; adaptación de El Quantificador.",
  "Nota: Jóvenes = personas de 18 a 29 años. Cada porcentaje se calcula sobre la PEA joven",
  "de la provincia correspondiente y usa el factor de expansión fexp."
)

chart_palette <- c(
  "Empleo adecuado" = "#2D7DB3",
  "Desempleo" = "#D97729"
)

# 2. Chart 1: sex comparison ----
sex_plot <- ggplot(
  sex_long,
  aes(x = sexo, y = porcentaje, fill = indicador)
) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.58,
    colour = "white",
    linewidth = 0.2
  ) +
  geom_text(
    aes(
      label = etiqueta,
      group = indicador
    ),
    position = position_dodge(width = 0.72),
    vjust = -0.25,
    size = 3,
    colour = "grey20"
  ) +
  scale_fill_manual(values = chart_palette) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 10),
    labels = label_number_intl(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  coord_cartesian(ylim = c(0, 55), clip = "off") +
  labs(
    title = wrap_title_house(
      "Las mujeres jóvenes enfrentan más desempleo y menos empleo adecuado"
    ),
    subtitle = wrap_subtitle_house(
      "Empleo adecuado y desempleo entre la PEA de 18 a 29 años, por sexo, Ecuador, 2025"
    ),
    x = NULL,
    y = "Porcentaje de la PEA joven",
    fill = NULL,
    caption = wrap_caption_house(sex_caption)
  ) +
  theme_quantificador() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),
    legend.key.width = grid::unit(0.42, "cm"),
    legend.key.height = grid::unit(0.3, "cm"),
    panel.grid.major.y = element_line(
      colour = "grey88",
      linewidth = 0.35,
      linetype = "dashed"
    ),
    panel.grid.minor = element_blank()
  )

# 3. Chart 2: provincial ranking ----
province_data <- province_data |>
  mutate(
    etiqueta = label_number_intl(accuracy = 0.1, suffix = "%")(
      empleo_adecuado_pct
    )
  )

province_plot <- ggplot(
  province_data,
  aes(x = empleo_adecuado_pct, y = provincia)
) +
  geom_col(
    width = 0.68,
    fill = "#2D7DB3",
    colour = "white",
    linewidth = 0.2
  ) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.15,
    size = 3,
    colour = "grey20"
  ) +
  scale_x_continuous(
    breaks = seq(0, 50, by = 10),
    labels = label_number_intl(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(
    name = NULL,
    expand = expansion(add = c(0.35, 0.35))
  ) +
  coord_cartesian(xlim = c(0, 55), clip = "off") +
  labs(
    title = wrap_title_house(
      "Morona Santiago tiene 6 veces menos empleo adecuado juvenil que Pichincha"
    ),
    subtitle = wrap_subtitle_house(
      "Porcentaje de la PEA joven con empleo adecuado, por provincia, Ecuador, 2025"
    ),
    x = "Empleo adecuado (% de la PEA joven)",
    caption = wrap_caption_house(province_caption)
  ) +
  theme_quantificador() +
  theme(
    panel.grid.major.x = element_line(
      colour = "grey88",
      linewidth = 0.35,
      linetype = "dashed"
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# 4. Export ----
dir.create(dirname(sex_out_path), recursive = TRUE, showWarnings = FALSE)

# The standard logo position collides with the multi-line caption on these
# two dense portrait charts, so it is raised while preserving its size.
sex_final <- house_apply_logo(sex_plot, "portrait", y = 0.18)
province_final <- house_apply_logo(province_plot, "portrait", y = 0.18)

ggsave(
  filename = sex_out_path,
  plot = sex_final,
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

ggsave(
  filename = province_out_path,
  plot = province_final,
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardados: ", sex_out_path, " y ", province_out_path)
invisible(sessionInfo())
