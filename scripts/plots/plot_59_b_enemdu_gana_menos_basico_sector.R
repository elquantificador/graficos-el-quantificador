# ============================================================
# plot_59_b_enemdu_gana_menos_basico_sector.R
# Personas que trabajan y ganan menos del salario básico ($482), por sector,
# con y sin empleo formal, enero a mayo de 2026.
# Requiere: data/processed/enemdu_gana_menos_basico_sector.rds
# Guarda:   outputs/figures/59_b_gana-menos-basico-sector-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_gana_menos_basico_sector.R
#   Rscript scripts/plots/plot_59_b_enemdu_gana_menos_basico_sector.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "ggplot2", "stringr", "ragg"))

out_path <- "outputs/figures/59_b_gana-menos-basico-sector-ecuador.png"
azul <- "#2D7DB3"
gris <- "grey45"

gana_menos <- readRDS("data/processed/enemdu_gana_menos_basico_sector.rds") |>
  dplyr::mutate(sector = factor(sector, levels = rev(sector)))

barras <- gana_menos |>
  tidyr::pivot_longer(
    c(sin_empleo_formal, con_empleo_formal),
    names_to = "empleo",
    values_to = "porcentaje"
  ) |>
  dplyr::mutate(
    empleo = factor(
      empleo,
      levels = c("con_empleo_formal", "sin_empleo_formal"),
      labels = c("Con empleo formal", "Sin empleo formal")
    )
  )

p_base <- ggplot(barras, aes(x = porcentaje, y = sector, fill = empleo)) +
  geom_col(width = 0.7) +
  geom_text(
    data = gana_menos,
    aes(x = gana_menos_de_482 + 1.5, y = sector, label = paste0(round(gana_menos_de_482), "%")),
    inherit.aes = FALSE, hjust = 0, size = 3, fontface = "bold"
  ) +
  scale_fill_manual(values = c("Con empleo formal" = gris, "Sin empleo formal" = azul)) +
  scale_x_continuous(
    breaks = c(0, 25, 50, 75), labels = \(x) paste0(x, "%"), limits = c(0, 95)
  ) +
  scale_y_discrete(labels = \(x) stringr::str_wrap(x, 22)) +
  labs(
    title = wrap_title_house(
      "Más de 8 de cada 10 personas que trabajan en agricultura y pesca ganan menos del básico"
    ),
    subtitle = wrap_subtitle_house(
      "Personas que trabajan y ganan menos de $482, por sector, enero a mayo de 2026"
    ),
    caption = wrap_caption_house(
      paste(
        "Fuente: INEC, ENEMDU de enero a mayo de 2026.",
        "Elaboración: Daniel Sánchez para El Quantificador de Laboratorio LIDE.",
        "Nota: porcentaje sobre todas las personas que trabajan en cada sector.",
        "Sin empleo formal incluye el sector informal, el empleo doméstico y",
        "los no clasificados."
      )
    ),
    x = NULL, y = NULL
  ) +
  theme_quantificador() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 7, colour = "grey20"),
    legend.key.size = grid::unit(0.3, "cm")
  )

p_final <- add_logo(p_base, y = 0.17)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(
  out_path, plot = p_final, width = 4, height = 5, dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)
