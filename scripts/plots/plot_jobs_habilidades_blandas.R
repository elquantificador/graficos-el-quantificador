# ============================================================
# plot_jobs_habilidades_blandas.R
# Grafica las habilidades blandas más mencionadas en ofertas de datos.
# Requiere: data/processed/jobs_habilidades_blandas.rds
# Guarda:   outputs/figures/35_habilidades-blandas-demandadas-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_jobs_habilidades_blandas.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/jobs_habilidades_blandas.rds"
out_path <- "outputs/figures/35_habilidades-blandas-demandadas-ecuador.png"
spec <- house_spec("portrait")

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_jobs_habilidades_blandas.R")
}

result <- readRDS(data_path)
plot_data <- result$top_skills |>
  arrange(rank) |>
  mutate(skill = factor(skill, levels = rev(skill)))

title_txt <- wrap_title_house(
  "Las habilidades en comunicación y organización son las competencias más solicitadas por empleadores ecuatorianos"
)
subtitle_txt <- wrap_subtitle_house(
  "Porcentaje de ofertas que menciona cada competencia; cinco principales, Ecuador, junio-julio de 2026"
)
caption_txt <- wrap_caption_house(
  paste(
    "Fuente: Multitrabajos, Computrabajo y Mipleo, mediante web scraping de 745 ofertas laborales.",
    "La muestra analítica reúne 434 ofertas de datos y áreas adyacentes con descripción disponible,",
    "entre junio y julio de 2026. Se identifican menciones explícitas mediante un diccionario de texto;",
    "los porcentajes no suman 100%, ya que cada oferta puede mencionar varias competencias.",
    "Elaboración: Daniel Sánchez para El Quantificador de Laboratorio LIDE."
  )
)

p_base <- ggplot(plot_data, aes(x = share, y = skill)) +
  geom_segment(
    aes(x = 0, xend = share, yend = skill),
    linewidth = 1.1,
    colour = "grey78",
    lineend = "round"
  ) +
  geom_point(size = 3.4, colour = "#2F4B7C") +
  geom_text(
    aes(label = percent_intl(share, accuracy = 1)),
    nudge_x = 0.03,
    hjust = 0,
    size = 3,
    fontface = "bold",
    colour = "grey20"
  ) +
  scale_x_continuous(
    labels = label_percent_intl(accuracy = 1),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = "Porcentaje de ofertas",
    y = NULL,
    caption = caption_txt
  ) +
  theme_quantificador() +
  theme(
    plot.title = element_text(lineheight = 1.05),
    axis.line.y = element_line(colour = "grey60", linewidth = 0.5),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 8.5),
    plot.subtitle = element_text(margin = margin(b = 10)),
    plot.margin = margin(8, 34, 8, 16)
  )

p_final <- house_apply_logo(p_base, "portrait", y = 0.14)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
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
message("Guardado: ", out_path)
