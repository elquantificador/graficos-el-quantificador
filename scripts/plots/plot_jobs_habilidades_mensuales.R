# ============================================================
# plot_jobs_habilidades_mensuales.R
# Grafica las herramientas técnicas más demandadas en ofertas de datos.
# Requiere: data/processed/jobs_habilidades_mensuales.rds
# Guarda:   outputs/figures/32_habilidades-demandadas-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_jobs_habilidades_mensuales.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

in_path <- "data/processed/jobs_habilidades_mensuales.rds"
out_path <- "outputs/figures/32_habilidades-demandadas-ecuador.png"
spec <- house_spec("portrait")

result <- readRDS(in_path)
plot_data <- result$skills |>
  arrange(rank) |>
  mutate(skill = factor(skill, levels = rev(skill)))

title_txt <- wrap_title_house(
  "Excel, Power BI y SQL dominan la demanda de habilidades técnicas en Ecuador",
  width = 40
)
subtitle_txt <- wrap_subtitle_house(
  "Porcentaje de ofertas que menciona cada herramienta técnica; cinco principales, Ecuador, junio-julio de 2026"
)
caption_txt <- wrap_caption_house(
  "Fuente: Multitrabajos, Computrabajo y Mipleo, mediante web scraping de 745 ofertas laborales en análisis de datos o áreas adyacentes, entre junio y julio de 2026. Se consideran solamente herramientas de software técnico. Los porcentajes no suman 100%, ya que cada oferta puede listar más de una herramienta. Elaboración: Daniel Sánchez para El Quantificador de Laboratorio LIDE."
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
