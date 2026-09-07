# ============================================================
# plot_reem_antiguedad_empresas_activas.R
# Grafica la distribución de antigüedad de empresas activas REEM 2025.
# Requiere: data/processed/reem_antiguedad_empresas_activas.rds
# Guarda:   outputs/figures/44_antiguedad-empresas-activas-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_reem_antiguedad_empresas_activas.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales"))

data_path <- "data/processed/reem_antiguedad_empresas_activas.rds"
out_path <- "outputs/figures/44_antiguedad-empresas-activas-ecuador.png"
spec <- house_spec("portrait")

if (!file.exists(data_path)) {
  message("No existe ", data_path, ". Ejecutando limpieza previa...")
  source("scripts/data-cleaning/clean_reem_antiguedad_empresas_activas.R")
}

chart_data <- readRDS(data_path)
df <- chart_data$antiguedad
mediana <- chart_data$mediana_anios

caption_txt <- paste(
  "Fuente: INEC, Registro Estadístico de Empresas (REEM) 2025 provisional.",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador de Laboratorio LIDE.",
  "Nota: antigüedad = 2025 menos el año de inicio de actividad.",
  "Incluye 1.204.165 empresas activas con fecha válida de inicio; 60 años o más se agrupa."
)

p_base <- ggplot(df, aes(x = antiguedad_anios, y = porcentaje_empresas)) +
  geom_col(
    width = 0.9,
    fill = "#2D7DB3",
    colour = "white",
    linewidth = 0.08
  ) +
  geom_vline(
    xintercept = mediana,
    colour = "#1C4D6E",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  annotate(
    "label",
    x = mediana + 2,
    y = max(df$porcentaje_empresas) * 0.92,
    label = "Mediana: 11 a\u00f1os",
    size = 3,
    hjust = 0,
    linewidth = 0.2,
    fill = "white",
    colour = "#1C4D6E"
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, by = 10),
    labels = c(as.character(seq(0, 50, by = 10)), "60+"),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = wrap_title_house(
      "En 2025, más de la mitad de las empresas activas ecuatorianas tenía 11 años o más de antigüedad"
    ),
    subtitle = wrap_subtitle_house(
      "Antigüedad de empresas activas, Ecuador, 2025"
    ),
    x = "Antig\u00fcedad de la empresa (a\u00f1os)",
    y = "Porcentaje de empresas activas",
    caption = wrap_caption_house(caption_txt)
  ) +
  theme_quantificador() +
  theme(
    panel.grid.major.y = element_line(
      colour = "grey88",
      linewidth = 0.35,
      linetype = "dashed"
    ),
    panel.grid.minor = element_blank()
  )

p_final <- add_logo(p_base, y = 0.18)
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

ggsave(
  out_path,
  plot = p_final,
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
