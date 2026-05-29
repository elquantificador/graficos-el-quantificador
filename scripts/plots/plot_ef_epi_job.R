# ============================================================
# plot_ef_epi_job.R
# Genera el gráfico de puntaje de inglés EF EPI 2025 por
# función laboral en Ecuador.
# Requiere: data/processed/ef_epi_job.rds
# Guarda:   outputs/figures/08_ingles_funcion-laboral-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_ef_epi_job.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("scales", "ragg", "forcats", "dplyr", "stringr"))

df <- readRDS("data/processed/ef_epi_job.rds")

# Translate job function names to Spanish
job_labels_es <- c(
  "Strategy & Project Management" = "Estrategia y gestión de proyectos",
  "Sales"                         = "Ventas",
  "IT"                            = "Tecnología (TI)",
  "Teacher"                       = "Docencia",
  "Marketing"                     = "Marketing",
  "Customer Service"              = "Atención al cliente",
  "Human Resources"               = "Recursos humanos",
  "Operations"                    = "Operaciones",
  "Admin & Clerical"              = "Administración y secretariado",
  "Legal"                         = "Legal",
  "Accounting & Finance"          = "Contabilidad y finanzas",
  "Technicians & Maintenance"     = "Técnicos y mantenimiento",
  "Unspecified / unemployed"      = "Sin especificar / desempleado",
  "Student"                       = "Estudiante"
)

df <- df |> mutate(job_function = recode(job_function, !!!job_labels_es))

# Shift baseline so bars start from y-axis at 0
score_min   <- 395
ecuador_avg <- 466

df <- df |> mutate(score_plot = score - score_min)

band_colors <- c(
  "Very low" = "#d94f3d",
  "Low"      = "#ef9f4e",
  "Moderate" = "#5ba35b",
  "High"     = "#2d7db3"
)

# CEFR zone backgrounds removed

caption_txt <- paste0(
  "Fuente: EF English Proficiency Index (EF EPI), edición 2025. Education First (EF). ",
  "Elaboración: El Quantificador de Laboratorio LIDE.\n",
  "Correspondencia CEFR: A2 <400 · B1 400–499 · B2 500–599 · C1 ≥600."
)

p_base <- ggplot(df, aes(x = fct_reorder(job_function, score), y = score_plot,
                          fill = proficiency_band)) +
  geom_col(width = 0.65) +
  # CEFR boundary lines (shifted)
  geom_hline(yintercept = c(5, 105, 205),
             linetype = "dotted", colour = "grey55", linewidth = 0.25) +
  # CEFR level labels above top bar (midpoints of zones, shifted)
  annotate("text",
    x     = 14.75,
    y     = c(55, 155, 220),
    label = c("B1", "B2", "C1"),
    size = 3.2, colour = "grey30", hjust = 0.5, fontface = "bold") +
  geom_text(
    aes(label = score),
    hjust = -0.15, vjust = 0.5, size = 2.5, colour = "grey20"
  ) +
  scale_fill_manual(
    values = band_colors,
    labels = c("Very low" = "Muy bajo", "Low" = "Bajo",
               "Moderate" = "Moderado", "High" = "Alto")
  ) +
  scale_y_continuous(
    breaks = c(5, 55, 105, 155, 205),
    labels = c("400", "450", "500", "550", "600"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    x        = "",
    y        = "Puntaje EF EPI (0–800)",
    title    = "Because they're nice? Los profesionales en Estrategia y\nGestión de Proyectos tienen el mejor inglés del Ecuador",
    subtitle = "Puntaje EF EPI 2025 por función laboral",
    caption  = caption_txt,
    fill     = "Nivel EF EPI"
  ) +
  coord_flip(clip = "off") +
  theme_quantificador() +
  theme(
    legend.position      = "bottom",
    legend.justification = "left",
    legend.direction     = "horizontal",
    legend.title         = element_text(size = 6.5, face = "bold"),
    legend.text          = element_text(size = 6.5),
    legend.key.width     = unit(3, "mm"),
    legend.key.height    = unit(3, "mm"),
    legend.box.margin    = margin(0, 0, 0, -55),
    axis.text.y      = element_text(hjust = 1),
    axis.title.x     = element_text(hjust = 0),
    plot.title       = element_text(size = 12.5, face = "bold", colour = "grey20", hjust = 0),
    plot.margin      = margin(14, 42, 14, 16)
  )

dir.create("outputs/figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.19, width = 0.09, height = 0.09)
ggsave("outputs/figures/08_ingles_funcion-laboral-ecuador.png", p_final,
       width = 5, height = 6, units = "in", dpi = 300, device = ragg::agg_png)
message("Guardado: outputs/figures/08_ingles_funcion-laboral-ecuador.png")

