# ============================================================
# plot_escolaridad_ingreso.R
# Genera el gráfico de ingreso laboral por nivel educativo.
# Requiere: data/processed/escolaridad_ingreso.rds
# Guarda:   outputs/figures/04_ingresos_nivel-educativo-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_escolaridad_ingreso.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "survey", "scales", "ragg"))

df <- readRDS("data/processed/escolaridad_ingreso.rds")

options(survey.lonely.psu = "certainty")

des <- svydesign(
  id      = ~upm,
  strata  = ~estrato,
  weights = ~fexp,
  data    = df,
  nest    = TRUE
)

q <- svyby(
  ~ingrl, ~escolaridad, des,
  svyquantile,
  quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90),
  na.rm = TRUE, ci = FALSE, se = FALSE, keep.var = FALSE
)

boxstats <- as.data.frame(q) %>%
  transmute(
    escolaridad = escolaridad,
    ymin   = `1`,   # p10
    lower  = `2`,   # p25
    middle = `3`,   # p50
    upper  = `4`,   # p75
    ymax   = `5`    # p90
  )

portrait_path <- "outputs/figures/04_ingresos_nivel-educativo-ecuador.png"

title_raw <- "Solo los trabajadores con posgrados tienden a ganar más de USD 1,000 al mes"
caption_raw <- "Fuente: ENEMDU - INEC, enero 2026. Cálculos por el autor. Caja = p25–p75, línea = mediana, bigotes = p10–p90. Se implementan percentiles ponderados por pesos muestrales."

build_chart <- function(orientation) {
  spec <- house_spec(orientation)
  if (orientation == "landscape") {
    title_txt   <- stringr::str_wrap(title_raw, width = spec$title_wrap)
    caption_txt <- stringr::str_wrap(caption_raw, width = spec$caption_wrap)
  } else {
    title_txt   <- "Solo los trabajadores con posgrados\ntienden a ganar más de USD 1,000 al mes"
    caption_txt <- "Fuente: ENEMDU - INEC, enero 2026. Cálculos por el autor. Caja = p25–p75, línea = mediana, bigotes = p10–p90.\nSe implementan percentiles ponderados por pesos muestrales."
  }

  ggplot(boxstats, aes(x = escolaridad)) +
    geom_boxplot(
      aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
      stat = "identity",
      fill = "#ef9f4e",
      linewidth = 0.6
    ) +
    labs(
      title    = title_txt,
      subtitle = "Ingresos por nivel educativo, enero 2026, personas de 15+ años",
      x        = "",
      y        = "Ingreso laboral mensual (USD)",
      caption  = caption_txt
    ) +
    scale_y_continuous(
      labels = label_dollar_intl(accuracy = 1),
      breaks = seq(0, 3000, 500),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_cartesian(ylim = c(0, 3000)) +
    theme_quantificador(orientation) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

dir.create("outputs/figures", showWarnings = FALSE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  p_final <- house_apply_logo(build_chart(orientation), orientation, y = 0.20)
  dest <- house_out_path(portrait_path, orientation)
  ggsave(dest, p_final, width = spec$width, height = spec$height, units = "in",
         dpi = spec$dpi, device = ragg::agg_png, bg = "white")
  message("Guardado: ", dest)
}

