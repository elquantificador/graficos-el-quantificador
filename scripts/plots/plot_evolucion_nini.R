# ============================================================
# plot_evolucion_nini.R
# Genera el gráfico de evolución mensual del porcentaje NINI
# (jóvenes que no estudian ni trabajan) por sexo.
# Requiere: data/processed/evolucion_nini.rds
# Guarda:   outputs/figures/03_inactividad-juvenil_sexo-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_evolucion_nini.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("data.table", "survey", "scales"))

df <- readRDS("data/processed/evolucion_nini.rds")

diseno <- svydesign(
  id      = ~upm,
  strata  = ~estrato,
  weights = ~fexp,
  data    = df,
  nest    = TRUE
)

nini_mensual_sexo <- svyby(~nini, ~periodo + sexo, diseno, svymean, na.rm = TRUE)
setDT(nini_mensual_sexo)

nini_mensual_sexo[, pct_nini := nini * 100]
setorder(nini_mensual_sexo, periodo)
nini_mensual_sexo[, fecha := as.Date(
  paste0(sprintf("%06d", as.integer(periodo)), "01"),
  format = "%Y%m%d"
)]
nini_mensual_sexo <- nini_mensual_sexo[fecha >= as.Date("2024-01-01")]

meses_es <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
              "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

portrait_path <- "outputs/figures/03_inactividad-juvenil_sexo-ecuador.png"

title_raw <- "La inactividad juvenil es mucho más alta entre las mujeres que entre los hombres"
subtitle_raw <- "Evolución mensual del porcentaje de inactividad laboral y educativa (15-34 años) por sexo en Ecuador, desde enero 2024"
caption_raw <- "Fuente: ENEMDU - INEC. Cálculos por el autor. NINI = 1 si la persona no trabaja y no estudia ni está en nivelación SENESCYT."

build_chart <- function() {
  spec <- house_spec("portrait")
    title_txt    <- "La inactividad juvenil es mucho más alta entre las mujeres\nque entre los hombres"
    subtitle_txt <- "Evolución mensual del porcentaje de inactividad laboral y educativa (15-34 años)\npor sexo en Ecuador, desde enero 2024"
    caption_txt  <- "Fuente: ENEMDU - INEC. Cálculos por el autor.\nNINI = 1 si la persona no trabaja y no estudia ni está en nivelación SENESCYT."

  ggplot(nini_mensual_sexo,
    aes(x = fecha, y = pct_nini, color = sexo, fill = sexo, group = sexo)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    labs(
      title    = title_txt,
      subtitle = subtitle_txt,
      x        = "",
      y        = "Porcentaje de jovenes que no estudian ni trabajan (%)",
      color    = "Sexo",
      fill     = "Sexo",
      caption  = caption_txt
    ) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_x_date(
      date_breaks = "3 months",
      labels = function(x) paste(meses_es[as.integer(format(x, "%m"))], format(x, "%Y"))
    ) +
    theme_quantificador() +
    theme(
      axis.text          = element_text(size = 6.5),
      axis.text.x        = element_text(angle = 45, hjust = 1),
      axis.title.y       = element_text(hjust = 0),
      plot.title         = element_text(size = 9.5),
      plot.subtitle      = element_text(size = 7.5),
      legend.position    = "bottom",
      legend.title       = element_text(size = 6.5),
      legend.text        = element_text(size = 6.5),
      legend.box.margin  = margin(t = -8, r = 0, b = 0, l = 0),
      plot.margin        = margin(12, 20, 8, 14)
    )
}

dir.create("outputs/figures", showWarnings = FALSE)

  spec <- house_spec("portrait")
  p_final <- house_apply_logo(build_chart(), "portrait", x = 0.88, y = 0.14)
  dest <- portrait_path
  ggsave(dest, p_final, width = spec$width, height = spec$height, dpi = spec$dpi, bg = "white")
  message("Guardado: ", dest)

