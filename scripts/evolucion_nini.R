# ============================================================
# 1. Cargar Paquetes
# ============================================================
library(data.table)
library(survey)
library(ggplot2)
library(scales)
library(cowplot)
library(grid)

# ============================================================
# 2. Definir rutas del proyecto (estructura principal)
# ============================================================
args_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(args_file) > 0) {
  normalizePath(sub("^--file=", "", args_file), winslash = "/")
} else {
  normalizePath(file.path("scripts", "evolucion_nini.R"), winslash = "/", mustWork = FALSE)
}

project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
ruta <- file.path(project_root, "data", "evolucion")
logo_path <- file.path(project_root, "quantificador.png")
output_path <- file.path(project_root, "figures", "evolucion_nini.png")

if (!dir.exists(ruta)) {
  stop("No se encontró la carpeta de datos en: ", ruta)
}

archivos <- list.files(ruta, pattern = "\\.csv$", full.names = TRUE)

if (length(archivos) == 0) {
  stop("No se encontraron archivos CSV en: ", ruta)
}

vars_interes <- c("p03","p20","p21","p22","p07","p09",
                  "area","p02","fexp","estrato","upm",
                  "condact","empleo","periodo")

enemdu <- rbindlist(
  lapply(archivos, function(f) fread(f, select = vars_interes)),
  use.names = TRUE, fill = TRUE
)

# ============================================================
# 3. Crear variable NINI
# ============================================================
enemdu[, nini := fifelse(
  p20 == 2 & p21 == 12 & p22 == 2 &   # No trabaja
    p07 == 2 & p09 != 6,               # No estudia ni nivelación SENESCYT
  1L, 0L
)]

# ============================================================
# 4. Ordenar y revisar periodos
# ============================================================
enemdu[, periodo := as.integer(periodo)]
setorder(enemdu, periodo)
unique(enemdu$periodo)

# ============================================================
# 5. Filtrar población 15–34 años
# ============================================================
enemdu_15_34 <- enemdu[p03 >= 15 & p03 <= 34 & !is.na(p03)]

# Crear variable sexo legible
enemdu_15_34[, sexo := factor(p02, levels = c(1, 2),
                              labels = c("Hombres", "Mujeres"))]

# ============================================================
# 6. Definir diseño muestral
# ============================================================
diseno <- svydesign(
  id = ~upm,
  strata = ~estrato,
  weights = ~fexp,
  data = enemdu_15_34,
  nest = TRUE
)

# ============================================================
# 7. Estimación mensual total
# ============================================================
nini_mensual <- svyby(
  ~nini,
  ~periodo,
  diseno,
  svymean,
  na.rm = TRUE
)

nini_mensual$pct_nini <- nini_mensual$nini * 100

# ============================================================
# 8. Estimación mensual por sexo
# ============================================================
nini_mensual_sexo <- svyby(
  ~nini,
  ~periodo + sexo,
  diseno,
  svymean,
  na.rm = TRUE
)

setDT(nini_mensual_sexo)

# Porcentaje mensual
nini_mensual_sexo[, `:=`(
  pct_nini = nini * 100
)]

setorder(nini_mensual_sexo, periodo)

# Convertir periodo YYYYMM a fecha para el gráfico
nini_mensual_sexo[, fecha := as.Date(
  paste0(sprintf("%06d", as.integer(periodo)), "01"),
  format = "%Y%m%d"
)]

# Mantener solo enero 2024 en adelante
nini_mensual_sexo <- nini_mensual_sexo[fecha >= as.Date("2024-01-01")]

# ============================================================
# 9. Gráfico
# ============================================================

p_base <- ggplot(nini_mensual_sexo,
  aes(x = fecha,
      y = pct_nini,
      color = sexo,
      fill = sexo,
      group = sexo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "La inactividad juvenil es mucho más alta entre las mujeres\nque entre los hombres",
    subtitle = "Evolución mensual del porcentaje de inactividad laboral y educativa (15-34 años)\npor sexo en Ecuador, desde enero 2024",
    x = "",
    y = "Porcentaje de jovenes que no estudian ni trabajan (%)",
    color = "Sexo",
    fill = "Sexo",
    caption = "Fuente: ENEMDU - INEC. Cálculos por el autor.\nNINI = 1 si la persona no trabaja y no estudia ni está en nivelación SENESCYT."
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_date(
    date_breaks = "3 months",
    labels = function(x) {
      meses_es <- c(
        "enero", "febrero", "marzo", "abril", "mayo", "junio",
        "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
      )
      paste(meses_es[as.integer(format(x, "%m"))], format(x, "%Y"))
    }
  ) +
  theme_classic() +
  theme(
    axis.text.y  = element_text(colour = "grey20", size = 6.5),
    axis.text.x  = element_text(colour = "grey20", size = 6.5, angle = 45, hjust = 1),
    axis.title.x = element_text(size = 6.5, margin = margin(t = 6, r = 0, b = 0, l = 0), hjust = 0),
    axis.title.y = element_text(size = 6.5, margin = margin(r = 5), hjust = 0),
    plot.title = element_text(colour = "grey20", size = 9.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "grey30", size = 7.5, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "grey30", size = 5.5, lineheight = 1.1, hjust = 0,
                                margin = margin(t = 3, r = 0, b = 0, l = 0)),
    axis.line = element_line(colour = "grey60"),
    legend.position = "bottom",
    legend.title = element_text(size = 6.5),
    legend.text = element_text(size = 6.5),
    legend.box.margin = margin(t = -8, r = 0, b = 0, l = 0),
    plot.margin = margin(12, 20, 8, 14),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

p1 <- ggdraw() +
  draw_plot(p_base, x = 0, y = 0, width = 1, height = 1) +
  draw_image(
    logo_path,
    x = 0.86, y = 0.14,
    width = 0.10, height = 0.10
  )

p1

# EXPORT, INSTAGRAM size

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, p1, width = 4, height = 5, dpi = 300)