# ============================================================
# utils.R — Funciones compartidas para los gráficos de
#            El Quantificador
# Usar con: source("scripts/utils.R")  (desde la raíz del proyecto)
# ============================================================

source("scripts/packages.R")
ensure_packages(c("ggplot2", "cowplot"))

# ---- Constantes ----
LOGO_PATH <- "quantificador.png"

# ---- Tema base ----
#' Tema ggplot2 compartido para todos los gráficos de El Quantificador
theme_quantificador <- function() {
  theme_classic() +
    theme(
      axis.text             = element_text(colour = "grey20", size = 7.5),
      axis.title.x          = element_text(size = 7, margin = margin(t = 8, r = 0, b = 0, l = 0), hjust = 0),
      axis.title.y          = element_text(size = 7, margin = margin(r = 6), hjust = 1),
      plot.title            = element_text(colour = "grey20", size = 12.5, face = "bold", hjust = 0),
      plot.subtitle         = element_text(colour = "grey30", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption          = element_text(colour = "grey30", size = 5, lineheight = 1.1, hjust = 0,
                                           margin = margin(t = 6, r = 0, b = 0, l = 0)),
      axis.line             = element_line(colour = "grey60"),
      legend.position       = "none",
      panel.grid            = element_blank(),
      plot.margin           = margin(6, 36, 6, 16),
      plot.title.position   = "plot",
      plot.caption.position = "plot"
    )
}

# ---- Tema para gráficos de género/mujeres ----
#' Variante de tema para gráficos de estadísticas de mujeres (paleta morada)
theme_women <- function() {
  theme_bw() +
    theme(
      panel.grid             = element_blank(),
      panel.border           = element_blank(),
      plot.caption           = element_text(hjust = 0, face = "italic", size = 5,
                                            colour = "grey30", lineheight = 1.1,
                                            margin = margin(t = 6, r = 0, b = 0, l = 0)),
      legend.background      = element_blank(),
      legend.box.background  = element_blank(),
      text                   = element_text(color = "black", family = "sans"),
      axis.ticks.x           = element_blank(),
      axis.ticks.y           = element_line(color = "black"),
      axis.line.x            = element_line(color = "black"),
      axis.line.y            = element_line(color = "black"),
      plot.title             = element_text(colour = "grey20", size = 12.5, face = "bold", hjust = 0),
      plot.subtitle          = element_text(colour = "grey30", size = 9, lineheight = 1.1, hjust = 0,
                                            margin = margin(b = 8)),
      axis.text              = element_text(size = 7.5),
      axis.title.x           = element_text(size = 7, margin = margin(t = 8, r = 0, b = 0, l = 0), hjust = 0),
      axis.title.y           = element_text(size = 7, margin = margin(r = 6), hjust = 1),
      plot.margin            = margin(14, 36, 4, 16),
      plot.title.position    = "plot",
      plot.caption.position  = "plot"
    )
}

# ---- Formatters ----
#' Intl-style number formatter with decimal comma and thousands point
label_number_intl <- function(...) {
  scales::label_number(big.mark = ".", decimal.mark = ",", ...)
}

#' Intl-style percent formatter with decimal comma
label_percent_intl <- function(...) {
  scales::label_percent(decimal.mark = ",", ...)
}

#' Intl-style dollar formatter with decimal comma and thousands point
label_dollar_intl <- function(...) {
  scales::label_dollar(big.mark = ".", decimal.mark = ",", ...)
}

#' Intl-style inline percent text
percent_intl <- function(x, ...) {
  scales::percent(x, decimal.mark = ",", ...)
}

# ---- Logo overlay ----
#' Superpone el logo sobre un ggplot usando cowplot
#' @param plot      Un objeto ggplot
#' @param logo_path Ruta al archivo de imagen del logo
#' @param x, y      Posición de la esquina inferior-izquierda del logo (fracción 0–1)
#' @param width, height Tamaño del logo (fracción del área del gráfico)
add_logo <- function(plot,
                     logo_path = LOGO_PATH,
                     x = 0.88, y = 0.07,
                     width = 0.09, height = 0.09) {
  ggdraw() +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    ) +
    draw_plot(plot, x = 0, y = 0, width = 1, height = 1) +
    draw_image(logo_path, x = x, y = y, width = width, height = height)
}
