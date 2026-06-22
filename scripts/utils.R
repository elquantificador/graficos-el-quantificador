# ============================================================
# utils.R — Funciones compartidas para los gráficos de
#            El Quantificador
# Usar con: source("scripts/utils.R")  (desde la raíz del proyecto)
# ============================================================

source("scripts/packages.R")
ensure_packages(c("ggplot2", "cowplot", "stringr", "png"))

# ---- Constantes ----
LOGO_PATH <- "quantificador.png"
HOUSE_TITLE_SIZE_PT <- 12.5
HOUSE_SUBTITLE_SIZE_PT <- 9
HOUSE_CAPTION_SIZE_PT <- 6.5
HOUSE_TITLE_WRAP_WIDTH <- 38

house_wrap_width <- function(text_size_pt,
                             reference_width = HOUSE_TITLE_WRAP_WIDTH,
                             reference_size_pt = HOUSE_TITLE_SIZE_PT) {
  round(reference_width * reference_size_pt / text_size_pt)
}

HOUSE_SUBTITLE_WRAP_WIDTH <- house_wrap_width(HOUSE_SUBTITLE_SIZE_PT)
HOUSE_CAPTION_WRAP_WIDTH <- house_wrap_width(HOUSE_CAPTION_SIZE_PT)
HOUSE_SUBTITLE_WRAP_WIDTH <- 60
HOUSE_CAPTION_WRAP_WIDTH <- round(
  HOUSE_SUBTITLE_WRAP_WIDTH * HOUSE_SUBTITLE_SIZE_PT / HOUSE_CAPTION_SIZE_PT
)

# ---- Lienzo apaisado (landscape, 16:9) ----
# Segunda versión de cada gráfico para formatos anchos (slides / social).
# 8 x 4.5 in @ 300 dpi = 2400 x 1350 px (exacto 16:9). Se puede reducir a
# 1200 x 675 al entregar si una plataforma lo requiere. Los tamaños de fuente de
# la casa (en puntos) no cambian; sólo se reacomoda el ancho de envoltura.
LANDSCAPE_WIDTH_IN  <- 8
LANDSCAPE_HEIGHT_IN <- 4.5
LANDSCAPE_DPI       <- 300
LANDSCAPE_DIR       <- "outputs/figures/landscape"

# Ancho de envoltura apaisado: mismo principio que la casa, escalado al ancho
# usable del lienzo. La regla es aprovechar el espacio disponible — el título y
# el subtítulo deben caber en UNA línea siempre que sea posible, y sólo cortar a
# la siguiente línea cuando exceden el ancho usable. Por eso el ancho de título
# se escala por la razón de anchos usables (lienzo menos márgenes por defecto):
# portrait 4in con margin(.,36,.,16) ≈ 3,28in usable; landscape 8in con
# margin(.,20,.,16) = 7,5in usable. Subtítulo/caption se derivan del título con
# el MISMO helper que portrait (house_wrap_width), proporcional al tamaño de
# fuente, de modo que también llenen el ancho disponible.
.house_usable_in <- function(width_in, left_pt, right_pt) {
  width_in - (left_pt + right_pt) / 72
}
PORTRAIT_USABLE_IN  <- .house_usable_in(4, 16, 36)
LANDSCAPE_USABLE_IN <- .house_usable_in(LANDSCAPE_WIDTH_IN, 16, 20)

LANDSCAPE_TITLE_WRAP_WIDTH    <- round(
  HOUSE_TITLE_WRAP_WIDTH * LANDSCAPE_USABLE_IN / PORTRAIT_USABLE_IN
)
LANDSCAPE_SUBTITLE_WRAP_WIDTH <- house_wrap_width(
  HOUSE_SUBTITLE_SIZE_PT, reference_width = LANDSCAPE_TITLE_WRAP_WIDTH
)
LANDSCAPE_CAPTION_WRAP_WIDTH <- house_wrap_width(
  HOUSE_CAPTION_SIZE_PT, reference_width = LANDSCAPE_TITLE_WRAP_WIDTH
)

#' Ancho de envoltura apaisado para un tamaño de fuente concreto (pt)
#'
#' Llena el mismo ancho usable del lienzo que el título, pero al tamaño de fuente
#' indicado: a menor tamaño caben más caracteres en la misma anchura física. Útil
#' para captions (o títulos/subtítulos) que usan un tamaño distinto al de la casa
#' (caption 6,5 pt), para que igual aprovechen todo el ancho disponible.
#' Ej.: caption a 5,5 pt -> landscape_wrap_for_size(5.5).
landscape_wrap_for_size <- function(size_pt) {
  house_wrap_width(size_pt, reference_width = LANDSCAPE_TITLE_WRAP_WIDTH)
}

#' Especificación de lienzo por orientación
#'
#' Único lugar donde viven los números por orientación: dimensiones del lienzo,
#' resolución, anchos de envoltura de título/subtítulo/caption y la posición
#' vertical por defecto del logo. La rama "portrait" reproduce exactamente las
#' constantes históricas de la casa.
#' @param orientation "portrait" (4x5) o "landscape" (8x4.5, 16:9)
#' @return lista con width, height, dpi, dir, title_wrap, subtitle_wrap,
#'   caption_wrap y logo_y
house_spec <- function(orientation = c("portrait", "landscape")) {
  orientation <- match.arg(orientation)
  if (orientation == "landscape") {
    list(
      orientation   = "landscape",
      width         = LANDSCAPE_WIDTH_IN,
      height        = LANDSCAPE_HEIGHT_IN,
      dpi           = LANDSCAPE_DPI,
      dir           = LANDSCAPE_DIR,
      title_wrap    = LANDSCAPE_TITLE_WRAP_WIDTH,
      subtitle_wrap = LANDSCAPE_SUBTITLE_WRAP_WIDTH,
      caption_wrap  = LANDSCAPE_CAPTION_WRAP_WIDTH,
      logo_y        = 0.10
    )
  } else {
    list(
      orientation   = "portrait",
      width         = 4,
      height        = 5,
      dpi           = 300,
      dir           = "outputs/figures",
      title_wrap    = HOUSE_TITLE_WRAP_WIDTH,
      subtitle_wrap = HOUSE_SUBTITLE_WRAP_WIDTH,
      caption_wrap  = HOUSE_CAPTION_WRAP_WIDTH,
      logo_y        = 0.07
    )
  }
}

#' Aplica el logo sólo en portrait
#'
#' Los formatos apaisados (landscape) NO llevan el logo de El Quantificador.
#' En portrait se superpone con add_logo(); en landscape se devuelve el plot tal
#' cual (guardar con bg = "white"). Centraliza la regla "sin logo en landscape".
#' @param plot ggplot base
#' @param orientation "portrait" o "landscape"
#' @param ... argumentos para add_logo() (sólo se usan en portrait)
house_apply_logo <- function(plot, orientation = c("portrait", "landscape"), ...) {
  orientation <- match.arg(orientation)
  if (orientation == "landscape") plot else add_logo(plot, ...)
}

#' Ruta de salida para una orientación dada
#'
#' Portrait conserva la ruta histórica; landscape escribe el mismo nombre de
#' archivo bajo outputs/figures/landscape/.
#' @param portrait_path ruta portrait, p.ej. "outputs/figures/02_slug.png"
#' @param orientation "portrait" o "landscape"
house_out_path <- function(portrait_path, orientation = c("portrait", "landscape")) {
  orientation <- match.arg(orientation)
  if (orientation == "landscape") {
    file.path(LANDSCAPE_DIR, basename(portrait_path))
  } else {
    portrait_path
  }
}

# ---- Tema base ----
#' Tema ggplot2 compartido para todos los gráficos de El Quantificador
#'
#' @param orientation "portrait" (margen derecho amplio, por defecto) o
#'   "landscape" (margen derecho menor para el lienzo ancho). El resto de la
#'   tipografía y los colores no cambian. Los scripts pueden sobreescribir
#'   `plot.margin` con su propio `theme()` como siempre.
theme_quantificador <- function(orientation = c("portrait", "landscape")) {
  orientation <- match.arg(orientation)
  base_margin <- if (orientation == "landscape") margin(6, 20, 6, 16) else margin(6, 36, 6, 16)
  theme_classic() +
    theme(
      axis.text             = element_text(colour = "grey20", size = 7.5),
      axis.title.x          = element_text(size = 7, margin = margin(t = 8, r = 0, b = 0, l = 0), hjust = 0),
      axis.title.y          = element_text(size = 7, margin = margin(r = 6), hjust = 1),
      plot.title            = element_text(colour = "grey20", size = 12.5, face = "bold", hjust = 0),
      plot.subtitle         = element_text(colour = "grey30", size = 9, lineheight = 1.1, hjust = 0),
      plot.caption          = element_text(colour = "grey30", size = 6.5, lineheight = 1.1, hjust = 0,
                                           margin = margin(t = 6, r = 0, b = 0, l = 0)),
      axis.line             = element_line(colour = "grey60"),
      legend.position       = "none",
      panel.grid            = element_blank(),
      plot.margin           = base_margin,
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
      plot.caption           = element_text(hjust = 0, size = 6.5,
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

# ---- Text wrapping ----
#' Envuelve titulos usando el ancho visual de referencia de la casa
wrap_title_house <- function(text, width = HOUSE_TITLE_WRAP_WIDTH) {
  stringr::str_wrap(text, width = width)
}

#' Envuelve subtitulos para que coincidan visualmente con el ancho del titulo
wrap_subtitle_house <- function(text, width = HOUSE_SUBTITLE_WRAP_WIDTH) {
  stringr::str_wrap(text, width = width)
}

#' Envuelve captions para que coincidan visualmente con el ancho del titulo
wrap_caption_house <- function(text, width = HOUSE_CAPTION_WRAP_WIDTH) {
  stringr::str_wrap(text, width = width)
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
  logo_layer <- if (requireNamespace("magick", quietly = TRUE)) {
    draw_image(logo_path, x = x, y = y, width = width, height = height)
  } else {
    logo_img <- png::readPNG(logo_path)
    draw_grob(
      grid::rasterGrob(logo_img, interpolate = TRUE),
      x = x, y = y, width = width, height = height
    )
  }

  ggdraw() +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    ) +
    draw_plot(plot, x = 0, y = 0, width = 1, height = 1) +
    logo_layer
}
