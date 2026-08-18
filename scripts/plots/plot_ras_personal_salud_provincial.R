# ============================================================
# plot_ras_personal_salud_provincial.R
# Compara la presencia rural de obstetrices y TAPS.
#
# Entradas:
#   data/raw/ras/msp_serie_*.rds
#
# Salida:
#   outputs/figures/38_personal-salud_provincia-ecuador.png
# ============================================================

# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_ras_personal_salud_provincial.R

# set.seed(42) # El script no contiene procesos aleatorios.


# 1. Configuración ---------------------------------------------------------

source("scripts/utils.R")
source("scripts/packages.R")

ensure_packages(c(
  "dplyr",
  "ggplot2",
  "ragg",
  "scales",
  "tidyr"
))

raw_dir <- "data/raw/ras"
out_path <- "outputs/figures/38_personal-salud_provincia-ecuador.png"

occupation_cols <- c("tobst", "ttaps")
target_year <- 2021
start_year <- 2013
occupation_colors <- c(Obstetrices = "#D96C2C", TAPS = "#00A8CB")


# 2. Lectura de datos ------------------------------------------------------

ras <- setNames(
  lapply(
    c("nac", "prov", "cant", "parr", "area"),
    function(level) {
      readRDS(file.path(raw_dir, paste0("msp_serie_", level, ".rds")))
    }
  ),
  c("nac", "prov", "cant", "parr", "area")
)

# 3. Validaciones ----------------------------------------------------------

sum_occupations <- function(data, year) {
  data |>
    dplyr::filter(.data$anio == year) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(occupation_cols),
        ~ sum(.x, na.rm = TRUE)
      )
    )
}

available_years <- sort(unique(ras$nac$anio))
if (!target_year %in% available_years) {
  stop("El RAS no contiene el año objetivo: ", target_year, ".")
}

reference <- sum_occupations(ras$nac, target_year)

for (level in c("prov", "cant", "parr", "area")) {
  candidate <- sum_occupations(ras[[level]], target_year)
  difference <- abs(as.numeric(candidate) - as.numeric(reference))

  if (any(difference > 1e-8)) {
    stop(
      "La suma de ", level,
      " no coincide con la serie nacional en ", target_year, "."
    )
  }
}

area_codes <- as.numeric(unclass(ras$area$area))
area_labels <- attr(ras$area$area, "labels")
if (is.null(area_labels) || length(area_codes) != nrow(ras$area)) {
  stop("La variable de área no contiene etiquetas utilizables.")
}
area_label_map <- setNames(names(area_labels), as.character(unname(area_labels)))

area_ras <- ras$area |>
  dplyr::mutate(
    area_code = area_codes,
    area_label = unname(area_label_map[as.character(.data$area_code)])
  )

if (anyNA(area_ras$area_label)) {
  stop("La variable de área contiene códigos sin etiqueta.")
}


# 4. Indicadores -----------------------------------------------------------

area_totals <- area_ras |>
  dplyr::filter(.data$anio >= start_year, .data$anio <= target_year) |>
  dplyr::group_by(.data$anio, .data$area_label) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(occupation_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

if (anyNA(area_totals[[occupation_cols[1]]]) || anyNA(area_totals[[occupation_cols[2]]])) {
  stop("La serie de área contiene valores faltantes.")
}

rural_share <- area_totals |>
  dplyr::group_by(.data$anio) |>
  dplyr::summarise(
    Obstetrices = .data$tobst[.data$area_label == "Rural"] / sum(.data$tobst),
    TAPS = .data$ttaps[.data$area_label == "Rural"] / sum(.data$ttaps),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = c("Obstetrices", "TAPS"),
    names_to = "occupation",
    values_to = "rural_share"
  )

if (anyNA(rural_share$rural_share)) {
  stop("La participación rural contiene valores faltantes.")
}

endpoint_df <- rural_share |>
  dplyr::filter(.data$anio == target_year) |>
  dplyr::mutate(
    endpoint_label = paste0(
      .data$occupation, ": ",
      label_percent_intl(accuracy = 1)(.data$rural_share)
    )
  )

title_raw <- "TAPS y obstetrices tienen una mayor presencia rural"

subtitle_raw <- paste0(
  "Porcentaje del personal de cada tipo ubicado en áreas rurales, RAS, ",
  start_year, "-", target_year
)

caption_raw <- paste0(
  "Fuente: Registro de Actividades y Recursos de Salud (RAS). ",
  "Elaboración: Odalis Clemente y Alonso Quijano Ruiz para El Quantificador de Laboratorio LIDE. ",
  "En 2021, el área rural concentra ",
  label_percent_intl(accuracy = 1)(endpoint_df$rural_share[endpoint_df$occupation == "Obstetrices"]),
  " de las obstetrices y ",
  label_percent_intl(accuracy = 1)(endpoint_df$rural_share[endpoint_df$occupation == "TAPS"]),
  " de los TAPS. La serie de TAPS comienza en 2013."
)


# 5. Visualización ---------------------------------------------------------

chart <- ggplot2::ggplot(
  rural_share,
  ggplot2::aes(
    x = .data$anio,
    y = .data$rural_share,
    colour = .data$occupation,
    group = .data$occupation
  )
) +
  ggplot2::geom_hline(
    yintercept = 0.5,
    colour = "grey70",
    linetype = "dotted",
    linewidth = 0.4
  ) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::geom_text(
    data = endpoint_df,
    ggplot2::aes(label = .data$endpoint_label),
    hjust = -0.05,
    size = 2.4,
    show.legend = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(start_year, target_year, by = 2),
    expand = ggplot2::expansion(mult = c(0.02, 0.18))
  ) +
  ggplot2::scale_y_continuous(
    labels = label_percent_intl(accuracy = 1),
    limits = c(0.2, 0.6),
    breaks = seq(0.2, 0.6, by = 0.1),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::scale_colour_manual(
    values = occupation_colors,
    guide = "none"
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    x = "Año",
    y = "Personal ubicado en área rural",
    title = wrap_title_house(title_raw),
    subtitle = wrap_subtitle_house(subtitle_raw),
    caption = wrap_caption_house(caption_raw)
  ) +
  theme_quantificador() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_text(size = 7),
    axis.title.y = ggplot2::element_text(size = 7),
    plot.subtitle = ggplot2::element_text(size = 8),
    plot.margin = ggplot2::margin(10, 58, 6, 16)
  )


# 6. Exportación -----------------------------------------------------------

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")

ggplot2::ggsave(
  filename = out_path,
  plot = house_apply_logo(
    chart,
    "portrait",
    x = 0.89,
    y = 0.085,
    width = 0.065,
    height = 0.065
  ),
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)


# 7. Información de sesión -------------------------------------------------

sessionInfo()
