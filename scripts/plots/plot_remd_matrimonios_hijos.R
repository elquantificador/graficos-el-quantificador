# ============================================================
# plot_remd_matrimonios_hijos.R
# Genera la curva acumulada de divorcios registrados por hijos.
# Requiere: data/processed/remd_matrimonios_hijos.rds
# Guarda:   outputs/figures/48_divorcios-hijos-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_remd_matrimonios_hijos.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "survival", "ragg"))

input_path <- "data/processed/remd_matrimonios_hijos.rds"
png_path <- "outputs/figures/48_divorcios-hijos-ecuador.png"

chart_data <- readRDS(input_path)$data

fit <- survival::survfit(
  survival::Surv(followup_years, event_registered_divorce) ~ children_group,
  data = chart_data
)

curve_summary <- summary(fit, censored = FALSE)
curve_points <- tibble::tibble(
  time = curve_summary$time,
  survival = curve_summary$surv,
  strata = as.character(curve_summary$strata)
) |>
  dplyr::mutate(
    children_group = sub("^[^=]+=", "", strata),
    event_share = 1 - survival
  ) |>
  dplyr::select(time, children_group, event_share)

end_summary <- summary(fit, times = 5, extend = TRUE)
end_points <- tibble::tibble(
  time = end_summary$time,
  survival = end_summary$surv,
  strata = as.character(end_summary$strata)
) |>
  dplyr::mutate(
    children_group = sub("^[^=]+=", "", strata),
    event_share = 1 - survival
  ) |>
  dplyr::select(time, children_group, event_share)

plot_data <- dplyr::bind_rows(
  tibble::tibble(
    time = 0,
    children_group = levels(chart_data$children_group),
    event_share = 0
  ),
  curve_points,
  end_points
) |>
  dplyr::mutate(
    children_group = factor(
      children_group,
      levels = c("0 hijos", "1 hijo", "2 hijos", "3 hijos", "4+ hijos")
    )
  ) |>
  dplyr::arrange(children_group, time)

labels <- end_points |>
  dplyr::mutate(
    children_group = factor(
      children_group,
      levels = c("0 hijos", "1 hijo", "2 hijos", "3 hijos", "4+ hijos")
    ),
    label = paste0(
      as.character(children_group),
      ": ",
      scales::percent(event_share, accuracy = 0.1, decimal.mark = ",")
    )
  )

palette_children <- c(
  "0 hijos" = "#1A3A5C",
  "1 hijo" = "#2D6A9F",
  "2 hijos" = "#3F7EA0",
  "3 hijos" = "#557F96",
  "4+ hijos" = "#6D8EA2"
)

p_base <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = time, y = event_share, colour = children_group, group = children_group)
) +
  ggplot2::geom_step(linewidth = 0.7, direction = "hv") +
  ggplot2::geom_text(
    data = labels,
    ggplot2::aes(x = 5.08, y = event_share, label = label),
    hjust = 0,
    size = 2.7,
    inherit.aes = FALSE
  ) +
  ggplot2::scale_colour_manual(values = palette_children) +
  ggplot2::scale_x_continuous(
    breaks = 0:5,
    limits = c(0, 6.3),
    expand = c(0, 0)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 0.06, 0.02),
    limits = c(0, 0.067),
    labels = scales::label_percent(accuracy = 1, decimal.mark = ","),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    title = wrap_title_house("A menos hijos, mayor la probabilidad de divorcio"),
    subtitle = wrap_subtitle_house("Porcentaje acumulado de matrimonios con un divorcio registrado, cohorte de 2020, seguimiento de cinco años"),
    x = "Años desde la inscripción",
    y = "Matrimonios con un divorcio registrado",
    caption = wrap_caption_house(paste(
      "Fuente: Registro Estadístico de Matrimonios y Divorcios (REMD) 2020-2025, INEC.",
      "Elaboración: Christian Salas para El Quantificador de Laboratorio LIDE.",
      "Nota: las curvas muestran el porcentaje acumulado de matrimonios con un divorcio registrado. Se excluyen 1.413 matrimonios sin una categoría válida de hijos reconocidos; los grupos incluyen 22.246, 5.879, 3.349, 1.365 y 738 matrimonios, respectivamente. El enlace usa campos completos y una combinación única de la fecha del matrimonio, el sexo y las fechas de nacimiento de los dos integrantes."
    ))
  ) +
  theme_quantificador() +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme(
    legend.position = "none",
    plot.margin = ggplot2::margin(6, 56, 6, 22)
  )

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
spec <- house_spec("portrait")

ggplot2::ggsave(
  filename = png_path,
  plot = house_apply_logo(p_base, "portrait", x = 0.84, y = 0.25),
  width = spec$width,
  height = spec$height,
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", png_path)
