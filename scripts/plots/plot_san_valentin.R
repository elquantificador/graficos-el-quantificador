# ============================================================
# plot_san_valentin.R
# Genera el gráfico de inflación acumulada de productos de
# San Valentín (IPC enero 2016 – enero 2026).
# Requiere: data/processed/san_valentin.rds
# Guarda:   outputs/figures/02_san-valentin_inflacion-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_san_valentin.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("scales", "ragg"))

df <- readRDS("data/processed/san_valentin.rds")

caption_raw <- paste0(
  "Fuente: Instituto Nacional de Estadística y Censos (INEC) - Índice de Precios al ",
  "Consumidor (IPC). Elaboración: El Quantificador. La inflación se calcula como el ",
  "cambio porcentual del índice entre enero de 2016 y enero de 2026."
)

portrait_path <- "outputs/figures/02_san-valentin_inflacion-ecuador.png"

build_chart <- function(orientation) {
  spec <- house_spec(orientation)

  # Portrait conserva el caption histórico (con su salto manual) para que el PNG
  # 4x5 publicado no cambie ni un pixel; landscape reacomoda al ancho apaisado.
  caption_txt <- if (orientation == "landscape") {
    wrap_caption_house(caption_raw, width = spec$caption_wrap)
  } else {
    paste0(
      "Fuente: Instituto Nacional de Estadística y Censos (INEC) - Índice de Precios al Consumidor (IPC). Elaboración:\n",
      "El Quantificador. La inflación se calcula como el cambio porcentual del índice entre enero de 2016 y enero de 2026."
    )
  }

  ggplot(df, aes(x = reorder(product, inflation_2016_2026, decreasing = FALSE),
                 y = inflation_2016_2026)) +
    geom_bar(stat = "identity", width = 0.55, fill = "#ef9f4e") +
    geom_text(
      aes(label = percent_intl(inflation_2016_2026, accuracy = 0.1)),
      hjust = -0.10, vjust = 0.5, size = 2.5
    ) +
    labs(
      x        = "",
      y        = "Inflación acumulada (enero 2016 a enero 2026)",
      title    = "El amor no es gratis (y cada vez cuesta más)",
      subtitle = "En 10 años, los productos de San Valentín cuestan hasta 37% más",
      caption  = caption_txt
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.18))
    ) +
    coord_flip(clip = "off") +
    theme_quantificador(orientation) +
    theme(
      axis.text.y  = element_text(hjust = 0),
      axis.title.y = element_text(hjust = 0),
      plot.margin  = if (orientation == "landscape") margin(14, 20, 14, 16) else margin(14, 36, 14, 16)
    )
}

dir.create("outputs/figures", showWarnings = FALSE)
dir.create(LANDSCAPE_DIR, showWarnings = FALSE, recursive = TRUE)

for (orientation in c("portrait", "landscape")) {
  spec <- house_spec(orientation)
  # Sin logo en landscape; portrait mantiene el logo en su posición de la casa.
  p_final <- house_apply_logo(build_chart(orientation), orientation, x = 0.90, y = 0.14)
  out_path <- house_out_path(portrait_path, orientation)
  ggsave(out_path, p_final,
         width = spec$width, height = spec$height, units = "in",
         dpi = spec$dpi, device = ragg::agg_png, bg = "white")
  message("Guardado: ", out_path)
}

