# ============================================================
# plot_52_b_sipa_colada_difuntos.R
# Precio mayorista por libra de las frutas de la colada morada:
# septiembre vs. semana previa al Día de Difuntos, 2025.
# Requiere: data/raw/colada_morada/sipa_precios_sep_oct_2025.csv
# Guarda:   outputs/figures/52_b_sipa-colada-difuntos-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_52_b_sipa_colada_difuntos.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

out_path <- "outputs/figures/52_b_sipa-colada-difuntos-ecuador.png"

usd <- function(x) paste0("$", sub(".", ",", sprintf("%.2f", x), fixed = TRUE))

df <- read.csv("data/raw/colada_morada/sipa_precios_sep_oct_2025.csv",
               encoding = "UTF-8") |>
  arrange(desc(cambio)) |>
  mutate(
    fruta    = factor(fruta, levels = rev(fruta)),
    pct      = floor(cambio * 100 + 0.5),
    etiqueta = paste0(ifelse(pct > 0, "+", ""), pct, "%"),
    y        = as.numeric(fruta)
  )

col_oct <- "#D97729"
col_sep <- "#2D7DB3"
fuente  <- "Arial"
y_top   <- max(df$y)
mora    <- df[df$y == y_top, ]

caption_txt <- wrap_caption_house(paste0(
  "Fuente: Ministerio de Agricultura y Ganadería (MAG), Sistema de Información ",
  "Pública Agropecuaria (SIPA), precios mayoristas diarios por mercado, 2025. ",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador. Nota: promedio por libra entre los mercados con ",
  "datos en ambos periodos. Septiembre: promedio del mes; octubre: semana del 26 ",
  "de octubre al 1 de noviembre. Babaco solo en Ambato, Loja y Riobamba."
))

p_base <- ggplot(df, aes(y = fruta)) +
  geom_segment(aes(x = septiembre, xend = octubre, yend = fruta),
               colour = "grey85", linewidth = 0.8) +
  geom_point(aes(x = septiembre), colour = col_sep, size = 1.4) +
  geom_point(aes(x = octubre), colour = col_oct, size = 1.4) +
  # Aumento a la derecha del punto de octubre
  geom_text(aes(x = octubre + 0.03, label = etiqueta),
            hjust = 0, size = 2.5, colour = col_oct, fontface = "bold",
            family = fuente) +
  # Precios en dólares debajo de cada punto
  geom_text(aes(x = septiembre, y = y - 0.32, label = usd(septiembre)),
            hjust = 1, size = 2.5, colour = col_sep, family = fuente) +
  geom_text(aes(x = octubre, y = y - 0.32, label = usd(octubre)),
            hjust = 0, size = 2.5, colour = col_oct, family = fuente) +
  annotate("text", x = mora$septiembre, y = y_top + 0.42, label = "Septiembre",
           colour = col_sep, size = 2.6, hjust = 0.5, family = fuente) +
  annotate("text", x = mora$octubre, y = y_top + 0.42, label = "Fin de octubre",
           colour = col_oct, size = 2.6, hjust = 0.5, family = fuente) +
  scale_x_continuous(
    labels = label_dollar_intl(accuracy = 0.01),
    breaks = seq(0, 1.25, 0.25),
    limits = c(0, 1.32),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x        = "Precio mayorista (USD por libra)",
    y        = NULL,
    title    = wrap_title_house(
      "De septiembre a octubre, la mora sube de precio en un 52% por Difuntos"
    ),
    subtitle = wrap_subtitle_house(
      "Precios de los ingredientes de la colada morada, septiembre y octubre de 2025"
    ),
    caption  = caption_txt
  ) +
  coord_cartesian(clip = "off") +
  theme_quantificador() +
  theme(text = element_text(family = fuente))

p_final <- add_logo(p_base, y = 0.20)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300,
       device = ragg::agg_png, bg = "white")
message("Guardado: ", out_path)
