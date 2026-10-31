# ============================================================
# plot_52_b_sipa_colada_difuntos.R
# Precio mayorista de las frutas de la colada morada antes y
# después del Día de Difuntos, frente a septiembre (2021-2025).
# Requiere: data/raw/colada_morada/sipa_semana_difuntos_2021_2025.csv
# Guarda:   outputs/figures/52_b_sipa-colada-difuntos-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_52_b_sipa_colada_difuntos.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

out_path <- "outputs/figures/52_b_sipa-colada-difuntos-ecuador.png"

df <- read.csv("data/raw/colada_morada/sipa_semana_difuntos_2021_2025.csv",
               encoding = "UTF-8") |>
  mutate(
    fruta   = factor(fruta, levels = rev(fruta)),
    pct     = floor(antes * 100 + 0.5),
    etiqueta = paste0(ifelse(pct > 0, "+", ""), pct, "%")
  )

col_antes   <- "#D97729"
col_despues <- "#2D7DB3"

caption_txt <- wrap_caption_house(paste0(
  "Fuente: Ministerio de Agricultura y Ganadería (MAG), Sistema de Información ",
  "Pública Agropecuaria (SIPA), precios mayoristas diarios por mercado, 2021-2025. ",
  "Elaboración: El Quantificador. Nota: precio de cada mercado y año dividido ",
  "para su promedio de septiembre; mediana semanal. Antes: 26 oct a 1 nov. ",
  "Después: 9 a 15 nov. Babaco solo en Ambato, Loja y Riobamba."
))

fuente <- "Arial"

p_base <- ggplot(df, aes(y = fruta)) +
  geom_segment(aes(x = despues, xend = antes, yend = fruta),
               colour = "grey85", linewidth = 1.1) +
  annotate("segment", x = 0, xend = 0, y = 0.6, yend = 6.3,
           colour = "grey60", linewidth = 0.4, linetype = "dashed") +
  geom_point(aes(x = antes), colour = col_antes, size = 3.2) +
  geom_point(aes(x = despues), colour = col_despues, size = 2.2) +
  geom_text(aes(x = pmax(antes, despues) + 0.03, label = etiqueta),
            hjust = 0, size = 3, colour = col_antes, fontface = "bold",
            family = fuente) +
  annotate("text", x = 0.465, y = 6.45, label = "Semana antes",
           colour = col_antes, size = 2.6, hjust = 0.5, family = fuente) +
  annotate("text", x = 0.013, y = 6.45, label = "Semana después",
           colour = col_despues, size = 2.6, hjust = 0.5, family = fuente) +
  scale_x_continuous(
    labels = label_percent_intl(accuracy = 1),
    breaks = seq(-0.1, 0.5, 0.1),
    limits = c(-0.13, 0.55)
  ) +
  labs(
    x        = "Variación frente al precio mayorista de septiembre",
    y        = NULL,
    title    = wrap_title_house(
      "La mora de la colada morada sube 47% antes de Difuntos"
    ),
    subtitle = wrap_subtitle_house(
      "Precio mayorista de las frutas que lleva la colada morada, la semana antes y la semana después del feriado, frente a septiembre, mediana 2021-2025"
    ),
    caption  = caption_txt
  ) +
  coord_cartesian(clip = "off") +
  theme_quantificador() +
  theme(
    text         = element_text(family = fuente),
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank()
  )

p_final <- add_logo(p_base, y = 0.20)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300,
       device = ragg::agg_png, bg = "white")
message("Guardado: ", out_path)
