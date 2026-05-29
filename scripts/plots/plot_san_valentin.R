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

caption_txt <- paste0(
  "Fuente: Instituto Nacional de Estadística y Censos (INEC) - Índice de Precios al Consumidor (IPC). Elaboración:\n",
  "El Quantificador. La inflación se calcula como el cambio porcentual del índice entre enero de 2016 y enero de 2026."
)

p_base <- ggplot(df, aes(x = reorder(product, inflation_2016_2026, decreasing = FALSE),
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
  theme_quantificador() +
  theme(
    axis.text.y  = element_text(hjust = 0),
    axis.title.y = element_text(hjust = 0),
    plot.margin  = margin(14, 36, 14, 16)
  )

dir.create("outputs/figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.90, y = 0.14)
ggsave("outputs/figures/02_san-valentin_inflacion-ecuador.png", p_final,
       width = 4, height = 5, units = "in", dpi = 300, device = ragg::agg_png)
message("Guardado: outputs/figures/02_san-valentin_inflacion-ecuador.png")

