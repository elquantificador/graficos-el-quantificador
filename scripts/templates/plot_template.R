# ============================================================
# plot_template.R
# One-line description of what this script does.
# Requiere: data/processed/source_topic.rds
# Guarda:   outputs/figures/NN_slug-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_source_topic.R
# ============================================================

source('scripts/utils.R')
source('scripts/packages.R')
ensure_packages(c('dplyr', 'ggplot2', 'scales', 'ragg'))

in_path <- 'data/processed/source_topic.rds'
out_path <- 'outputs/figures/NN_slug-ecuador.png'

df <- readRDS(in_path)

caption_txt <- wrap_caption_house(
  'Fuente: ... Elaboración: ... Nota: ...'
)

p_base <- ggplot(df, aes(x = x, y = y)) +
  geom_col(fill = '#1f4e79') +
  labs(
    title = wrap_title_house('Titulo del grafico'),
    subtitle = wrap_subtitle_house('Subtitulo del grafico'),
    caption = caption_txt,
    x = NULL,
    y = NULL
  ) +
  theme_quantificador()

p_final <- add_logo(p_base)

dir.create('outputs/figures', recursive = TRUE, showWarnings = FALSE)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300, device = ragg::agg_png)
message('Guardado: ', out_path)
