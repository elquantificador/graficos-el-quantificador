# ============================================================
# plot_femicidios.R
# Genera el gráfico de femicidios y otras muertes de mujeres
# por año (Fiscalía General del Estado, 2014–2025).
# Requiere: data/processed/femicidios.rds
# Guarda:   figures/femicidios_8m.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plot_femicidios.R
# ============================================================

source("scripts/utils.R")
library(dplyr)
library(stringr)

muertes_fem <- readRDS("data/processed/femicidios.rds")

purple_women  <- "#88398a"
purple_women2 <- "#52307c"

caption_grafo1 <- paste0(
  "Fuente: Fiscalía General del Estado. Femicidios corresponden al delito de femicidio según el art. 141 del Código Orgánico",
  "\n",
  str_wrap("Integral Penal. Las cifras de otras muertes incluyen asesinatos, homicidios intencionales, sicariatos, robos, ejecuciones extrajudiciales, entre otros. Elaboración por los autores.", 121)
)

p_base <- ggplot(muertes_fem, aes(x = as.character(año), y = cantidad, fill = tipo)) +
  geom_col(width = 0.7, position = "stack", color = "black") +
  labs(
    x        = "",
    y        = "Número de muertes",
    title    = "El gobierno ecuatoriano no conoce con exactitud\ncuantas mujeres mueren por femicidio cada año",
    subtitle = "Los femicidios en Ecuador han caído en los últimos años, pero las mujeres\nsiguen muriendo en contexto delictivo",
    fill     = "Tipo de muerte",
    caption  = caption_grafo1
  ) +
  scale_fill_manual(
    values = c("Femicidios" = purple_women, "Otras muertes" = purple_women2),
    limits = c("Femicidios", "Otras muertes")
  ) +
  scale_y_continuous(breaks = seq(0, 800, 100), limits = c(0, 800), expand = c(0, 0)) +
  geom_text(aes(label = cantidad),
            position = position_stack(vjust = 0.5),
            color = "white", size = 2.5, fontface = "bold") +
  theme_women() +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 6, color = "black"),
    legend.title       = element_text(size = 6, color = "black"),
    axis.text.y        = element_text(size = 6, color = "black"),
    axis.text.x        = element_text(size = 6, color = "black"),
    axis.ticks.y       = element_line(color = "black", linewidth = 0.3),
    legend.box.spacing = unit(2, "pt")
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.90, y = 0.07)
ggsave("figures/femicidios_8m.png", p_final,
       width = 4.5, height = 5.5, units = "in", dpi = 300, device = ragg::agg_png)
message("Guardado: figures/femicidios_8m.png")
