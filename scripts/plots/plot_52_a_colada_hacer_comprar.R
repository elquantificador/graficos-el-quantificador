# ============================================================
# plot_52_a_colada_hacer_comprar.R
# Costo por litro de colada morada: hecha en casa vs. panadería,
# octubre 2026.
# Requiere: data/raw/colada_morada/costo_litro_colada_2026_10.csv
# Guarda:   outputs/figures/52_a_colada-hacer-comprar-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_52_a_colada_hacer_comprar.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

out_path <- "outputs/figures/52_a_colada-hacer-comprar-ecuador.png"

df <- read.csv("data/raw/colada_morada/costo_litro_colada_2026_10.csv",
               encoding = "UTF-8", na.strings = character(0)) |>
  mutate(
    etiqueta = ifelse(detalle == "", opcion, paste0(opcion, "\n(", detalle, ")")),
    etiqueta = factor(etiqueta, levels = rev(etiqueta)),
    valor    = paste0("$", sub(".", ",", sprintf("%.2f", costo_litro), fixed = TRUE))
  )

caption_txt <- wrap_caption_house(paste0(
  "Fuente: precios publicados por siete panaderías en sus sitios web, Rappi y ",
  "Uber Eats; supermercados en Rappi y tiendas en línea; MAG, SIPA, precios ",
  "mayoristas; receta de Laylita's Recipes. Consulta: 31 de octubre de 2026. ",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador. Nota: receta de 10 porciones, unos 4 litros, ",
  "sin gas ni tiempo de preparación. Al por mayor solo cambian mora, frutilla, ",
  "naranjilla y piña. Las apps de reparto pueden incluir recargo."
))

p_base <- ggplot(df, aes(x = etiqueta, y = costo_litro, fill = tipo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = valor), hjust = -0.15, size = 3, colour = "grey20",
            family = "Arial") +
  scale_fill_manual(values = c(casa = "#EF9F4E", panaderia = "#2D7DB3")) +
  scale_y_continuous(
    labels = label_dollar_intl(accuracy = 1),
    breaks = seq(0, 16, 4),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    x        = NULL,
    y        = "Costo por litro (USD), octubre 2026",
    title    = wrap_title_house("¿Hacer o comprar colada morada?"),
    subtitle = wrap_subtitle_house(
      "Precio de la colada morada por litro, por ubicación, octubre de 2026"
    ),
    caption  = caption_txt
  ) +
  coord_flip(clip = "off") +
  theme_quantificador() +
  theme(text = element_text(family = "Arial"))

p_final <- add_logo(p_base, y = 0.17)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300,
       device = ragg::agg_png, bg = "white")
message("Guardado: ", out_path)
