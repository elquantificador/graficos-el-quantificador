# ============================================================
# plot_57_a_inec_ipc_arroz_incidencia.R
# Productos con mayor incidencia en la inflación mensual de octubre de 2026.
# Requiere: data/processed/inec_ipc_arroz.rds
# Guarda:   outputs/figures/57_a_arroz-incidencia-inflacion-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_inec_ipc_arroz.R
#   Rscript scripts/plots/plot_57_a_inec_ipc_arroz_incidencia.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

out_path <- "outputs/figures/57_a_arroz-incidencia-inflacion-ecuador.png"
inflacion_mensual <- 0.33
n_productos <- 10

incidencias <- readRDS("data/processed/inec_ipc_arroz.rds")$incidencias

plot_data <- incidencias |>
  dplyr::slice_max(incidencia_mensual_pct, n = n_productos) |>
  dplyr::mutate(
    destacado = producto == "Arroz",
    producto = stats::reorder(producto, incidencia_mensual_pct),
    etiqueta = formatC(incidencia_mensual_pct, format = "f", digits = 2, decimal.mark = ",")
  )

participacion <- plot_data$incidencia_mensual_pct[plot_data$destacado] / inflacion_mensual

title_text <- "El arroz aportó más de la mitad de la inflación de octubre"
subtitle_text <- "Incidencia en la variación mensual del IPC, por producto, octubre de 2026"
caption_text <- wrap_caption_house(paste(
  "Fuente: INEC, Índice de Precios al Consumidor, Boletín Técnico N° 10-2026.",
  "Elaboración: Daniel Sánchez Pazmiño para El Quantificador.",
  "Nota: La incidencia es el aporte de cada producto a la variación mensual",
  "del IPC, en puntos porcentuales; la suma de las incidencias de todos",
  "los productos da la inflación mensual, que fue de 0,33%. El arroz aportó",
  "0,20 puntos, cerca de", paste0(round(participacion * 100), "%"), "del total.",
  "Se muestran los diez productos con mayor incidencia entre los publicados",
  "por el INEC para las tres divisiones que más aportaron en octubre:",
  "alimentos y bebidas no alcohólicas, transporte, y servicios de restaurantes",
  "y alojamiento. En octubre, el precio del arroz subió 14,95% frente a septiembre."
))

p_base <- ggplot(plot_data, aes(x = incidencia_mensual_pct, y = producto)) +
  geom_col(aes(fill = destacado), width = 0.7) +
  geom_text(aes(label = etiqueta), hjust = -0.15, size = 3, colour = "grey20") +
  scale_fill_manual(values = c(`TRUE` = "#D97729", `FALSE` = "grey70")) +
  scale_x_continuous(
    labels = label_number_intl(accuracy = 0.05),
    breaks = seq(0, 0.20, by = 0.05),
    expand = expansion(mult = c(0, 0.15))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = wrap_title_house(title_text),
    subtitle = wrap_subtitle_house(subtitle_text),
    caption = caption_text,
    x = "Incidencia mensual (puntos porcentuales)",
    y = NULL
  ) +
  theme_quantificador() +
  theme(axis.ticks.y = element_blank())

p_final <- add_logo(p_base)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300, device = ragg::agg_png)
message("Guardado: ", out_path)
