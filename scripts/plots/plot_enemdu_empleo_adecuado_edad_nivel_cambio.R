# ============================================================
# plot_enemdu_empleo_adecuado_edad_nivel_cambio.R
# Genera un gráfico horizontal del cambio interanual en el
# nivel de empleo adecuado por grupo de edad.
# Requiere: data/raw/enemdu/202603_Tabulados_Mercado_Laboral_EXCEL (2).xlsx
# Guarda:   figures/empleo_adecuado_grupo_edad_nivel_cambio_2026_03.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("readxl", "dplyr", "scales", "ragg", "stringr"))

out_path <- "figures/empleo_adecuado_grupo_edad_nivel_cambio_2026_03.png"
input_path <- "data/raw/enemdu/202603_Tabulados_Mercado_Laboral_EXCEL (2).xlsx"

pop_df <- readxl::read_xlsx(
  input_path,
  sheet = "1. Poblaciones",
  range = "A2:D2000",
  col_names = c("encuesta", "periodo", "indicador", "total")
)

char_df <- readxl::read_xlsx(
  input_path,
  sheet = "3.2 Caracterización Adec_pleno",
  range = "A2:DA13",
  col_names = FALSE
)

periods <- as.character(unlist(char_df[1, 3:ncol(char_df)]))
col_mar_25 <- which(periods == "mar-25") + 2
col_mar_26 <- which(periods == "mar-26") + 2

total_mar_25 <- pop_df %>%
  filter(periodo == "mar-25", indicador == "Empleo Adecuado/Pleno") %>%
  pull(total)
total_mar_25 <- as.numeric(total_mar_25[1])

total_mar_26 <- pop_df %>%
  filter(periodo == "mar-26", indicador == "Empleo Adecuado/Pleno") %>%
  pull(total)
total_mar_26 <- as.numeric(total_mar_26[1])

plot_df <- tibble::tibble(
  grupo_edad = factor(
    c(
      "Todos los grupos de edad",
      "Entre 15 y 24 años",
      "Entre 25 y 34 años",
      "Entre 35 y 44 años",
      "Entre 45 y 64 años"
    ),
    levels = rev(c(
      "Todos los grupos de edad",
      "Entre 15 y 24 años",
      "Entre 25 y 34 años",
      "Entre 35 y 44 años",
      "Entre 45 y 64 años"
    ))
  ),
  cambio_nivel = c(
    total_mar_26 - total_mar_25,
    as.numeric(unlist(char_df[[col_mar_26]][8])) * total_mar_26 - as.numeric(unlist(char_df[[col_mar_25]][8])) * total_mar_25,
    as.numeric(unlist(char_df[[col_mar_26]][9])) * total_mar_26 - as.numeric(unlist(char_df[[col_mar_25]][9])) * total_mar_25,
    as.numeric(unlist(char_df[[col_mar_26]][10])) * total_mar_26 - as.numeric(unlist(char_df[[col_mar_25]][10])) * total_mar_25,
    as.numeric(unlist(char_df[[col_mar_26]][11])) * total_mar_26 - as.numeric(unlist(char_df[[col_mar_25]][11])) * total_mar_25
  )
) %>%
  mutate(
    cambio_miles = cambio_nivel / 1000,
    etiqueta = ifelse(
      cambio_miles > 0,
      paste0("+", scales::number(cambio_miles, accuracy = 1, decimal.mark = ","), " mil"),
      paste0(scales::number(cambio_miles, accuracy = 1, decimal.mark = ","), " mil")
    ),
    x_label = dplyr::case_when(
      cambio_miles >= 0 ~ cambio_miles + 15,
      cambio_miles <= -80 ~ cambio_miles / 2,
      TRUE ~ cambio_miles - 15
    ),
    hjust_lab = dplyr::case_when(
      cambio_miles >= 0 ~ 0,
      cambio_miles <= -80 ~ 0.5,
      TRUE ~ 1
    ),
    color_lab = dplyr::case_when(
      cambio_miles <= -80 ~ "white",
      TRUE ~ "black"
    )
  )

title_txt <- stringr::str_wrap(
  "El nivel de empleo adecuado cayó más entre las personas de 45 a 64 años",
  width = 42
)

subtitle_txt <- stringr::str_wrap(
  "Cambio interanual en el nivel de empleo adecuado, marzo 2025 a marzo 2026",
  width = 54
)

caption_txt <- stringr::str_wrap(
  paste(
    "Fuente: INEC, Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU), tabulados de marzo de 2026.",
    "Cálculos de Daniel Sánchez para El Quantificador de Laboratorio LIDE.",
    "Empleo adecuado se refiere, en términos generales, a personas ocupadas que trabajan al menos la jornada legal y perciben ingresos laborales iguales o superiores al salario mínimo de referencia."
  ),
  width = 78
)

p_base <- ggplot(plot_df, aes(x = cambio_miles, y = grupo_edad)) +
  geom_col(aes(fill = cambio_miles >= 0), width = 0.62, show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.4) +
  geom_text(
    aes(x = x_label, label = etiqueta, hjust = hjust_lab),
    size = 2.5,
    colour = plot_df$color_lab
  ) +
  scale_fill_manual(values = c("TRUE" = "#2D7DB3", "FALSE" = "#EF9F4E")) +
  scale_x_continuous(
    labels = function(x) paste0(scales::number(x, accuracy = 1, decimal.mark = ","), " mil"),
    breaks = seq(-300, 100, 100),
    limits = c(-300, 100),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = "Cambio interanual en el nivel",
    y = NULL,
    caption = caption_txt
  ) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    axis.text.y = element_text(colour = "black", size = 8),
    axis.text.x = element_text(colour = "black", size = 7),
    axis.title.x = element_text(size = 7, margin = margin(t = 8), hjust = 0),
    axis.title.y = element_blank(),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 10)),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = "black"),
    plot.margin = margin(6, 58, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.89, y = 0.13)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)
