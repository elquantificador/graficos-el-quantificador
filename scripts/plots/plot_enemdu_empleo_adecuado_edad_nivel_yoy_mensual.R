# ============================================================
# plot_enemdu_empleo_adecuado_edad_nivel_yoy_mensual.R
# Genera un gráfico de la variación interanual mensual en el
# nivel de empleo adecuado por grupo de edad.
# Requiere: data/raw/enemdu/202603_Tabulados_Mercado_Laboral_EXCEL (2).xlsx
# Guarda:   figures/empleo_adecuado_grupo_edad_nivel_yoy_mensual.png
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("readxl", "dplyr", "tidyr", "lubridate", "ggplot2", "scales", "ragg", "stringr"))

out_path <- "figures/empleo_adecuado_grupo_edad_nivel_yoy_mensual.png"
input_path <- "data/raw/enemdu/202603_Tabulados_Mercado_Laboral_EXCEL (2).xlsx"

parse_enemdu_period <- function(x) {
  x <- tolower(trimws(as.character(x)))
  month_map <- c(ene = 1, feb = 2, mar = 3, abr = 4, may = 5, jun = 6,
                 jul = 7, ago = 8, sep = 9, oct = 10, nov = 11, dic = 12)
  parts <- regmatches(x, regexec("^([a-z]+)-?([0-9]{2,4})$", x))[[1]]
  if (length(parts) < 3) return(as.Date(NA))
  year <- as.integer(parts[3])
  if (year < 100) year <- 2000 + year
  as.Date(sprintf("%04d-%02d-01", year, month_map[[parts[2]]]))
}

pop_df <- readxl::read_xlsx(
  input_path,
  sheet = "1. Poblaciones",
  range = "A2:D2000",
  col_names = c("encuesta", "periodo", "indicador", "total")
) %>%
  filter(indicador == "Empleo Adecuado/Pleno") %>%
  mutate(
    total_nivel = as.numeric(total),
    fecha = as.Date(vapply(periodo, parse_enemdu_period, as.Date(NA)))
  ) %>%
  filter(fecha >= as.Date("2020-09-01"), fecha <= as.Date("2026-03-01")) %>%
  select(periodo, fecha, total_nivel)

char_df <- readxl::read_xlsx(
  input_path,
  sheet = "3.2 Caracterización Adec_pleno",
  range = "A2:DA13",
  col_names = FALSE
)

periods <- as.character(unlist(char_df[1, 3:ncol(char_df)]))
fechas <- as.Date(vapply(periods, parse_enemdu_period, as.Date(NA)))
keep <- fechas >= as.Date("2020-09-01") & fechas <= as.Date("2026-03-01")

shares_df <- tibble::tibble(
  periodo = periods[keep],
  fecha = fechas[keep],
  `Todos los grupos de edad` = 1,
  `Entre 15 y 24 años` = as.numeric(unlist(char_df[8, 3:ncol(char_df)]))[keep],
  `Prime age (25 a 64 años)` =
    as.numeric(unlist(char_df[9, 3:ncol(char_df)]))[keep] +
    as.numeric(unlist(char_df[10, 3:ncol(char_df)]))[keep] +
    as.numeric(unlist(char_df[11, 3:ncol(char_df)]))[keep],
  `65 años y más` = as.numeric(unlist(char_df[12, 3:ncol(char_df)]))[keep]
) %>%
  tidyr::pivot_longer(
    cols = -c(periodo, fecha),
    names_to = "grupo_edad",
    values_to = "share"
  ) %>%
  mutate(share = as.numeric(share))

plot_df <- shares_df %>%
  left_join(pop_df, by = c("periodo", "fecha")) %>%
  mutate(nivel = share * total_nivel) %>%
  group_by(grupo_edad) %>%
  arrange(fecha, .by_group = TRUE) %>%
  mutate(yoy_nivel = (nivel - lag(nivel, 12)) / 1000) %>%
  ungroup() %>%
  filter(!is.na(yoy_nivel), fecha >= as.Date("2025-01-01"))

label_df <- plot_df %>%
  group_by(grupo_edad) %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    label = case_when(
      grupo_edad == "Entre 15 y 24 años" ~ "15 a 24 años",
      grupo_edad == "65 años y más" ~ "65 años y más",
      grupo_edad == "Prime age (25 a 64 años)" ~ "Prime age",
      grupo_edad == "Todos los grupos de edad" ~ "Todos"
    ),
    x_label = fecha + 25,
    y_label = case_when(
      grupo_edad == "Entre 15 y 24 años" ~ yoy_nivel - 8,
      grupo_edad == "65 años y más" ~ yoy_nivel - 10,
      grupo_edad == "Prime age (25 a 64 años)" ~ yoy_nivel + 7,
      TRUE ~ yoy_nivel + 7
    )
  )

title_txt <- stringr::str_wrap(
  "El nivel de empleo adecuado cae con más fuerza entre jóvenes y mayores",
  width = 40
)

subtitle_txt <- stringr::str_wrap(
  "Cambio interanual mensual en el nivel de empleo adecuado por grupo de edad, 2025-2026",
  width = 54
)

caption_txt <- stringr::str_wrap(
  paste(
    "Fuente: INEC, Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU), tabulados de marzo de 2026.",
    "Cálculos de Daniel Sánchez para El Quantificador de Laboratorio LIDE.",
    "Empleo adecuado se refiere, en términos generales, a personas ocupadas que trabajan al menos la jornada legal y perciben ingresos laborales iguales o superiores al salario mínimo de referencia."
  ),
  width = 98
)

palette <- c(
  "Entre 15 y 24 años" = "#D95F02",
  "65 años y más" = "#7570B3",
  "Prime age (25 a 64 años)" = "#1B9E77",
  "Todos los grupos de edad" = "#EF9F4E"
)

p_base <- ggplot(plot_df, aes(x = fecha, y = yoy_nivel, color = grupo_edad)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.35) +
  geom_line(linewidth = 0.9) +
  geom_text(
    data = label_df,
    aes(x = x_label, y = y_label, label = label),
    hjust = 0,
    size = 2.3,
    fontface = "bold",
    color = "black",
    show.legend = FALSE
  ) +
  scale_color_manual(values = palette) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b",
    expand = expansion(mult = c(0.01, 0.10))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(scales::number(x, accuracy = 1, decimal.mark = ","), " mil"),
    breaks = seq(-300, 300, 100),
    limits = c(-300, 300),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = "Cambio interanual en el nivel",
    caption = caption_txt
  ) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    axis.text.y = element_text(colour = "black", size = 8),
    axis.text.x = element_text(colour = "black", size = 7),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 0.5, colour = "black"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 10)),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    plot.margin = margin(6, 78, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.86, y = 0.13, width = 0.075, height = 0.075)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4.7,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)
