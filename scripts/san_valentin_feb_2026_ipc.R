# ------------------------------------------------------------
# Grafico San Valentin IPC (Feb 2026)
# Fuente: INEC (IPC)
# Autor: El Quantificador
# Fecha: 2026-02-13
# Descripcion: Serie de precios para San Valentin
# ------------------------------------------------------------

# Paquetes (instalar si faltan)
if (!require(readxl)) install.packages("readxl", repos = "https://cloud.r-project.org")
if (!require(dplyr))  install.packages("dplyr",  repos = "https://cloud.r-project.org")
if (!require(tidyr))  install.packages("tidyr",  repos = "https://cloud.r-project.org")
if (!require(stringr))install.packages("stringr",repos = "https://cloud.r-project.org")
if (!require(lubridate)) install.packages("lubridate", repos = "https://cloud.r-project.org")
if (!require(ggplot2)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require(png))    install.packages("png",    repos = "https://cloud.r-project.org")
if (!require(jpeg))   install.packages("jpeg",   repos = "https://cloud.r-project.org")
if (!require(grid))   install.packages("grid",   repos = "https://cloud.r-project.org")
if (!require(cowplot))install.packages("cowplot",repos = "https://cloud.r-project.org")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(png)
library(jpeg)
library(grid)
library(cowplot)

# ---- Rutas (EDITAR) ----
xlsx_path <- "data/ipc_ind_nac_reg_ciud_01_2026.xlsx"

out_path  <- "figures/valentines_ipc_ecuador.png"

logo_path <- "quantificador.png"

# ---- Helper: parsear etiquetas de mes ----
parse_month_any <- function(x) {
  x0 <- tolower(trimws(x))

  # Comun: "2026-01" o "2026_01"
  if (str_detect(x0, "^\\d{4}[-_/]\\d{2}$")) {
    y <- as.integer(str_sub(x0, 1, 4))
    m <- as.integer(str_sub(x0, 6, 7))
    return(as.Date(sprintf("%04d-%02d-01", y, m)))
  }

  # Comun: "ene-2026" "feb-2026" etc.
  # Mapa de abreviaturas en espanol
  mon_map <- c(
    "ene"="01","feb"="02","mar"="03","abr"="04","may"="05","jun"="06",
    "jul"="07","ago"="08","sep"="09","oct"="10","nov"="11","dic"="12"
  )
  if (str_detect(x0, "^(ene|feb|mar|abr|may|jun|jul|ago|sep|oct|nov|dic)[-_ ]\\d{4}$")) {
    mon <- str_match(x0, "^(ene|feb|mar|abr|may|jun|jul|ago|sep|oct|nov|dic)")[,2]
    yr  <- as.integer(str_match(x0, "(\\d{4})$")[,2])
    m   <- mon_map[[mon]]
    return(as.Date(sprintf("%04d-%s-01", yr, m)))
  }

  # Respaldo: intentar con lubridate (puede devolver NA)
  suppressWarnings(as.Date(x0))
}

# ---- Leer datos ----
sheets <- excel_sheets(xlsx_path)
raw <- 
    read_xlsx(xlsx_path, 
              sheet = sheets[2],
              skip =4 ) |> 
    janitor::clean_names()

# Se esperan al menos estas columnas (los nombres pueden variar)
# Estandarizamos:
# - columna de codigo: "Cód. CCIF" o similar
# - descripcion: "Descripción CCIF" o similar
# - ponderacion: "Ponderación" o similar
# - columnas de tiempo: indices mensuales

parse_month_any <- function(x) {
  x0 <- tolower(trimws(as.character(x)))

  mon_map <- c(
    ene="01", feb="02", mar="03", abr="04", may="05", jun="06",
    jul="07", ago="08", sep="09", oct="10", nov="11", dic="12"
  )

  mon <- stringr::str_match(x0, "^(ene|feb|mar|abr|may|jun|jul|ago|sep|oct|nov|dic)")[,2]
  yy  <- suppressWarnings(as.integer(stringr::str_match(x0, "_(\\d{2})$")[,2]))

  yyyy <- dplyr::if_else(is.na(yy), NA_integer_, 2000L + yy)
  mm   <- unname(mon_map[mon])

  # build Date; invalid combos become NA
  out <- as.Date(paste0(yyyy, "-", mm, "-01"))
  out[is.na(mon) | is.na(yy)] <- as.Date(NA)
  out
}

# Data prep -------------------------------

df <-
    raw |> 
    select(
        cod_ccif,
        descripcion_ccif,
        ene_15:ene_26
    ) |> 
    filter(cod_ccif %in% c("11", "01182094", "09421293", "0933186", "09331286")) |> 
    pivot_longer(
        cols = ene_15:ene_26,
        names_to = "periodo",
        values_to = "valor"
    ) |> 
    mutate(periodo = parse_month_any(periodo),
           year = year(periodo),
           month = month(periodo)) |> 
    filter(month == 1, year %in% c(2016, 2026)) |>
    select(product = descripcion_ccif, cod_ccif, periodo, valor) |> 
    pivot_wider(names_from = periodo, values_from = valor) |> 
    janitor::clean_names() |> 
    mutate(inflation_2016_2026 = (x2026_01_01 - x2016_01_01) / x2016_01_01,
           product = case_when(
             cod_ccif == "11"        ~ "Restaurantes y hoteles",
             cod_ccif == "01182094" ~ "Chocolate",
             cod_ccif == "09421293" ~ "Entradas al cine",
             cod_ccif == "09331286" ~ "Flores",
             TRUE ~ product
           ))

# ---- Etiquetas, titulo, subtitulo, pie ----
library(stringr)

caption_txt <- "Fuente: Instituto Nacional de Estadística y Censos (INEC) - Índice de Precios al Consumidor (IPC). Elaboración:\nEl Quantificador. La inflación se calcula como el cambio porcentual del índice entre enero de 2016 y enero de 2026."

# ---- Grafico ----

p_base <- 
  ggplot(df, aes(x = reorder(product, inflation_2016_2026, decreasing = FALSE),
                 y = inflation_2016_2026)) +
  geom_bar(stat = "identity", width = 0.55, fill = "#ef9f4e") +
  geom_text(
    aes(label = scales::percent(inflation_2016_2026, accuracy = 0.1)),
    hjust = -0.10, vjust = 0.5, size = 2.5
  ) +
  labs(
    x = "",
    y = "Inflación acumulada (enero 2016 a enero 2026)",
    title = "El amor no es gratis (y cada vez cuesta más)",
    subtitle = "En 10 años, los productos de San Valentín cuestan hasta 37% más",
    caption = caption_txt
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.18))  # espacio para las etiquetas
  ) +
  coord_flip(clip = "off") +
  theme_classic() +
  theme(
    # Con coord_flip: nombres de productos = axis.text.y; porcentajes = axis.text.x
    axis.text.y  = element_text(colour = "grey20", size = 7.5, hjust = 0),
    axis.text.x  = element_text(colour = "grey20", size = 7.5),
    axis.title.x = element_text(size = 7, margin = margin(t = 8, r = 0, b = 0, l = 0), hjust = 0),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 0),

    plot.title    = element_text(colour = "grey20", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "grey30", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption  = element_text(colour = "grey30", size = 5, lineheight = 1.1, hjust = 0, margin = margin(t = 6, r = 0, b = 0, l = 0)),

    axis.line = element_line(colour = "grey60"),
    legend.position = "none",

    # Más margen derecho para que no se corten las etiquetas
    plot.margin = margin(14, 36, 14, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

p1 <- ggdraw() +
  draw_plot(p_base, x = 0, y = 0, width = 1, height = 1) +
  draw_image(
    logo_path,
    x = 0.90, y = 0.14,
    width = 0.10, height = 0.10
  )

p1

# ---- Guardar ----
ggsave(out_path, p1, width = 4, height = 5, units = "in", dpi = 300, device = ragg::agg_png)
