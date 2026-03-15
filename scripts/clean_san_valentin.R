# ============================================================
# clean_san_valentin.R
# Extrae del IPC (INEC) los productos relevantes para San
# Valentín y calcula la inflación acumulada 2016–2026.
# Guarda: data/processed/san_valentin.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/clean_san_valentin.R
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(janitor)

# Parsea etiquetas de columna con formato "ene_16", "feb_26", etc.
parse_month_any <- function(x) {
  x0 <- tolower(trimws(as.character(x)))
  mon_map <- c(
    ene = "01", feb = "02", mar = "03", abr = "04", may = "05", jun = "06",
    jul = "07", ago = "08", sep = "09", oct = "10", nov = "11", dic = "12"
  )
  mon  <- str_match(x0, "^(ene|feb|mar|abr|may|jun|jul|ago|sep|oct|nov|dic)")[, 2]
  yy   <- suppressWarnings(as.integer(str_match(x0, "_(\\d{2})$")[, 2]))
  yyyy <- if_else(is.na(yy), NA_integer_, 2000L + yy)
  mm   <- unname(mon_map[mon])
  out  <- as.Date(paste0(yyyy, "-", mm, "-01"))
  out[is.na(mon) | is.na(yy)] <- as.Date(NA)
  out
}

sheets <- excel_sheets("data/ipc_ind_nac_reg_ciud_01_2026.xlsx")
raw <- read_xlsx("data/ipc_ind_nac_reg_ciud_01_2026.xlsx", sheet = sheets[2], skip = 4) |>
  clean_names()

df <- raw |>
  select(cod_ccif, descripcion_ccif, ene_15:ene_26) |>
  filter(cod_ccif %in% c("11", "01182094", "09421293", "0933186", "09331286")) |>
  pivot_longer(cols = ene_15:ene_26, names_to = "periodo", values_to = "valor") |>
  mutate(
    periodo = parse_month_any(periodo),
    year    = year(periodo),
    month   = month(periodo)
  ) |>
  filter(month == 1, year %in% c(2016, 2026)) |>
  select(product = descripcion_ccif, cod_ccif, periodo, valor) |>
  pivot_wider(names_from = periodo, values_from = valor) |>
  clean_names() |>
  mutate(
    inflation_2016_2026 = (x2026_01_01 - x2016_01_01) / x2016_01_01,
    product = case_when(
      cod_ccif == "11"        ~ "Restaurantes y hoteles",
      cod_ccif == "01182094" ~ "Chocolate",
      cod_ccif == "09421293" ~ "Entradas al cine",
      cod_ccif == "09331286" ~ "Flores",
      TRUE                   ~ product
    )
  )

dir.create("data/processed", showWarnings = FALSE)
saveRDS(df, "data/processed/san_valentin.rds")
message("Guardado: data/processed/san_valentin.rds  (", nrow(df), " filas)")
