# ============================================================
# clean_enemdu_ingreso_hogar.R
# Agrega ingresos de la ENEMDU marzo 2026 a nivel de hogar
# usando id_hogar como identificador. Calcula ingreso laboral
# (ocupacion primaria y secundaria), ingresos de capital o
# inversiones, transferencias y prestaciones, y bonos.
# Requiere: data/raw/enemdu/enemdu_persona_2026_03.sav
# Guarda:   data/processed/enemdu_ingreso_hogar_2026_03.rds
#           outputs/tables/enemdu_ingreso_hogar_2026_03_summary.xlsx
#           outputs/tables/enemdu_ingreso_hogar_2026_03_summary.html
# Muestra:  resumen de ingreso_total_hogar
# ============================================================
# Ejecutar desde la raiz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_ingreso_hogar.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "haven", "readr", "openxlsx", "gt", "Hmisc"))

input_path <- "data/raw/enemdu/enemdu_persona_2026_03.sav"
rds_path <- "data/processed/enemdu_ingreso_hogar_2026_03.rds"
xlsx_path <- "outputs/tables/enemdu_ingreso_hogar_2026_03_summary.xlsx"
html_path <- "outputs/tables/enemdu_ingreso_hogar_2026_03_summary.html"

enemdu_ingreso_hogar <- read_sav(input_path) %>%
  mutate(
    across(
      c(p63, p66, p68b, p69, p70b, p71b, p72b, p73b, p74b, p76, p78),
      ~ as.numeric(zap_labels(.x))
    ),
    across(
      c(p63, p66, p68b, p69, p70b, p71b, p72b, p73b, p74b, p76, p78),
      ~ na_if(.x, 999999)
    ),
    across(
      c(p63, p66, p68b, p69, p70b, p71b, p72b, p73b, p74b, p76, p78),
      ~ na_if(.x, -1)
    ),
    across(
      c(p63, p66, p68b, p69, p70b, p71b, p72b, p73b, p74b, p76, p78),
      ~ replace_na(.x, 0)
    ),
    ingreso_laboral_primaria = p63 + p66 + p68b,
    ingreso_laboral_secundaria = p69 + p70b,
    ingreso_laboral_total = ingreso_laboral_primaria + ingreso_laboral_secundaria,
    ingreso_capital_inversiones = p71b,
    transferencias_prestaciones = p72b + p73b + p74b + p76 + p78,
    bdh = p76,
    bono_discapacidad = p78,
    ingreso_total_hogar_persona =
      ingreso_laboral_total +
      ingreso_capital_inversiones +
      transferencias_prestaciones
  ) %>%
  group_by(id_hogar) %>%
  summarise(
    area = first(area),
    fexp = first(fexp),
    ingreso_laboral_primaria = sum(ingreso_laboral_primaria, na.rm = TRUE),
    ingreso_laboral_secundaria = sum(ingreso_laboral_secundaria, na.rm = TRUE),
    ingreso_laboral_total = sum(ingreso_laboral_total, na.rm = TRUE),
    ingreso_capital_inversiones = sum(ingreso_capital_inversiones, na.rm = TRUE),
    transferencias_prestaciones = sum(transferencias_prestaciones, na.rm = TRUE),
    bdh = sum(bdh, na.rm = TRUE),
    bono_discapacidad = sum(bono_discapacidad, na.rm = TRUE),
    ingreso_total_hogar = sum(ingreso_total_hogar_persona, na.rm = TRUE),
    .groups = "drop"
  )

summary_total <- enemdu_ingreso_hogar %>%
  summarise(
    Promedio = weighted.mean(ingreso_total_hogar, w = fexp, na.rm = TRUE),
    `Cuartil 1` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.25, na.rm = TRUE)[[1]],
    Mediana = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.50, na.rm = TRUE)[[1]],
    `Cuartil 3` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.75, na.rm = TRUE)[[1]],
    `Quintil 1 (p20)` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.20, na.rm = TRUE)[[1]],
    `Quintil 2 (p40)` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.40, na.rm = TRUE)[[1]],
    `Quintil 3 (p60)` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.60, na.rm = TRUE)[[1]],
    `Quintil 4 (p80)` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.80, na.rm = TRUE)[[1]]
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "estadistico",
    values_to = "valor"
  ) %>%
  mutate(area = "Total")

summary_area <- enemdu_ingreso_hogar %>%
  mutate(
    area = case_when(
      area == 1 ~ "Urbana",
      area == 2 ~ "Rural",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(area) %>%
  summarise(
    Promedio = weighted.mean(ingreso_total_hogar, w = fexp, na.rm = TRUE),
    `Cuartil 1` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.25, na.rm = TRUE)[[1]],
    Mediana = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.50, na.rm = TRUE)[[1]],
    `Cuartil 3` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.75, na.rm = TRUE)[[1]],
    `Quintil 1 (p20)` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.20, na.rm = TRUE)[[1]],
    `Quintil 2 (p40)` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.40, na.rm = TRUE)[[1]],
    `Quintil 3 (p60)` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.60, na.rm = TRUE)[[1]],
    `Quintil 4 (p80)` = Hmisc::wtd.quantile(ingreso_total_hogar, weights = fexp, probs = 0.80, na.rm = TRUE)[[1]],
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -area,
    names_to = "estadistico",
    values_to = "valor"
  )

summary_table <- bind_rows(summary_total, summary_area) %>%
  pivot_wider(
    names_from = area,
    values_from = valor
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

saveRDS(enemdu_ingreso_hogar, rds_path)

wb <- createWorkbook()
addWorksheet(wb, "summary")
writeData(wb, "summary", summary_table)

header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  fgFill = "#D9E2F3",
  border = "Bottom"
)

currency_style <- createStyle(numFmt = "$#,##0.00")

addStyle(
  wb,
  "summary",
  style = header_style,
  rows = 1,
  cols = 1:ncol(summary_table),
  gridExpand = TRUE
)

addStyle(
  wb,
  "summary",
  style = currency_style,
  rows = 2:(nrow(summary_table) + 1),
  cols = 2:ncol(summary_table),
  gridExpand = TRUE
)

setColWidths(wb, "summary", cols = 1, widths = 22)
setColWidths(wb, "summary", cols = 2:ncol(summary_table), widths = 14)
freezePane(wb, "summary", firstRow = TRUE)

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

summary_table %>%
  gt() %>%
  fmt_currency(columns = c(Total, Urbana, Rural), currency = "USD", decimals = 2) %>%
  cols_label(
    estadistico = "Métrica",
    Total = "Total",
    Urbana = "Urbana",
    Rural = "Rural"
  ) %>%
  tab_header(
    title = "ENEMDU marzo 2026",
    subtitle = "Resumen ponderado del ingreso total de los hogares a nivel nacional y por área"
  ) %>%
  tab_source_note(
    source_note = "Fuente: ENEMDU, microdatos marzo 2026. Cálculos ponderados. Por: Daniel Sánchez."
  ) %>%
  gtsave(html_path)

print(summary_table)
message("Guardado: ", rds_path)
message("Guardado: ", xlsx_path)
message("Guardado: ", html_path)

