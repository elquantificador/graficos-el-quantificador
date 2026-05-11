# ============================================================
# clean_enemdu_ingreso_secemp.R
# Resume ingresos individuales por categoria secemp en la
# ENEMDU marzo 2026, con ponderacion fexp.
# Requiere: data/raw/enemdu/enemdu_persona_2026_03.sav
# Guarda:   tables/enemdu_ingreso_secemp_2026_03_summary.xlsx
#           tables/enemdu_ingreso_secemp_2026_03_summary.html
# ============================================================
# Ejecutar desde la raiz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_ingreso_secemp.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "haven", "readr", "openxlsx", "gt", "Hmisc"))

input_path <- "data/raw/enemdu/enemdu_persona_2026_03.sav"
xlsx_path <- "tables/enemdu_ingreso_secemp_2026_03_summary.xlsx"
html_path <- "tables/enemdu_ingreso_secemp_2026_03_summary.html"

enemdu_persona <- read_sav(input_path) %>%
  mutate(
    across(
      c(p63, p66, p68b, p69, p71b, p72b, p73b, p74b, p76, p78),
      ~ as.numeric(zap_labels(.x))
    ),
    across(
      c(p63, p66, p68b, p69, p71b, p72b, p73b, p74b, p76, p78),
      ~ na_if(.x, 999999)
    ),
    across(
      c(p63, p66, p68b, p69, p71b, p72b, p73b, p74b, p76, p78),
      ~ na_if(.x, -1)
    ),
    across(
      c(p63, p66, p68b, p69, p71b, p72b, p73b, p74b, p76, p78),
      ~ replace_na(.x, 0)
    ),
    secemp = as.numeric(zap_labels(secemp)),
    secemp_nombre = case_when(
      secemp == 1 ~ "Sector Formal",
      secemp == 2 ~ "Sector Informal",
      secemp == 3 ~ "Empleo Doméstico",
      secemp == 4 ~ "No Clasificados",
      TRUE ~ NA_character_
    ),
    ingreso_laboral_primaria = p63 + p66 + p68b,
    ingreso_laboral_secundaria = p69,
    ingreso_laboral_total = ingreso_laboral_primaria + ingreso_laboral_secundaria,
    ingreso_capital_inversiones = p71b,
    transferencias_prestaciones = p72b + p73b + p74b + p76 + p78,
    ingreso_total_persona =
      ingreso_laboral_total +
      ingreso_capital_inversiones +
      transferencias_prestaciones
  )

n_hogares <- enemdu_persona %>%
  summarise(n_hogares = n_distinct(id_hogar)) %>%
  pull(n_hogares)

enemdu_secemp <- enemdu_persona %>%
  filter(!is.na(secemp_nombre))

summary_total <- enemdu_secemp %>%
  summarise(
    `Número de hogares` = n_hogares,
    Promedio = weighted.mean(ingreso_total_persona, w = fexp, na.rm = TRUE),
    `Cuartil 1` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.25, na.rm = TRUE)[[1]],
    Mediana = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.50, na.rm = TRUE)[[1]],
    `Cuartil 3` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.75, na.rm = TRUE)[[1]],
    `Quintil 1 (p20)` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.20, na.rm = TRUE)[[1]],
    `Quintil 2 (p40)` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.40, na.rm = TRUE)[[1]],
    `Quintil 3 (p60)` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.60, na.rm = TRUE)[[1]],
    `Quintil 4 (p80)` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.80, na.rm = TRUE)[[1]]
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "metrica",
    values_to = "valor"
  ) %>%
  mutate(grupo = "Total")

summary_secemp <- enemdu_secemp %>%
  group_by(secemp_nombre) %>%
  summarise(
    `Número de hogares` = n_hogares,
    Promedio = weighted.mean(ingreso_total_persona, w = fexp, na.rm = TRUE),
    `Cuartil 1` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.25, na.rm = TRUE)[[1]],
    Mediana = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.50, na.rm = TRUE)[[1]],
    `Cuartil 3` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.75, na.rm = TRUE)[[1]],
    `Quintil 1 (p20)` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.20, na.rm = TRUE)[[1]],
    `Quintil 2 (p40)` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.40, na.rm = TRUE)[[1]],
    `Quintil 3 (p60)` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.60, na.rm = TRUE)[[1]],
    `Quintil 4 (p80)` = Hmisc::wtd.quantile(ingreso_total_persona, weights = fexp, probs = 0.80, na.rm = TRUE)[[1]],
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -secemp_nombre,
    names_to = "metrica",
    values_to = "valor"
  ) %>%
  rename(grupo = secemp_nombre)

summary_table <- bind_rows(summary_total, summary_secemp) %>%
  pivot_wider(
    names_from = grupo,
    values_from = valor
  )

dir.create("tables", showWarnings = FALSE, recursive = TRUE)

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
count_style <- createStyle(numFmt = "#,##0")

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
  style = count_style,
  rows = 2,
  cols = 2:ncol(summary_table),
  gridExpand = TRUE
)

addStyle(
  wb,
  "summary",
  style = currency_style,
  rows = 3:(nrow(summary_table) + 1),
  cols = 2:ncol(summary_table),
  gridExpand = TRUE
)

setColWidths(wb, "summary", cols = 1, widths = 22)
setColWidths(wb, "summary", cols = 2:ncol(summary_table), widths = 18)
freezePane(wb, "summary", firstRow = TRUE)

saveWorkbook(wb, xlsx_path, overwrite = TRUE)

summary_table %>%
  gt() %>%
  fmt_number(columns = -metrica, rows = metrica == "Número de hogares", decimals = 0) %>%
  fmt_currency(columns = -metrica, rows = metrica != "Número de hogares", currency = "USD", decimals = 2) %>%
  cols_label(metrica = "Métrica") %>%
  tab_header(
    title = "ENEMDU marzo 2026",
    subtitle = "Resumen ponderado del ingreso individual por categoría secemp"
  ) %>%
  tab_source_note(
    source_note = "Fuente: ENEMDU, microdatos marzo 2026. Cálculos ponderados. Por: Daniel Sánchez."
  ) %>%
  gtsave(html_path)

print(summary_table)
message("Hogares distintos en la base: ", n_hogares)
message("Guardado: ", xlsx_path)
message("Guardado: ", html_path)
