# ============================================================
# clean_enemdu_ingreso_sector_laboral.R
# Prepara percentiles ponderados del ingreso laboral mensual
# por sector formal e informal en la ENEMDU de marzo 2026.
# Requiere: data/raw/enemdu/enemdu_persona_2026_03.sav
# Guarda:   data/processed/enemdu_ingreso_sector_laboral_2026_03.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_enemdu_ingreso_sector_laboral.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven", "Hmisc"))

input_path <- "data/raw/enemdu/enemdu_persona_2026_03.sav"
out_path <- "data/processed/enemdu_ingreso_sector_laboral_2026_03.rds"

wtd_q <- function(x, w, p) {
  as.numeric(Hmisc::wtd.quantile(x, weights = w, probs = p, na.rm = TRUE)[[1]])
}

enemdu_df <- haven::read_sav(input_path) %>%
  mutate(
    secemp = as.numeric(haven::zap_labels(secemp)),
    p05a = as.numeric(haven::zap_labels(p05a)),
    p05b = as.numeric(haven::zap_labels(p05b)),
    p03 = as.numeric(haven::zap_labels(p03)),
    empleo = as.numeric(haven::zap_labels(empleo)),
    ingrl = as.numeric(haven::zap_labels(ingrl)),
    fexp = as.numeric(fexp),
    secemp = case_when(
      is.na(secemp) & p05a == 10 & p05b == 10 ~ 2,
      is.na(secemp) & (p05a != 10 | p05b != 10) ~ 1,
      TRUE ~ secemp
    )
  ) %>%
  filter(
    empleo == 1,
    p03 >= 15,
    secemp %in% c(1, 2),
    !is.na(ingrl),
    !ingrl %in% c(-1, 999999),
    ingrl > 0,
    !is.na(fexp),
    fexp > 0
  ) %>%
  mutate(
    sector = factor(
      if_else(secemp == 1, "Formal", "Informal"),
      levels = c("Formal", "Informal")
    )
  )

overall_p90 <- wtd_q(enemdu_df$ingrl, enemdu_df$fexp, 0.90)

summary_df <- enemdu_df %>%
  filter(ingrl <= overall_p90) %>%
  group_by(sector) %>%
  summarise(
    personas_ponderadas = sum(fexp, na.rm = TRUE),
    p10 = wtd_q(ingrl, fexp, 0.10),
    p25 = wtd_q(ingrl, fexp, 0.25),
    mediana = wtd_q(ingrl, fexp, 0.50),
    p75 = wtd_q(ingrl, fexp, 0.75),
    p90 = wtd_q(ingrl, fexp, 0.90),
    promedio = weighted.mean(ingrl, fexp, na.rm = TRUE),
    .groups = "drop"
  )

chart_data <- list(
  summary = summary_df,
  metadata = list(
    period = "marzo 2026",
    p90_global = overall_p90,
    universo = "Personas ocupadas de 15 años o más con ingreso laboral positivo"
  )
)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(chart_data, out_path)
message("Guardado: ", out_path)
