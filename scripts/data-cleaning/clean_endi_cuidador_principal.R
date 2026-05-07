# ============================================================
# clean_endi_cuidador_principal.R
# Carga y procesa los datos de la ENDI R2 para analizar con
# quién permanece la mayor parte del tiempo el niño o niña.
# Requiere: data/raw/endi_r2/BDD_ENDI_R2_f2_salud_ninez.rds
# Guarda:   data/processed/endi_r2_cuidador_principal.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_endi_cuidador_principal.R
# ============================================================

library(haven)
library(dplyr)
library(survey)

path_rawdata_r2 <- "data/raw/endi_r2"
out_path <- "data/processed/endi_r2_cuidador_principal.rds"

endi_r2_salud_ninez <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f2_salud_ninez.rds")) %>%
  mutate(
    cuidador_raw = as_factor(f2_s5_512),
    cuidador_principal = case_when(
      cuidador_raw == "Madre" ~ "Madre",
      cuidador_raw %in% c("Padre", "Abuelo, abuela") ~ "Padre o abuelos",
      cuidador_raw %in% c(
        "Tíos/tías",
        "Miembros del hogar de 10 años y más",
        "Miembros del hogar menores de 10 años",
        "Otros familiares, vecinos/as o amigos/as",
        "Se queda solo",
        "Empleada o niñera"
      ) ~ "Otros",
      cuidador_raw == "Centro de Desarrollo Infantil (CDI)" ~ "Centro de Desarrollo Infantil (CDI)",
      TRUE ~ NA_character_
    ),
    cuidador_principal = factor(
      cuidador_principal,
      levels = c(
        "Madre",
        "Padre o abuelos",
        "Centro de Desarrollo Infantil (CDI)",
        "Otros"
      )
    )
  )

options(survey.lonely.psu = "adjust")

analysis_df <- endi_r2_salud_ninez %>%
  filter(!is.na(cuidador_principal))

endi_r2_design <- svydesign(
  ids = ~id_upm,
  strata = ~estrato,
  weights = ~fexp,
  data = analysis_df,
  nest = TRUE
)

share_cuidador <- svymean(~cuidador_principal, endi_r2_design, na.rm = TRUE)
share_ci <- confint(share_cuidador)

plot_df <- tibble(
  cuidador_principal = factor(
    sub("^cuidador_principal", "", names(coef(share_cuidador))),
    levels = levels(analysis_df$cuidador_principal)
  ),
  share = as.numeric(coef(share_cuidador)),
  share_low = share_ci[, 1],
  share_upp = share_ci[, 2]
) %>%
  left_join(
    analysis_df %>%
      count(cuidador_principal, name = "n"),
    by = "cuidador_principal"
  ) %>%
  mutate(n = coalesce(n, 0L))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(plot_df, out_path)
message("Guardado: ", out_path)
