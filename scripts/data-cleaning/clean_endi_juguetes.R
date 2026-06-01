# ============================================================
# clean_endi_juguetes.R
# Carga y procesa los datos de la ENDI R2 para analizar con
# qué juguetes o elementos juegan los niños y niñas.
# Requiere: data/raw/endi_r2/BDD_ENDI_R2_f3_desarrollo_inf.rds
# Guarda:   data/processed/endi_r2_juguetes.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_endi_juguetes.R
# ============================================================

library(dplyr)
library(survey)
library(tibble)

path_rawdata_r2 <- "data/raw/endi_r2"
out_path <- "data/processed/endi_r2_juguetes.rds"

toy_labels <- c(
  f3_s1_100_a = "Muñecas, carritos\ny otros juguetes",
  f3_s1_100_b = "Objetos del hogar\ny de la naturaleza",
  f3_s1_100_c = "Juguetes comprados\nen un almacen o mercado",
  f3_s1_100_d = "Juguetes para armar\no construir",
  f3_s1_100_e = "Juguetes para aprender\nformas, texturas o colores",
  f3_s1_100_f = "Muñecos y juegos\nde roles o fantasía",
  f3_s1_100_g = "Tablets, celulares\ny consolas"
)

toy_vars <- names(toy_labels)

endi_r2_desarrollo <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f3_desarrollo_inf.rds")) %>%
  mutate(
    across(
      all_of(toy_vars),
      ~ case_when(
        . == 1 ~ 1,
        . == 2 ~ 0,
        TRUE ~ NA_real_
      )
    )
  )

options(survey.lonely.psu = "adjust")

endi_r2_design <- svydesign(
  ids = ~id_upm,
  strata = ~estrato,
  weights = ~fexp_di,
  data = endi_r2_desarrollo,
  nest = TRUE
)

toy_formula <- as.formula(paste("~", paste(toy_vars, collapse = "+")))
toy_share <- svymean(toy_formula, endi_r2_design, na.rm = TRUE)
toy_ci <- confint(toy_share)

plot_df <- tibble(
  variable = names(coef(toy_share)),
  toy_type = unname(toy_labels[variable]),
  share = as.numeric(coef(toy_share)),
  share_low = toy_ci[, 1],
  share_upp = toy_ci[, 2],
  n = vapply(toy_vars, function(v) sum(!is.na(endi_r2_desarrollo[[v]])), integer(1))
) %>%
  mutate(
    toy_type = factor(toy_type, levels = toy_type[order(share)]),
    highlight = variable == "f3_s1_100_c"
  ) %>%
  arrange(share)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(plot_df, out_path)
message("Guardado: ", out_path)
