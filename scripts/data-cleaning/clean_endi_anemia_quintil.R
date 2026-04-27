# ============================================================
# clean_endi_anemia_quintil.R
# Carga y procesa los datos de la ENDI R2 para análisis de
# prevalencia de anemia en niñas/os de 6 a 23 meses por quintil.
# Requiere: data/raw/endi_r2/BDD_ENDI_R2_f1_personas.rds
# Guarda:   data/processed/endi_r2_prev_anemia_quintil.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_endi_anemia_quintil.R
# ============================================================

library(haven)
library(dplyr)
library(survey)

path_rawdata_r2 <- "data/raw/endi_r2"
out_path <- "data/processed/endi_r2_prev_anemia_quintil.rds"

endi_r2_personas <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f1_personas.rds")) %>%
  mutate(
    quintil = as_factor(quintil),
    quintil = factor(
      quintil,
      levels = c("Quintil 1", "Quintil 2", "Quintil 3", "Quintil 4", "Quintil 5"),
      ordered = TRUE
    )
  )

options(survey.lonely.psu = "adjust")

endi_r2_personas_valid <- endi_r2_personas %>%
  filter(!is.na(ane6_23_new), !is.na(quintil))

endi_r2_design <- svydesign(
  ids = ~id_upm,
  strata = ~estrato,
  weights = ~fexp,
  data = endi_r2_personas_valid,
  nest = TRUE
)

prev_anemia_quintil <- svyby(
  ~ane6_23_new,
  ~quintil,
  endi_r2_design,
  svymean,
  na.rm = TRUE,
  vartype = c("ci")
) %>%
  as.data.frame() %>%
  transmute(
    quintil = quintil,
    prev_anemia = ane6_23_new,
    prev_anemia_low = ci_l,
    prev_anemia_upp = ci_u
  ) %>%
  left_join(
    endi_r2_personas_valid %>%
      count(quintil, name = "n"),
    by = "quintil"
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(prev_anemia_quintil, out_path)
message("Guardado: ", out_path)
