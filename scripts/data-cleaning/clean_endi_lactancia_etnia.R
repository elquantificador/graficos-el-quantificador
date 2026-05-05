# ============================================================
# clean_endi_lactancia_etnia.R
# Carga y procesa ENDI R2 para estimar el porcentaje de
# ninos y ninas cuya madre les dio el seno al nacer, por etnia.
# Requiere:
#   - data/raw/endi_r2/BDD_ENDI_R2_f1_personas.rds
#   - data/raw/endi_r2/BDD_ENDI_R2_f2_lactancia.rds
# Guarda: data/processed/endi_r2_lactancia_nacer_etnia.rds
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "haven", "srvyr", "survey"))

path_rawdata_r2 <- "data/raw/endi_r2"
out_path <- "data/processed/endi_r2_lactancia_nacer_etnia.rds"

endi_r2_personas <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f1_personas.rds"))
endi_r2_lactancia <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f2_lactancia.rds"))

madres <- endi_r2_personas %>%
  filter(id_per == id_mef) %>%
  transmute(
    id_hogar,
    id_mef = id_per,
    etnia = as_factor(etnia)
  )

plot_df <- endi_r2_lactancia %>%
  left_join(madres, by = c("id_hogar", "id_mef")) %>%
  mutate(
    dio_seno_nacer = case_when(
      f2_s3_302 == 1 ~ 1,
      f2_s3_302 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(etnia), !is.na(dio_seno_nacer)) %>%
  as_survey_design(
    ids = id_upm,
    weights = fexp_lm,
    strata = estrato,
    nest = TRUE
  ) %>%
  group_by(etnia) %>%
  summarise(
    pct_dio_seno = survey_mean(dio_seno_nacer, vartype = c("ci"), na.rm = TRUE)
  ) %>%
  arrange(desc(pct_dio_seno))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(plot_df, out_path)
message("Guardado: ", out_path)
