# ============================================================
# clean_endi_lactancia_duracion_peso_scatter.R
# Prepara una base exploratoria para un scatter de duracion
# de lactancia exclusiva vs peso infantil.
# Requiere:
#   - data/raw/endi_r2/BDD_ENDI_R2_f1_personas.rds
#   - data/raw/endi_r2/BDD_ENDI_R2_f2_lactancia.rds
# Guarda: data/processed/endi_r2_lactancia_duracion_peso_scatter.rds
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr"))

path_rawdata_r2 <- "data/raw/endi_r2"
out_path <- "data/processed/endi_r2_lactancia_duracion_peso_scatter.rds"

endi_r2_personas <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f1_personas.rds"))
endi_r2_lactancia <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f2_lactancia.rds"))

children <- endi_r2_personas %>%
  transmute(
    id_hogar,
    id_per,
    grupo_edad_nin,
    weight_kg = rowMeans(
      cbind(f1_s5_4_1, f1_s5_4_2, f1_s5_4_3),
      na.rm = TRUE
    )
  ) %>%
  mutate(weight_kg = if_else(is.nan(weight_kg), NA_real_, weight_kg))

scatter_df <- endi_r2_lactancia %>%
  transmute(
    id_hogar,
    id_per,
    id_upm,
    estrato,
    fexp_lm,
    meses_lact_excl = as.numeric(f2_s3_307_1),
    dias_lact_excl = as.numeric(f2_s3_307_2)
  ) %>%
  mutate(
    meses_lact_excl = if_else(meses_lact_excl %in% c(77, 88, 99), NA_real_, meses_lact_excl),
    dias_lact_excl = if_else(dias_lact_excl %in% c(77, 88, 99), NA_real_, dias_lact_excl),
    dias_lact_excl = if_else(dias_lact_excl > 31, NA_real_, dias_lact_excl),
    duracion_lact_excl_meses = meses_lact_excl + coalesce(dias_lact_excl, 0) / 30
  ) %>%
  left_join(children, by = c("id_hogar", "id_per")) %>%
  filter(
    !is.na(duracion_lact_excl_meses),
    !is.na(weight_kg),
    is.finite(duracion_lact_excl_meses),
    duracion_lact_excl_meses >= 0,
    duracion_lact_excl_meses <= 24,
    weight_kg > 0,
    weight_kg < 30
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(scatter_df, out_path)
message("Guardado: ", out_path)
