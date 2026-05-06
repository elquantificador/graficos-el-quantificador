# ============================================================
# clean_endi_lactancia_ingreso_peso_scatter.R
# Prepara una base exploratoria para un scatter de peso infantil
# vs ingreso per capita del hogar, coloreado por lactancia al nacer.
# Requiere:
#   - data/raw/endi_r2/BDD_ENDI_R2_f1_personas.rds
#   - data/raw/endi_r2/BDD_ENDI_R2_f2_lactancia.rds
# Guarda: data/processed/endi_r2_lactancia_ingreso_peso_scatter.rds
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr"))

path_rawdata_r2 <- "data/raw/endi_r2"
out_path <- "data/processed/endi_r2_lactancia_ingreso_peso_scatter.rds"

ENDI_R2_persons <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f1_personas.rds"))
endi_r2_lactancia <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f2_lactancia.rds"))

income_vars <- c(
  "f1_s2_9", "f1_s2_10_2", "f1_s2_11",
  "f1_s2_12", "f1_s2_13", "f1_s2_14_2",
  "f1_s2_15", "f1_s2_16_2", "f1_s2_17_2",
  "f1_s2_18_2", "f1_s2_19_2", "f1_s2_20_2",
  "f1_s2_22"
)

ENDI_R2_persons <- ENDI_R2_persons %>%
  mutate(across(all_of(income_vars), as.numeric))

for (v in income_vars) {
  ENDI_R2_persons[[v]] <- case_when(
    ENDI_R2_persons[[v]] %in% c(9999999, 99999999) ~ 999999,
    TRUE ~ ENDI_R2_persons[[v]]
  )
}

ENDI_R2_persons <- ENDI_R2_persons %>%
  mutate(
    x = case_when(
      (f1_s2_9 == 999999 | f1_s2_12 == 999999) |
        (f1_s2_15 == 999999 & f1_s2_16_2 == 999999 &
           is.na(f1_s2_17_2) & is.na(f1_s2_18_2) &
           is.na(f1_s2_19_2) & is.na(f1_s2_20_2) &
           is.na(f1_s2_22)) ~ 1,
      (f1_s2_15 == 999999 & f1_s2_16_2 == 999999) &
        (!is.na(f1_s2_17_2) | !is.na(f1_s2_18_2) |
           !is.na(f1_s2_19_2) | !is.na(f1_s2_20_2) |
           !is.na(f1_s2_22)) ~ 2,
      TRUE ~ 0
    )
  )

for (v in income_vars) {
  ENDI_R2_persons[[v]] <- case_when(
    ENDI_R2_persons[[v]] == 999999 ~ NA_real_,
    TRUE ~ ENDI_R2_persons[[v]]
  )
}

ENDI_R2_persons <- ENDI_R2_persons %>%
  mutate(
    ind = rowSums(across(c(f1_s2_9, f1_s2_10_2, f1_s2_11)), na.rm = TRUE),
    asal = rowSums(across(c(f1_s2_12, f1_s2_13, f1_s2_14_2)), na.rm = TRUE),
    ila1 = ind + asal,
    ila2 = rowSums(across(c(f1_s2_15, f1_s2_16_2)), na.rm = TRUE),
    ila = ila1 + ila2,
    ila = if_else(ila1 < 0, ila2, ila),
    icap = f1_s2_17_2,
    ipens = f1_s2_18_2,
    ilocal = f1_s2_19_2,
    iextr = f1_s2_20_2,
    isocial = f1_s2_22,
    itrans = rowSums(across(c(icap, ipens, ilocal, iextr, isocial)), na.rm = TRUE),
    inla = icap + itrans,
    ii = ila + inla,
    ii = if_else(x == 1, NA_real_, ii),
    ii = if_else(x == 2, inla, ii),
    ii = if_else(ii == 0, NA_real_, ii)
  ) %>%
  group_by(id_hogar) %>%
  mutate(
    ih = sum(ii, na.rm = TRUE),
    ih = if_else(ih == 0, NA_real_, ih),
    hsize = n(),
    ingtot_pc = ih / hsize
  ) %>%
  ungroup()

children <- ENDI_R2_persons %>%
  transmute(
    id_hogar,
    id_per,
    persona,
    ingtot_pc,
    fexp,
    area,
    region,
    weight_kg = rowMeans(
      cbind(f1_s5_4_1, f1_s5_4_2, f1_s5_4_3),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    weight_kg = if_else(is.nan(weight_kg), NA_real_, weight_kg)
  )

scatter_df <- endi_r2_lactancia %>%
  transmute(
    id_hogar,
    id_per,
    id_upm,
    estrato,
    fexp_lm,
    breastfed_birth = case_when(
      f2_s3_302 == 1 ~ "Si dio el seno al nacer",
      f2_s3_302 == 2 ~ "No dio el seno al nacer",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(children, by = c("id_hogar", "id_per")) %>%
  filter(
    !is.na(breastfed_birth),
    !is.na(weight_kg),
    !is.na(ingtot_pc),
    is.finite(ingtot_pc),
    ingtot_pc > 0,
    weight_kg > 0,
    weight_kg < 30
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(scatter_df, out_path)
message("Guardado: ", out_path)
