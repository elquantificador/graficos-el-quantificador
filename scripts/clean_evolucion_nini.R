# ============================================================
# clean_evolucion_nini.R
# Carga las series trimestrales ENEMDU y prepara los datos
# para el análisis de inactividad juvenil (NINI).
# Guarda: data/processed/evolucion_nini.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/clean_evolucion_nini.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("data.table"))

vars_interes <- c("p03", "p20", "p21", "p22", "p07", "p09",
                  "area", "p02", "fexp", "estrato", "upm",
                  "condact", "empleo", "periodo")

archivos <- list.files("data/evolucion", pattern = "\\.csv$", full.names = TRUE)
if (length(archivos) == 0) stop("No se encontraron archivos CSV en data/evolucion/")

enemdu <- rbindlist(
  lapply(archivos, function(f) fread(f, select = vars_interes)),
  use.names = TRUE, fill = TRUE
)

# NINI = no trabaja Y no estudia (ni está en nivelación SENESCYT)
enemdu[, nini := fifelse(
  p20 == 2 & p21 == 12 & p22 == 2 &
    p07 == 2 & p09 != 6,
  1L, 0L
)]

enemdu[, periodo := as.integer(periodo)]
setorder(enemdu, periodo)

enemdu_15_34 <- enemdu[p03 >= 15 & p03 <= 34 & !is.na(p03)]
enemdu_15_34[, sexo := factor(p02, levels = c(1, 2), labels = c("Hombres", "Mujeres"))]

dir.create("data/processed", showWarnings = FALSE)
saveRDS(enemdu_15_34, "data/processed/evolucion_nini.rds")
message("Guardado: data/processed/evolucion_nini.rds  (", nrow(enemdu_15_34), " filas)")
