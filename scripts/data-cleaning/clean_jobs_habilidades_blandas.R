# ============================================================
# clean_jobs_habilidades_blandas.R
# Consolida dos cortes de ofertas y calcula las habilidades blandas
# más mencionadas en las descripciones de las ofertas.
# Requiere: data/raw/jobs_scrape/jobs_20260617_043911.csv
#           data/raw/jobs_scrape/jobs_20260716_013707.csv
# Guarda:   data/processed/jobs_habilidades_blandas.rds
#           outputs/tables/jobs_habilidades_blandas.csv
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_jobs_habilidades_blandas.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr", "stringr", "tidyr", "purrr"))

june_path <- "data/raw/jobs_scrape/jobs_20260617_043911.csv"
july_path <- "data/raw/jobs_scrape/jobs_20260716_013707.csv"
out_rds_path <- "data/processed/jobs_habilidades_blandas.rds"
out_csv_path <- "outputs/tables/jobs_habilidades_blandas.csv"

top_n <- 5L

expected_unique_jobs <- 745L
expected_universe_jobs <- 437L
expected_description_jobs <- 434L

june_raw <- read_csv(
  june_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)
july_raw <- read_csv(
  july_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

june_snapshot_match <- str_match(basename(june_path), "jobs_(\\d{8})_(\\d{6})")
july_snapshot_match <- str_match(basename(july_path), "jobs_(\\d{8})_(\\d{6})")
june_snapshot_at <- as.POSIXct(
  str_c(june_snapshot_match[, 2], june_snapshot_match[, 3]),
  format = "%Y%m%d%H%M%S",
  tz = "America/Guayaquil"
)
july_snapshot_at <- as.POSIXct(
  str_c(july_snapshot_match[, 2], july_snapshot_match[, 3]),
  format = "%Y%m%d%H%M%S",
  tz = "America/Guayaquil"
)

if (nrow(june_raw) != n_distinct(june_raw$source, june_raw$job_id)) {
  stop("El corte de junio contiene claves source + job_id duplicadas.")
}
if (nrow(july_raw) != n_distinct(july_raw$source, july_raw$job_id)) {
  stop("El corte de julio contiene claves source + job_id duplicadas.")
}

all_jobs <- bind_rows(
  june_raw |>
    mutate(snapshot_file = basename(june_path), snapshot_at = june_snapshot_at),
  july_raw |>
    mutate(snapshot_file = basename(july_path), snapshot_at = july_snapshot_at)
) |>
  arrange(source, job_id, desc(snapshot_at)) |>
  distinct(source, job_id, .keep_all = TRUE)

if (nrow(all_jobs) != expected_unique_jobs) {
  stop(
    "El número de ofertas únicas cambió: se esperaban ", expected_unique_jobs,
    " ofertas y se obtuvieron ", nrow(all_jobs), "."
  )
}

universe_jobs <- all_jobs |>
  filter(str_to_lower(str_squish(in_universe)) == "true")

if (nrow(universe_jobs) != expected_universe_jobs) {
  stop(
    "El universo de ofertas cambió: se esperaban ", expected_universe_jobs,
    " ofertas y se obtuvieron ", nrow(universe_jobs), "."
  )
}

jobs <- universe_jobs |>
  filter(
    !is.na(description),
    str_squish(description) != ""
  ) |>
  mutate(
    job_text = str_c(coalesce(skills, ""), " ", coalesce(description, ""))
  )

if (nrow(jobs) != expected_description_jobs) {
  stop(
    "La base con descripción cambió: se esperaban ", expected_description_jobs,
    " ofertas y se obtuvieron ", nrow(jobs), "."
  )
}

# "Organización y planificación" evita el sustantivo genérico "organización"
# a solas: en el texto de las ofertas casi siempre se refiere a la empresa
# ("nuestra organización", "objetivos de la organización"), no a la
# competencia del candidato.
skill_terms <- tibble(
  skill = c(
    "Comunicación",
    "Trabajo en equipo",
    "Pensamiento analítico",
    "Organización y planificación",
    "Proactividad e iniciativa",
    "Orientación a resultados",
    "Resolución de problemas",
    "Liderazgo",
    "Adaptabilidad",
    "Negociación",
    "Creatividad e innovación",
    "Trabajo bajo presión"
  ),
  pattern = c(
    "comunicaci[oó]n|habilidades?\\s+comunicativas?",
    "trabaj[oa]s?\\s+en\\s+equipo|equipos?\\s+multidisciplinari|trabajo\\s+colaborativo|colaboraci[oó]n",
    "pensamiento\\s+anal[ií]tico|capacidad\\s+anal[ií]tica|pensamiento\\s+cr[ií]tico",
    "organizad\\w*|planificaci[oó]n|atenci[oó]n\\s+al\\s+detalle|capacidad(es)?\\s+de\\s+organizaci[oó]n|habilidad(es)?\\s+de\\s+organizaci[oó]n|organizaci[oó]n\\s+(del\\s+tiempo|y\\s+planificaci[oó]n|personal)",
    "proactiv\\w*|iniciativa",
    "orientaci[oó]n\\s+a\\s+resultados|orientad[oa]\\s+a\\s+resultados|enfoque\\s+a\\s+resultados|orientaci[oó]n\\s+al\\s+logro",
    "resoluci[oó]n\\s+de\\s+problemas|soluci[oó]n\\s+de\\s+problemas|resolver\\s+problemas",
    "lideraz\\w*|liderar\\s+equipos|gesti[oó]n\\s+de\\s+equipos",
    "adaptab\\w*|flexib\\w*",
    "negociaci[oó]n",
    "creativ\\w*|innovaci[oó]n",
    "trabaj\\w*\\s+bajo\\s+presi[oó]n"
  )
)

skill_hits <- map2(
  skill_terms$skill,
  skill_terms$pattern,
  \(skill, pattern) {
    jobs |>
      filter(str_detect(job_text, regex(pattern, ignore_case = TRUE))) |>
      transmute(source, job_id, skill)
  }
) |>
  list_rbind()

skill_summary <- skill_hits |>
  count(skill, name = "n_offers") |>
  right_join(skill_terms |> select(skill), by = join_by(skill)) |>
  mutate(
    n_offers = coalesce(n_offers, 0L),
    denominator = nrow(jobs),
    share = n_offers / denominator
  ) |>
  arrange(desc(n_offers), skill) |>
  mutate(rank = row_number()) |>
  select(rank, skill, n_offers, denominator, share)

top_skills <- skill_summary |>
  slice_head(n = top_n)

if (nrow(top_skills) != top_n) {
  stop("No hay suficientes competencias para construir la figura.")
}

metadata <- tibble(
  period = "Cortes de junio y julio de 2026",
  june_snapshot_at = june_snapshot_at,
  july_snapshot_at = july_snapshot_at,
  unique_jobs = expected_unique_jobs,
  universe_jobs = expected_universe_jobs,
  description_jobs = expected_description_jobs,
  denominator = expected_description_jobs,
  extraction = "Diccionario de expresiones explícitas en skills y description",
  note = "Las ofertas pueden mencionar más de una competencia."
)

result <- list(
  skills = skill_summary,
  top_skills = top_skills,
  metadata = metadata
)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
saveRDS(result, out_rds_path)
write_csv(skill_summary, out_csv_path)
message("Guardado: ", out_rds_path)
message("Guardado: ", out_csv_path)
