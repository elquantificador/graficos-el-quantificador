# ============================================================
# clean_jobs_habilidades_mensuales.R
# Consolida dos cortes de ofertas y calcula las herramientas técnicas más demandadas.
# Requiere: data/raw/jobs_scrape/jobs_20260617_043911.csv
#           data/raw/jobs_scrape/jobs_20260716_013707.csv
# Guarda:   data/processed/jobs_habilidades_mensuales.rds
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_jobs_habilidades_mensuales.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("dplyr", "readr", "stringr", "tidyr"))

june_path <- "data/raw/jobs_scrape/jobs_20260617_043911.csv"
july_path <- "data/raw/jobs_scrape/jobs_20260716_013707.csv"
out_path <- "data/processed/jobs_habilidades_mensuales.rds"

expected_denominator <- 377L
expected_source_counts <- tibble(
  source = c("computrabajo", "mipleo", "multitrabajos"),
  n = c(26L, 43L, 308L)
)

canonical_skill_labels <- c(
  "sql" = "SQL", "python" = "Python", "r" = "R", "excel" = "Excel",
  "power bi" = "Power BI", "tableau" = "Tableau", "looker" = "Looker",
  "qlik" = "Qlik", "power query" = "Power Query", "dax" = "DAX",
  "power pivot" = "Power Pivot", "power platform" = "Power Platform",
  "etl" = "ETL", "data warehouse" = "Data Warehouse", "data lake" = "Data Lake",
  "bigquery" = "BigQuery", "google bigquery" = "BigQuery",
  "snowflake" = "Snowflake", "redshift" = "Redshift",
  "postgresql" = "PostgreSQL", "postgres" = "PostgreSQL",
  "mysql" = "MySQL", "sql server" = "SQL Server",
  "microsoft sql server" = "SQL Server", "ms sql server" = "SQL Server",
  "oracle" = "Oracle", "oracle database" = "Oracle",
  "mongodb" = "MongoDB", "sas" = "SAS", "spss" = "SPSS",
  "stata" = "Stata", "matlab" = "Matlab", "api" = "APIs", "apis" = "APIs",
  "git" = "Git", "aws" = "AWS", "azure" = "Azure",
  "gcp" = "Google Cloud", "google cloud" = "Google Cloud",
  "google cloud platform" = "Google Cloud", "sap" = "SAP",
  "sap hana" = "SAP", "sap s/4hana" = "SAP",
  "google sheets" = "Google Sheets", "spark" = "Spark",
  "pyspark" = "Spark", "apache spark" = "Spark", "databricks" = "Databricks",
  "airflow" = "Airflow", "dbt" = "Dbt", "docker" = "Docker",
  "hadoop" = "Hadoop", "linux" = "Linux", "terraform" = "Terraform",
  "kubernetes" = "Kubernetes", "tensorflow" = "TensorFlow",
  "pytorch" = "PyTorch", "keras" = "Keras", "scikit-learn" = "Scikit-learn",
  "scikit learn" = "Scikit-learn", "sklearn" = "Scikit-learn",
  "xgboost" = "XGBoost", "lightgbm" = "LightGBM", "pandas" = "Pandas",
  "numpy" = "NumPy", "javascript" = "JavaScript", "scala" = "Scala",
  "kpis" = "KPIs", "bi" = "BI", "erp" = "ERP", "crm" = "CRM",
  "estadistica" = "Estadística", "econometria" = "Econometría",
  "analisis de datos" = "Análisis de datos",
  "visualizacion de datos" = "Visualización de datos",
  "bases de datos" = "Bases de datos", "dashboards" = "Dashboards",
  "business intelligence" = "Business Intelligence", "rstudio" = "R",
  "r studio" = "R", "rlanguage" = "R", "r language" = "R"
)

technical_tools <- c(
  "SQL", "Python", "R", "Excel", "Power BI", "Tableau", "Looker", "Qlik",
  "Power Query", "DAX", "Power Pivot", "Power Platform", "ETL",
  "Data Warehouse", "Data Lake", "BigQuery", "Snowflake", "Redshift",
  "PostgreSQL", "MySQL", "SQL Server", "Oracle", "MongoDB", "SAS", "SPSS",
  "Stata", "Matlab", "APIs", "Git", "AWS", "Azure", "Google Cloud", "SAP",
  "Google Sheets", "Spark", "Databricks", "Airflow", "Dbt", "Docker",
  "Hadoop", "Linux", "Terraform", "Kubernetes", "TensorFlow", "PyTorch",
  "Keras", "Scikit-learn", "XGBoost", "LightGBM", "Pandas", "NumPy",
  "JavaScript", "Scala"
)

expected_top_skills <- tibble(
  skill = c("Excel", "Power BI", "SQL", "SAP", "Python"),
  n = c(262L, 92L, 53L, 41L, 24L)
)

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

jobs_latest <- bind_rows(
  june_raw |>
    mutate(snapshot_file = basename(june_path), snapshot_at = june_snapshot_at),
  july_raw |>
    mutate(snapshot_file = basename(july_path), snapshot_at = july_snapshot_at)
) |>
  arrange(source, job_id, desc(snapshot_at)) |>
  distinct(source, job_id, .keep_all = TRUE) |>
  filter(
    str_to_lower(str_squish(in_universe)) == "true",
    !is.na(skills),
    str_squish(skills) != ""
  )

source_counts <- jobs_latest |>
  count(source, name = "n") |>
  arrange(source)

if (nrow(jobs_latest) != expected_denominator) {
  stop(
    "La base elegible cambió: se esperaban ", expected_denominator,
    " ofertas con habilidades declaradas y se obtuvieron ", nrow(jobs_latest), "."
  )
}
if (!identical(source_counts, expected_source_counts)) {
  observed_counts <- source_counts |>
    transmute(label = str_c(source, "=", n)) |>
    pull(label) |>
    str_c(collapse = ", ")
  stop(
    "La composición por fuente cambió. Se esperaba computrabajo=26, mipleo=43, ",
    "multitrabajos=308; se obtuvo ", observed_counts, "."
  )
}

skills_by_job <- jobs_latest |>
  select(source, job_id, skills) |>
  mutate(skill = str_split(skills, ";\\s*")) |>
  unnest(skill) |>
  mutate(
    skill_key = str_to_lower(str_squish(skill)),
    skill = recode(
      skill_key,
      !!!canonical_skill_labels,
      .default = str_to_sentence(skill_key)
    )
  ) |>
  filter(skill %in% technical_tools) |>
  distinct(source, job_id, skill)

top_skills <- skills_by_job |>
  count(skill, name = "n") |>
  arrange(desc(n), skill) |>
  slice_head(n = 5) |>
  mutate(
    denominator = expected_denominator,
    share = n / denominator,
    rank = row_number()
  ) |>
  select(rank, skill, n, denominator, share)

observed_top_skills <- top_skills |>
  select(skill, n)
if (!identical(observed_top_skills, expected_top_skills)) {
  stop(
    "El top 10 de herramientas técnicas cambió tras la normalización; revise el diccionario, ",
    "los datos y el título editorial antes de publicar."
  )
}

metadata <- tibble(
  period = "Cortes disponibles consolidados",
  june_snapshot_at = june_snapshot_at,
  july_snapshot_at = july_snapshot_at,
  deduplication_key = "source + job_id; se conserva el corte más reciente",
  universe = "Roles de datos, adyacentes a datos y TI clasificados por el scraper",
  denominator = expected_denominator
)

result <- list(
  skills = top_skills,
  source_counts = source_counts,
  metadata = metadata
)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(result, out_path)
message("Guardado: ", out_path)
