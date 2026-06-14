# ============================================================
# generate_chart_catalog.R
# Genera outputs/chart_catalog/chart_catalog.csv desde una fuente estructurada.
# Requiere: outputs/chart_catalog/chart_catalog_source.csv
# Guarda:   outputs/chart_catalog/chart_catalog.csv
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/generate_chart_catalog.R
# ============================================================

source("scripts/packages.R")
ensure_packages(c("readr"))

source_path <- "outputs/chart_catalog/chart_catalog_source.csv"
out_path <- "outputs/chart_catalog/chart_catalog.csv"

required_cols <- c(
  "Chart Name",
  "Subtitle",
  "Date",
  "LinkedIn Link",
  "Image Filename",
  "Author",
  "Description",
  "Script Link"
)

catalog_source <- readr::read_csv(source_path, show_col_types = FALSE, na = c("", "NA"))

missing_cols <- setdiff(required_cols, names(catalog_source))
if (length(missing_cols) > 0) {
  stop("Faltan columnas en chart_catalog_source.csv: ", paste(missing_cols, collapse = ", "))
}

catalog <- as.data.frame(catalog_source, check.names = FALSE, stringsAsFactors = FALSE)

blank_required <- vapply(required_cols, function(col) {
  any(is.na(catalog[[col]]) | trimws(catalog[[col]]) == "")
}, logical(1))

if (any(blank_required)) {
  stop(
    "Hay valores vacíos en columnas requeridas: ",
    paste(required_cols[blank_required], collapse = ", ")
  )
}

catalog[["Image Path"]] <- gsub("\\\\", "/", file.path("outputs", "figures", catalog[["Image Filename"]]))
catalog[["Landscape Image Path"]] <- gsub("\\\\", "/", file.path("outputs", "figures", "landscape", catalog[["Image Filename"]]))

missing_images <- catalog[!file.exists(catalog[["Image Path"]]), c("Chart Name", "Image Path"), drop = FALSE]
if (nrow(missing_images) > 0) {
  stop(
    "No se encontraron estas imágenes publicadas en outputs/figures/: ",
    paste(missing_images[["Image Path"]], collapse = ", ")
  )
}

missing_landscape <- catalog[!file.exists(catalog[["Landscape Image Path"]]), c("Chart Name", "Landscape Image Path"), drop = FALSE]
if (nrow(missing_landscape) > 0) {
  stop(
    "No se encontraron estas imágenes apaisadas en outputs/figures/landscape/: ",
    paste(missing_landscape[["Landscape Image Path"]], collapse = ", ")
  )
}

github_prefix <- "https://github.com/elquantificador/graficos-el-quantificador/blob/main/"
script_paths <- sub(github_prefix, "", catalog[["Script Link"]], fixed = TRUE)
missing_scripts <- catalog[!file.exists(script_paths), c("Chart Name", "Script Link"), drop = FALSE]
if (nrow(missing_scripts) > 0) {
  stop(
    "No se encontraron estos scripts referenciados en Script Link: ",
    paste(missing_scripts[["Script Link"]], collapse = ", ")
  )
}

output_cols <- c(
  "Chart Name",
  "Subtitle",
  "Date",
  "LinkedIn Link",
  "Image Filename",
  "Image Path",
  "Landscape Image Path",
  "Author",
  "Description",
  "Script Link"
)

catalog_out <- catalog[output_cols]

dir.create("outputs/chart_catalog", showWarnings = FALSE, recursive = TRUE)
readr::write_csv(catalog_out, out_path, na = "")
message("Guardado: ", out_path)
