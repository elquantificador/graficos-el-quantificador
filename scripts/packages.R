# ============================================================
# packages.R
# Helper para instalar (si hace falta) y cargar paquetes de R.
# ============================================================

# Usa una locale UTF-8 para que los acentos se conserven al renderizar PNG.
utf8_locales <- c(
  "C.UTF-8",
  "en_US.UTF-8",
  "English_United States.utf8",
  "Spanish_Ecuador.utf8"
)

if (!grepl("UTF-8|utf8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) {
  for (locale_name in utf8_locales) {
    locale_result <- tryCatch(
      suppressWarnings(Sys.setlocale("LC_CTYPE", locale_name)),
      error = function(error) NULL
    )
    if (!is.null(locale_result) &&
        grepl("UTF-8|utf8", locale_result, ignore.case = TRUE)) {
      break
    }
  }
}

if (!grepl("UTF-8|utf8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) {
  warning(
    "No se pudo activar una locale UTF-8; los acentos pueden renderizarse mal."
  )
}

ensure_packages <- function(pkgs, repos = "https://cloud.r-project.org") {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = repos)
    }
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }
}
