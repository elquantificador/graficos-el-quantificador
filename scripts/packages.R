# ============================================================
# packages.R
# Helper para instalar (si hace falta) y cargar paquetes de R.
# ============================================================

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
