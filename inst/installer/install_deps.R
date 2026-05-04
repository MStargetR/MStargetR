# MStargetR R Package Dependency Installer
# =============================================================================
# Run via: Rscript install_deps.R [--source-dir <path>]
#
# This script installs MStargetR and all required dependencies including
# Bioconductor packages, CRAN packages, and GUI dependencies.
#
# --source-dir: path to a bundled MStargetR source tree (used by the
#               Windows installer so that MStargetR itself can be installed
#               without internet access).

# ---------------------------------------------------------------------------
# Parse command-line arguments
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
source_dir <- NULL
sd_idx <- match("--source-dir", args)
if (!is.na(sd_idx) && length(args) >= sd_idx + 1) {
  source_dir <- args[sd_idx + 1]
}

cat("=== MStargetR Package Installer ===\n")
cat(paste("R version:", R.version.string, "\n"))
cat(paste("Library:", .libPaths()[1], "\n"))
cat(paste("Time:", Sys.time(), "\n"))
if (!is.null(source_dir)) cat(paste("Source dir:", source_dir, "\n"))
cat("\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Probe the CRAN mirror once so an unreachable network / firewall produces a
# single clear error instead of a flurry of per-package install failures.
check_cran_reachable <- function(url = "https://cloud.r-project.org",
                                 timeout = 10) {
  ok <- tryCatch({
    old <- options(timeout = timeout); on.exit(options(old), add = TRUE)
    h <- suppressWarnings(utils::download.file(
      url, tempfile(), quiet = TRUE, mode = "wb", method = "auto"
    ))
    identical(h, 0L)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) {
    stop("Cannot reach CRAN mirror ", url,
         ". Check network / proxy / firewall before retrying install_deps.R.",
         call. = FALSE)
  }
  invisible(TRUE)
}
check_cran_reachable()

# ---------------------------------------------------------------------------
# Helper: install if not present
# ---------------------------------------------------------------------------
install_if_missing <- function(pkg, source = "CRAN") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("  Installing ", pkg, " (", source, ")...\n"))
    tryCatch({
      if (source == "Bioconductor") {
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
          install.packages("BiocManager")
          # Stop early rather than calling BiocManager::install() below on an
          # uninstalled namespace; users will get a cryptic "there is no
          # package called 'BiocManager'" otherwise.
          if (!requireNamespace("BiocManager", quietly = TRUE)) {
            stop("BiocManager install appeared to succeed but the namespace ",
                 "is not loadable. Aborting before attempting Bioconductor ",
                 "package install.")
          }
        }
        BiocManager::install(pkg, ask = FALSE, update = FALSE)
      } else {
        install.packages(pkg)
      }
      if (requireNamespace(pkg, quietly = TRUE)) {
        cat(paste0("    OK: ", pkg, " ",
                    as.character(packageVersion(pkg)), "\n"))
      } else {
        cat(paste0("    WARNING: ", pkg, " install reported success but package not loadable\n"))
      }
    }, error = function(e) {
      cat(paste0("    ERROR: ", pkg, " - ", e$message, "\n"))
    })
  } else {
    cat(paste0("  ", pkg, " ", as.character(packageVersion(pkg)),
               " already installed\n"))
  }
}

# ---------------------------------------------------------------------------
# Step 1: Bioconductor packages
# ---------------------------------------------------------------------------
cat("\n[1/4] Bioconductor packages\n")

install_if_missing("BiocManager", "CRAN")

bioc_pkgs <- c("mzR", "ropls", "statTarget", "sva")
for (pkg in bioc_pkgs) {
  install_if_missing(pkg, "Bioconductor")
}

# ---------------------------------------------------------------------------
# Step 2: Core CRAN dependencies (from DESCRIPTION Imports)
# ---------------------------------------------------------------------------
cat("\n[2/4] Core CRAN dependencies\n")

core_pkgs <- c(
  "callr", "data.table", "dplyr", "future", "future.apply",
  "ggplot2", "janitor", "magrittr", "openxlsx", "plotly",
  "purrr", "readr", "stringr",
  "tibble", "tidyr", "tidyselect", "viridis"
)

for (pkg in core_pkgs) {
  install_if_missing(pkg, "CRAN")
}

# ---------------------------------------------------------------------------
# Step 3: GUI dependencies (from DESCRIPTION Suggests)
# ---------------------------------------------------------------------------
cat("\n[3/4] GUI dependencies\n")

gui_pkgs <- c("shiny", "bslib", "DT", "shinyWidgets", "htmltools", "rappdirs", "rmarkdown")
for (pkg in gui_pkgs) {
  install_if_missing(pkg, "CRAN")
}

# ---------------------------------------------------------------------------
# Step 4: Install MStargetR
# ---------------------------------------------------------------------------
cat("\n[4/4] MStargetR package\n")

install_if_missing("remotes", "CRAN")

# Check if MStargetR is already installed
if (requireNamespace("MStargetR", quietly = TRUE)) {
  cat(paste0("  MStargetR already installed\n"))
  cat("  Reinstalling to ensure latest version...\n")
}

# Build a list of candidate source directories (bundled source first)
local_candidates <- character(0)

# 1. Bundled source dir passed via --source-dir (from the Windows installer)
if (!is.null(source_dir) && file.exists(file.path(source_dir, "DESCRIPTION"))) {
  local_candidates <- c(local_candidates, normalizePath(source_dir, winslash = "/"))
}

# 2. Source tree relative to this script (development checkout)
script_file <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", a, value = TRUE)
  if (length(file_arg) > 0) normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")
  else NULL
}, error = function(e) NULL)

if (!is.null(script_file)) {
  dev_path <- normalizePath(file.path(dirname(script_file), "..", ".."),
                            winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(dev_path, "DESCRIPTION"))) {
    local_candidates <- c(local_candidates, dev_path)
  }
}

installed <- FALSE

# Try local/bundled source first (works offline)
for (lp in local_candidates) {
  cat(paste0("  Trying local install from: ", lp, "\n"))
  tryCatch({
    remotes::install_local(lp, upgrade = "never", force = TRUE, quiet = FALSE)
    cat("  MStargetR installed from local source\n")
    installed <- TRUE
    break
  }, error = function(e) {
    cat(paste0("  Local install failed: ", e$message, "\n"))
  })
}

# Fall back to GitHub if local install didn't work
if (!installed) {
  cat("  Trying GitHub install...\n")
  tryCatch({
    remotes::install_github("MStargetR/MStargetR",
                            upgrade = "never",
                            force = TRUE,
                            quiet = FALSE)
    cat("  MStargetR installed from GitHub\n")
    installed <- TRUE
  }, error = function(e) {
    cat(paste0("  GitHub install failed: ", e$message, "\n"))
  })
}

if (!installed) {
  cat("  ERROR: Could not install MStargetR.\n")
  cat("  Please install manually in R:\n")
  cat("    remotes::install_github(\"MStargetR/MStargetR\")\n")
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat("\n=== Installation Summary ===\n")

all_pkgs <- c(bioc_pkgs, core_pkgs, gui_pkgs, "MStargetR")
installed <- vapply(all_pkgs, function(p) {
  requireNamespace(p, quietly = TRUE)
}, logical(1))

cat(paste0("Installed: ", sum(installed), "/", length(all_pkgs), "\n"))

if (any(!installed)) {
  cat("Missing packages:\n")
  for (pkg in all_pkgs[!installed]) {
    cat(paste0("  - ", pkg, "\n"))
  }
} else {
  cat("All packages installed successfully!\n")
}

cat("\n=== Done ===\n")
