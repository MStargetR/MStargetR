# MStargetR Launcher Script
# =============================================================================
# Called by launch.bat / launch_silent.vbs to start the Shiny GUI.
# Separating R logic from the batch file avoids Windows cmd.exe quoting issues.
#
# Usage: Rscript launch_app.R [--script-dir <path>] [--debug]

# ---------------------------------------------------------------------------
# Parse command-line arguments
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
debug_mode <- "--debug" %in% args

script_dir <- NULL
sd_idx <- match("--script-dir", args)
if (!is.na(sd_idx) && length(args) >= sd_idx + 1) {
  script_dir <- args[sd_idx + 1]
}

log_msg <- function(...) {
  cat(paste0(..., "\n"))
}

# ---------------------------------------------------------------------------
# Try 1: Install/update from bundled source if needed, then launch
# ---------------------------------------------------------------------------
bundled_src <- NULL
if (!is.null(script_dir)) {
  candidate <- file.path(
    normalizePath(script_dir, winslash = "/", mustWork = FALSE),
    "source"
  )
  if (file.exists(file.path(candidate, "DESCRIPTION"))) {
    bundled_src <- candidate
  }
}

if (!is.null(bundled_src)) {
  # Compare bundled vs installed version — reinstall only when they differ
  bundled_ver <- tryCatch({
    desc <- read.dcf(file.path(bundled_src, "DESCRIPTION"), fields = "Version")
    package_version(desc[1, "Version"])
  }, error = function(e) NULL)

  installed_ver <- tryCatch(
    packageVersion("MStargetR"),
    error = function(e) NULL
  )

  needs_install <- is.null(installed_ver) || is.null(bundled_ver) ||
    bundled_ver != installed_ver

  if (needs_install && requireNamespace("remotes", quietly = TRUE)) {
    log_msg("Updating MStargetR from bundled source...")
    tryCatch({
      remotes::install_local(bundled_src, upgrade = "never", force = TRUE, quiet = FALSE)
      log_msg("Install complete.")
    }, error = function(e) {
      log_msg("Install failed: ", e$message, ". Trying existing version...")
    })
  }
}

# Upload size limit is set dynamically by launchMStargetR() based on host binding

# Ensure Bioconductor suggested packages are installed (may have failed during setup)
for (bioc_pkg in c("sva")) {
  if (!requireNamespace(bioc_pkg, quietly = TRUE)) {
    log_msg("Installing missing Bioconductor package: ", bioc_pkg, "...")
    tryCatch({
      if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
      BiocManager::install(bioc_pkg, ask = FALSE, update = FALSE)
    }, error = function(e) {
      log_msg("WARNING: Could not install ", bioc_pkg, ": ", e$message)
    })
  }
}

if (requireNamespace("MStargetR", quietly = TRUE)) {
  appDir <- system.file("shiny", "MStargetR_app", package = "MStargetR")

  if (nzchar(appDir) && dir.exists(appDir)) {
    log_msg("Launching MStargetR (installed package)...")
    if (debug_mode) {
      log_msg("  App directory: ", appDir)
      log_msg("  R version: ", R.version.string)
      log_msg("  Library paths: ", paste(.libPaths(), collapse = ", "))
    }
    shiny::runApp(appDir, launch.browser = TRUE, host = "127.0.0.1")
    q(save = "no", status = 0)
  } else {
    log_msg("WARNING: MStargetR is installed but the Shiny app directory is missing.")
    log_msg("Reinstall with: remotes::install_github(\"MStargetR/MStargetR\")")
  }
}

# ---------------------------------------------------------------------------
# Try 2: Install from bundled source and launch
# ---------------------------------------------------------------------------
log_msg("MStargetR package not found. Checking for bundled source...")

if (!is.null(script_dir)) {
  script_dir <- normalizePath(script_dir, winslash = "/", mustWork = FALSE)
  candidates <- c(
    file.path(script_dir, "source"),          # bundled by installer
    file.path(script_dir, "..", ".."),         # development checkout
    file.path(script_dir, "..")
  )
} else {
  candidates <- character(0)
}

for (d in candidates) {
  descfile <- file.path(d, "DESCRIPTION")
  if (!file.exists(descfile)) next

  d <- normalizePath(d, winslash = "/")
  log_msg("Found source at: ", d)

  # Try to install the package from local source
  if (requireNamespace("remotes", quietly = TRUE)) {
    log_msg("Installing MStargetR from local source...")
    tryCatch({
      remotes::install_local(d, upgrade = "never", force = TRUE, quiet = FALSE)
    }, error = function(e) {
      log_msg("Install failed: ", e$message)
    })
  }

  # If install succeeded, launch from the installed package
  if (requireNamespace("MStargetR", quietly = TRUE)) {
    appDir <- system.file("shiny", "MStargetR_app", package = "MStargetR")
    if (nzchar(appDir) && dir.exists(appDir)) {
      log_msg("Launching MStargetR (installed from bundled source)...")
      shiny::runApp(appDir, launch.browser = TRUE, host = "127.0.0.1")
      q(save = "no", status = 0)
    }
  }

  # Fallback: launch directly from source via devtools::load_all
  if (requireNamespace("devtools", quietly = TRUE)) {
    log_msg("Trying development mode via devtools::load_all...")
    tryCatch({
      devtools::load_all(d, quiet = TRUE)
      appDir <- file.path(d, "inst", "shiny", "MStargetR_app")
      if (dir.exists(appDir)) {
        log_msg("Launching MStargetR (development mode)...")
        shiny::runApp(appDir, launch.browser = TRUE, host = "127.0.0.1")
        q(save = "no", status = 0)
      }
    }, error = function(e) {
      log_msg("devtools::load_all failed: ", e$message)
    })
  }
}

# ---------------------------------------------------------------------------
# Nothing worked
# ---------------------------------------------------------------------------
log_msg("")
log_msg("ERROR: Could not start MStargetR.")
log_msg("")
log_msg("Troubleshooting:")
log_msg("  1. Run install_deps.R to install all dependencies")
log_msg("  2. Or in R run: remotes::install_github(\"MStargetR/MStargetR\")")
log_msg("")
log_msg("If the problem persists, report it at:")
log_msg("  https://github.com/MStargetR/MStargetR/issues")
q(save = "no", status = 1)
