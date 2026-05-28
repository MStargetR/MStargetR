# benchmark/lib/config.R
# Single source of truth for benchmark paths and canonical schema.
# Sourced by every driver script. Override BENCH_DIR via the
# MSTARGETR_BENCH_DIR env var if the data folder moves.

# ---- Paths -----------------------------------------------------------------

# Root of the external benchmark data folder (NOT in the repo). Contains
# raw_data/ with the real .wiff files and receives all result artifacts.
bench_dir <- function() {
  env <- Sys.getenv("MSTARGETR_BENCH_DIR", unset = NA_character_)
  if (!is.na(env) && nzchar(env)) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }
  default <- file.path(
    Sys.getenv("USERPROFILE", unset = "~"),
    "OneDrive - Murdoch University", "Desktop", "MStargetR_Benchmark"
  )
  normalizePath(default, winslash = "/", mustWork = FALSE)
}

bench_path    <- function(...) file.path(bench_dir(), ...)
results_dir   <- function() bench_path("results")
raw_data_dir  <- function() bench_path("raw_data")

# Canonical shared-table files (the single input both engines consume).
shared_table_csv    <- function() bench_path("shared_table.csv")      # MStargetR schema
shared_table_ma_csv <- function() bench_path("shared_table_MA.csv")   # MetaboAnalyst transpose

ensure_results_dir <- function() {
  d <- results_dir()
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# ---- Canonical shared-table schema ----------------------------------------
# Column 1: sample_name (character, unique)
# Column 2: sample_type ("qc" | "sample")
# Column 3: batch       (e.g. "plate1")
# Column 4: run_order   (integer, unique, acquisition order)
# Columns 5+: one numeric column per feature (feature name = column name)
# This matches make_bc_data() / batchCorrectR()'s expected schema exactly.
BENCH_META_COLS <- c("sample_name", "sample_type", "batch", "run_order")

# Feature (metabolite) column names = everything that isn't metadata.
bench_feature_cols <- function(df) setdiff(names(df), BENCH_META_COLS)

# ---- Docker image ----------------------------------------------------------
MA_DOCKER_IMAGE <- "mstargetr-bench/metaboanalystr:4.2.0"

# ---- Benchmark parameters --------------------------------------------------
# QC sample-type label used throughout the benchmark (canonical lower-case).
BENCH_QC_LABEL <- "qc"
# Head-to-head correction methods that BOTH engines implement.
BENCH_SHARED_METHODS <- c("ComBat", "QCRLSC")
# Bench iterations for warm timing.
BENCH_ITERATIONS <- 5L
