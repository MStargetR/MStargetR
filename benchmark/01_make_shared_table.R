# benchmark/01_make_shared_table.R
# Produce the canonical shared peak table that BOTH engines consume in the
# head-to-head. MStargetR runs the vendor-specific front end (msConvertR ->
# PeakForgeR) on the real .wiff files; the resulting peak-area report is
# pivoted to the canonical wide schema and hashed.
#
# Toggles (env vars):
#   MSTARGETR_RUN_PIPELINE=1  run msConvertR + PeakForgeR (needs Docker+Skyline)
#   MSTARGETR_ALLOW_EXAMPLE=1 fall back to the bundled Example_PeakForgeR_report
#                             when no real report is found (smoke testing only)
#
# Output (into the OneDrive benchmark folder):
#   shared_table.csv      canonical schema (MStargetR-native)
#   shared_table_MA.csv   MetaboAnalyst rowu upload format (reference copy)
#   results/shared_table_hash.txt

suppressWarnings(suppressMessages({
  library(MStargetR)
  library(dplyr)
  library(tidyr)
}))

here <- dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))
if (is.na(here) || !nzchar(here)) here <- "benchmark"
source(file.path(here, "lib", "config.R"))
source(file.path(here, "lib", "scale_table.R"))

ensure_results_dir()

# ---- 1. obtain a PeakForgeR-style peak report -----------------------------
run_pipeline   <- nzchar(Sys.getenv("MSTARGETR_RUN_PIPELINE"))
allow_example  <- nzchar(Sys.getenv("MSTARGETR_ALLOW_EXAMPLE"))
project_dir    <- bench_dir()
template       <- system.file("extdata", "LGW_lipid_mrm_template_v1.tsv",
                              package = "MStargetR")

find_report <- function(root) {
  # Primary: PeakForgeR writes CSVs named *PeakForgeR*.csv inside
  # <plate>/data/PeakForgeR/ subdirectories.  Match any CSV whose full path
  # contains a "PeakForgeR" directory component AND whose filename contains
  # "PeakForgeR".  Filter out near-empty placeholder files (< 1 kB).
  hits <- list.files(root, pattern = "PeakForgeR.*\\.csv$",
                     recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  hits <- hits[grepl("[/\\\\]PeakForgeR[/\\\\]", hits, ignore.case = TRUE)]
  hits <- hits[file.info(hits)$size > 1000]
  if (length(hits) > 0) return(hits)

  # Legacy fallback: old naming that included "report" in the filename.
  hits <- list.files(root,
                     pattern = "(PeakForgeR.*report|report.*PeakForgeR).*\\.csv$",
                     recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  hits <- hits[file.info(hits)$size > 1000]
  hits
}

if (run_pipeline) {
  message("01: running msConvertR + PeakForgeR on real .wiff files...")

  # Check whether mzML files already exist for every wiff-derived plate.
  # msConvertR's restructure step tries to copy the raw wiff files into each
  # plate's raw_data/ subdir; that file.copy fails on the second run when the
  # copies are already there. Skip msConvertR entirely when every expected plate
  # directory already contains at least one .mzML file.
  wiff_files <- list.files(file.path(project_dir, "raw_data"),
                           pattern = "\\.wiff$", ignore.case = TRUE,
                           full.names = FALSE)
  plate_names <- sub("\\.wiff$", "", wiff_files, ignore.case = TRUE)
  plates_have_mzml <- vapply(plate_names, function(p) {
    d <- file.path(project_dir, p, "data", "mzml")
    dir.exists(d) && length(list.files(d, pattern = "\\.mzML$", ignore.case = TRUE)) > 0
  }, logical(1))

  if (length(plate_names) > 0 && all(plates_have_mzml)) {
    message("01: mzML already present for all plates -- skipping msConvertR.")
  } else {
    msConvertR(input_directory = project_dir, output_directory = project_dir)
  }

  # PeakForgeR throws if SIL internal standards are absent or if it encounters
  # non-plate subdirectories (e.g. results/).  The Skyline CSV reports are
  # written before these post-checks fire, so catch and warn rather than stop.
  tryCatch(
    PeakForgeR(user_name = "benchmark",
               project_directory = project_dir,
               mrm_template_list = list(v1 = template),  # must be named
               QC_sample_label = "LTR"),
    error = function(e) {
      message("01: PeakForgeR threw an error (continuing -- Skyline CSVs ",
              "should already be present):\n    ", conditionMessage(e))
    }
  )
}

report_path <- find_report(project_dir)
if (length(report_path) == 0) {
  if (allow_example) {
    message("01: no real report found; falling back to bundled example ",
            "(SMOKE TEST ONLY -- results are not benchmark-valid).")
    report_path <- system.file("extdata", "Example_PeakForgeR_report.csv",
                               package = "MStargetR")
  } else {
    stop("01: no PeakForgeR report found under ", project_dir,
         ". Set MSTARGETR_RUN_PIPELINE=1 to generate it, or ",
         "MSTARGETR_ALLOW_EXAMPLE=1 to smoke-test with the bundled example.")
  }
}
message("01: using peak report(s): ", paste(report_path, collapse = ", "))

report <- do.call(rbind, lapply(report_path, function(p)
  utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)))

# ---- 2. pivot long report -> canonical wide table -------------------------
# Report columns: FileName, MoleculeName, Area, AcquiredTime (+ others).
stopifnot(all(c("FileName", "MoleculeName", "Area") %in% names(report)))

# Acquisition order from AcquiredTime when present, else file appearance order.
order_lookup <- report |>
  dplyr::distinct(FileName, AcquiredTime = if ("AcquiredTime" %in% names(report))
    AcquiredTime else NA_character_) |>
  dplyr::mutate(ts = suppressWarnings(as.POSIXct(AcquiredTime,
                  tryFormats = c("%m/%d/%Y %H:%M:%S", "%Y-%m-%dT%H:%M:%SZ",
                                 "%Y-%m-%d %H:%M:%S"), tz = "UTC"))) |>
  dplyr::arrange(ts, FileName) |>
  dplyr::mutate(run_order = dplyr::row_number())

wide <- report |>
  dplyr::group_by(FileName, MoleculeName) |>
  dplyr::summarise(Area = max(Area, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = MoleculeName, values_from = Area)

# sample_type: LTR / QC tokens in the filename -> "qc", else "sample".
classify <- function(fn) {
  ifelse(grepl("LTR|_QC|PQC|^QC", fn, ignore.case = TRUE), "qc", "sample")
}
# batch: parse plate token (HPPp###) from filename, else single batch.
parse_batch <- function(fn) {
  m <- regmatches(fn, regexpr("HPPp[0-9]+", fn, ignore.case = TRUE))
  ifelse(lengths(regmatches(fn, gregexpr("HPPp[0-9]+", fn, ignore.case = TRUE))) > 0,
         m, "plate1")
}

shared <- wide |>
  dplyr::rename(sample_name = FileName) |>
  dplyr::left_join(dplyr::select(order_lookup, FileName, run_order),
                   by = c("sample_name" = "FileName")) |>
  dplyr::mutate(sample_type = classify(sample_name),
                batch = parse_batch(sample_name)) |>
  dplyr::arrange(run_order)

# Reorder to canonical column order; coerce feature NAs -> NA (kept for impute).
feat_cols <- setdiff(names(shared), c("sample_name", "sample_type", "batch", "run_order"))
shared <- shared[, c("sample_name", "sample_type", "batch", "run_order", feat_cols)]
# Replace non-finite (from max of all-NA) with NA.
for (f in feat_cols) shared[[f]][!is.finite(shared[[f]])] <- NA_real_

message(sprintf("01: shared table = %d samples x %d features (%d QC, %d batches)",
                nrow(shared), length(feat_cols),
                sum(shared$sample_type == "qc"),
                dplyr::n_distinct(shared$batch)))

# ---- 3. write canonical + MA-format copies, hash both ---------------------
hash_canon <- write_shared_table(shared, shared_table_csv())

# MetaboAnalyst rowu reference copy (Sample, Label, features...).
ma_ref <- data.frame(Sample = shared$sample_name, Label = shared$sample_type,
                     check.names = FALSE)
ma_ref <- cbind(ma_ref, shared[, feat_cols, drop = FALSE])
utils::write.csv(ma_ref, shared_table_ma_csv(), row.names = FALSE)
hash_ma <- if (requireNamespace("digest", quietly = TRUE))
  digest::digest(file = shared_table_ma_csv(), algo = "sha256") else NA

writeLines(
  c(sprintf("shared_table.csv sha256: %s", hash_canon),
    sprintf("shared_table_MA.csv sha256: %s", hash_ma),
    sprintf("generated: %s", as.character(Sys.time())),
    sprintf("source_report: %s", paste(report_path, collapse = "; ")),
    sprintf("dims: %d samples x %d features", nrow(shared), length(feat_cols))),
  file.path(results_dir(), "shared_table_hash.txt")
)
message("01: wrote ", shared_table_csv(), " (sha256 ", substr(hash_canon, 1, 12), "...)")
