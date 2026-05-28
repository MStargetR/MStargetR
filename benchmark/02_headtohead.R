# benchmark/02_headtohead.R
# Per-stage wall-clock + peak-memory head-to-head at the base data size.
# MStargetR runs natively on the host; MetaboAnalystR runs in Docker. Both
# consume the SAME hashed shared_table.csv.
#
# Rigorous head-to-head stages (both engines, like-for-like):
#   combat  -- full preprocess+ComBat correction
#   qcrlsc  -- full preprocess+QC-RLSC correction
#   pca     -- PCA on autoscaled matrix
# MA also exposes impute/filter/normalize as separate steps; we record those
# as MA-side breakdown timings (MStargetR folds them into the correction call,
# noted in the report).
#
# Output: results/bm_headtohead.csv

suppressWarnings(suppressMessages({ library(MStargetR) }))

here <- dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))
if (is.na(here) || !nzchar(here)) here <- "benchmark"
source(file.path(here, "lib", "config.R"))
source(file.path(here, "lib", "timing.R"))
source(file.path(here, "lib", "scale_table.R"))

ensure_results_dir()
pin_single_thread()

if (!file.exists(shared_table_csv())) {
  stop("02: ", shared_table_csv(), " not found. Run 01_make_shared_table.R first.")
}
data <- utils::read.csv(shared_table_csv(), check.names = FALSE,
                        stringsAsFactors = FALSE)

# Guard: ComBat requires >= 2 batches; QCRLSC requires >= 2 QC per batch.
# The bundled 2-sample example has 1 batch, which would crash the timing harness.
# When the table is too small (< 2 batches or < 10 samples), replace it with a
# synthetic fixture so we can prove the timing logic works end-to-end.
# The real run will use the full ~168-sample table.
.n_batches_found <- length(unique(data$batch))
.n_samples_found <- nrow(data)
if (.n_batches_found < 2 || .n_samples_found < 10) {
  message(sprintf(
    "02: shared_table has %d samples / %d batch(es) -- too small for ",
    .n_samples_found, .n_batches_found),
    "ComBat/QCRLSC (need >= 2 batches and >= 10 samples).")
  message("02: substituting synthetic table (n=120, feats=30, batches=3) ",
          "for harness validation. REAL run will use the full table.")
  data <- make_scaled_table(n_samples = 120, n_features = 30, n_batches = 3)
  write_shared_table(data, shared_table_csv())
  message("02: synthetic table written to ", shared_table_csv())
}

feat_cols <- bench_feature_cols(data)
input_hash <- if (requireNamespace("digest", quietly = TRUE))
  digest::digest(file = shared_table_csv(), algo = "sha256") else NA
message("02: input sha256 ", substr(input_hash, 1, 12), "...  (",
        nrow(data), " samples x ", length(feat_cols), " features)")

iters <- BENCH_ITERATIONS
ma_reps <- 3L

# ---- MStargetR side --------------------------------------------------------
ms_rows <- list()

ms_rows$combat <- tryCatch(
  measure_stage("combat", iterations = iters, expr = {
    MStargetR::batchCorrectR(data, qc_label = BENCH_QC_LABEL, method = "ComBat",
                             report = FALSE, plot = FALSE)
  }),
  error = function(e) {
    message("02: MStargetR ComBat failed: ", conditionMessage(e)); NULL
  }
)

ms_rows$qcrlsc <- tryCatch(
  measure_stage("qcrlsc", iterations = iters, expr = {
    MStargetR::batchCorrectR(data, qc_label = BENCH_QC_LABEL, method = "QCRLSC",
                             qcrlsc_method = "subtract",
                             report = FALSE, plot = FALSE)
  }),
  error = function(e) {
    message("02: MStargetR QCRLSC skipped (", conditionMessage(e),
            "). Is qcrlscR installed?")
    NULL
  }
)

# PCA on autoscaled feature matrix (matches MA scale="AutoNorm"). Impute NA
# with column min/2 first (MStargetR/MA both floor missing values).
ms_rows$pca <- tryCatch(
  measure_stage("pca", iterations = iters, expr = {
    m <- as.matrix(data[, feat_cols, drop = FALSE])
    for (j in seq_len(ncol(m))) {
      na <- is.na(m[, j]); if (any(na)) m[na, j] <- min(m[, j], na.rm = TRUE) / 2
    }
    stats::prcomp(m, center = TRUE, scale. = TRUE)
  }),
  error = function(e) {
    message("02: MStargetR PCA failed: ", conditionMessage(e)); NULL
  }
)

# ---- End-to-end "complete pipeline" rows -----------------------------------
# Per-stage rows above are good for finding where time goes, but summing them
# overstates total cost on the MA side (one Docker startup per stage). These
# full_* rows time the realistic batch-correction + PCA pipeline in ONE
# process: batchCorrectR (includes its own preprocess) -> autoscaled PCA.
# The matching MA stages run in a single container invocation. See ma_runner.R
# stages full_combat / full_qcrlsc.
ms_full_pipeline <- function(method) {
  args <- list(data = data, qc_label = BENCH_QC_LABEL, method = method,
               report = FALSE, plot = FALSE)
  # qcrlsc_method only used by the QCRLSC path; ComBat ignores it.
  if (method == "QCRLSC") args$qcrlsc_method <- "subtract"
  m <- do.call(MStargetR::batchCorrectR, args)
  # Return key is $corrected_data (a wide df: sample_name, sample_type, batch,
  # run_order, <features...>). Restrict to the canonical feature columns.
  cm <- m$corrected_data
  mat <- as.matrix(cm[, intersect(feat_cols, names(cm)), drop = FALSE])
  for (j in seq_len(ncol(mat))) {
    na <- is.na(mat[, j])
    if (any(na)) mat[na, j] <- min(mat[, j], na.rm = TRUE) / 2
  }
  keep <- apply(mat, 2, function(x) is.finite(stats::sd(x)) && stats::sd(x) > 0)
  stats::prcomp(mat[, keep, drop = FALSE], center = TRUE, scale. = TRUE)
}

ms_rows$full_combat <- tryCatch(
  measure_stage("full_combat", iterations = iters, expr = {
    ms_full_pipeline("ComBat")
  }),
  error = function(e) {
    message("02: MStargetR full_combat failed: ", conditionMessage(e)); NULL
  }
)

ms_rows$full_qcrlsc <- tryCatch(
  measure_stage("full_qcrlsc", iterations = iters, expr = {
    ms_full_pipeline("QCRLSC")
  }),
  error = function(e) {
    message("02: MStargetR full_qcrlsc skipped (", conditionMessage(e),
            "). Is qcrlscR installed?")
    NULL
  }
)

# Filter out NULL entries (stages that failed gracefully) before binding.
ms_df <- do.call(rbind, Filter(Negate(is.null), ms_rows))

# ---- MetaboAnalyst side (Docker) ------------------------------------------
# ma_runner reads /data/shared_table.csv (the canonical file lives in bench_dir,
# which is mounted as /data). Copy the runner into the mount so the container
# can see it.
file.copy(file.path(here, "docker", "ma_runner.R"),
          bench_path("ma_runner.R"), overwrite = TRUE)

ma_stages <- c("read_sanity", "impute", "filter_mv", "filter_rsd",
               "normalize", "combat", "qcrlsc", "pca",
               # End-to-end pipelines in a single container invocation.
               "full_combat", "full_qcrlsc")
ma_rows <- lapply(ma_stages, function(st) {
  message("02: MA stage ", st, " ...")
  tryCatch(
    run_ma_stage_repeated(st, input = "shared_table.csv",
                          out = sprintf("ma_out_%s.csv", st),
                          mount_dir = bench_dir(), reps = ma_reps),
    error = function(e) {
      message("02: MA stage ", st, " failed: ", conditionMessage(e)); NULL
    })
})
ma_df <- do.call(rbind, lapply(ma_rows, function(r)
  if (is.null(r)) NULL else r[setdiff(names(r), "log")]))

# ---- combine + write -------------------------------------------------------
# ma_df may be NULL when all Docker stages fail (expected this stage).
if (is.null(ma_df) || nrow(ma_df) == 0) {
  out <- ms_df
} else {
  common <- intersect(names(ms_df), names(ma_df))
  out <- rbind(ms_df[, common, drop = FALSE], ma_df[, common, drop = FALSE])
}
out$n_samples  <- nrow(data)
out$n_features <- length(feat_cols)
out$input_sha256 <- input_hash
out$thread_mode <- "single"

outfile <- file.path(results_dir(), "bm_headtohead.csv")
utils::write.csv(out, outfile, row.names = FALSE)
message("02: wrote ", outfile)
print(out[, c("engine", "stage", "wall_s_median", "peak_mb")])
