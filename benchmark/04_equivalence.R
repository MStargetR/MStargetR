# benchmark/04_equivalence.R
# Output-equivalence: do MStargetR and MetaboAnalyst produce numerically
# comparable results for the shared stages? Runs each engine ONCE at base size,
# aligns outputs by (sample, feature), and computes agreement metrics.
#
# Output: results/bm_equivalence.csv + equivalence_plots/*.png

suppressWarnings(suppressMessages({ library(MStargetR); library(ggplot2) }))

here <- dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))
if (is.na(here) || !nzchar(here)) here <- "benchmark"
source(file.path(here, "lib", "config.R"))
source(file.path(here, "lib", "timing.R"))
source(file.path(here, "lib", "scale_table.R"))
source(file.path(here, "lib", "align_outputs.R"))
source(file.path(here, "lib", "equivalence_metrics.R"))

ensure_results_dir()
plot_dir <- file.path(results_dir(), "equivalence_plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
pin_single_thread()

if (!file.exists(shared_table_csv()))
  stop("04: run 01_make_shared_table.R first.")
data <- utils::read.csv(shared_table_csv(), check.names = FALSE,
                        stringsAsFactors = FALSE)

# Guard: equivalence analysis requires >= 2 batches and enough QC per batch.
# The bundled 2-sample example fails these requirements. Fall back to the same
# synthetic fixture used in 02_headtohead.R so both scripts use identical input.
.n_batches_found <- length(unique(data$batch))
.n_samples_found <- nrow(data)
if (.n_batches_found < 2 || .n_samples_found < 10) {
  message(sprintf(
    "04: shared_table has %d samples / %d batch(es) -- substituting ",
    .n_samples_found, .n_batches_found),
    "synthetic table (n=120, feats=30, batches=3) for harness validation.")
  data <- make_scaled_table(n_samples = 120, n_features = 30, n_batches = 3)
  # Do NOT overwrite shared_table.csv here; 02 already did (or will do) that.
}

feat_cols <- bench_feature_cols(data)
qc_mask <- tolower(data$sample_type) == BENCH_QC_LABEL

file.copy(file.path(here, "docker", "ma_runner.R"),
          bench_path("ma_runner.R"), overwrite = TRUE)

# per-feature RSD from QC rows of a wide df (sample_name + features)
rsd_from_qc <- function(df, qc_names) {
  fc <- setdiff(names(df), c("sample_name", BENCH_META_COLS))
  qc_rows <- df$sample_name %in% qc_names
  vapply(fc, function(f) {
    v <- suppressWarnings(as.numeric(df[[f]][qc_rows]))
    m <- mean(v, na.rm = TRUE)
    if (!is.finite(m) || m == 0) return(NA_real_)
    100 * stats::sd(v, na.rm = TRUE) / m
  }, numeric(1))
}
# Normalise QC sample names: batchCorrectR (and the MA corrected CSV after
# read_ma_corrected()) both strip the ".mzML" extension, so strip it here too
# so that rsd_from_qc() can find QC rows by name in the corrected matrices.
qc_names <- sub("\\.mzML$", "", data$sample_name[qc_mask], ignore.case = TRUE)

summaries <- list()

# ---- ComBat & QC-RLSC corrected-matrix equivalence ------------------------
for (mc in list(c(ms = "ComBat", ma = "combat"),
                c(ms = "QCRLSC", ma = "qcrlsc"))) {
  ms_method <- mc[["ms"]]; ma_stage <- mc[["ma"]]
  message("04: equivalence for ", ms_method)

  ms_corr <- tryCatch({
    res <- MStargetR::batchCorrectR(data, qc_label = BENCH_QC_LABEL,
                                    method = ms_method, report = FALSE, plot = FALSE)
    extract_mstargetr_corrected(res)
  }, error = function(e) { message("   MStargetR ", ms_method, ": ",
                                   conditionMessage(e)); NULL })

  ma_out <- bench_path(sprintf("ma_eq_%s.csv", ma_stage))
  ma_ok <- tryCatch({
    run_ma_stage(ma_stage, input = "shared_table.csv",
                 out = sprintf("ma_eq_%s.csv", ma_stage), mount_dir = bench_dir())
    file.exists(ma_out)
  }, error = function(e) { message("   MA ", ma_stage, ": ",
                                   conditionMessage(e)); FALSE })

  if (!is.null(ms_corr) && isTRUE(ma_ok)) {
    ma_corr <- read_ma_corrected(ma_out)
    aligned <- align_matrices(ms_corr, ma_corr)
    s <- corrected_matrix_summary(aligned)
    s$comparison <- paste0(ms_method, "_corrected_matrix")
    summaries[[length(summaries) + 1]] <- s

    # Bland-Altman on log10 scale
    la <- to_log10(aligned$a); lb <- to_log10(aligned$b)
    ba <- data.frame(mean = (la + lb) / 2, diff = la - lb)
    p <- ggplot(ba, aes(mean, diff)) +
      geom_point(alpha = 0.2, size = 0.6) +
      geom_hline(yintercept = c(-1, 0, 1) * 1.96 * sd(ba$diff, na.rm = TRUE),
                 linetype = c(2, 1, 2), colour = "red") +
      labs(title = paste("Bland-Altman:", ms_method, "(log10)"),
           x = "mean(log10 MStargetR, log10 MA)",
           y = "log10 MStargetR - log10 MA") + theme_bw()
    ggsave(file.path(plot_dir, sprintf("bland_altman_%s.png", ms_method)),
           p, width = 7, height = 5, dpi = 120)

    # RSD comparison on corrected matrices
    rsd_ms <- rsd_from_qc(ms_corr, qc_names)
    rsd_ma <- rsd_from_qc(ma_corr, qc_names)
    rs <- rsd_summary(rsd_ms, rsd_ma)
    rs$comparison <- paste0(ms_method, "_rsd")
    summaries[[length(summaries) + 1]] <- rs

    common_f <- intersect(names(rsd_ms), names(rsd_ma))
    rdf <- data.frame(MStargetR = rsd_ms[common_f], MetaboAnalyst = rsd_ma[common_f])
    p2 <- ggplot(rdf, aes(MStargetR, MetaboAnalyst)) +
      geom_point(alpha = 0.4) + geom_abline(slope = 1, linetype = 2) +
      geom_vline(xintercept = 30, colour = "red", linetype = 3) +
      geom_hline(yintercept = 30, colour = "red", linetype = 3) +
      labs(title = paste("Per-feature %RSD after", ms_method)) + theme_bw()
    ggsave(file.path(plot_dir, sprintf("rsd_scatter_%s.png", ms_method)),
           p2, width = 6, height = 6, dpi = 120)
  }
}

# ---- PCA equivalence (autoscaled both sides) ------------------------------
message("04: equivalence for PCA")
ms_pca <- tryCatch({
  m <- as.matrix(data[, feat_cols, drop = FALSE])
  for (j in seq_len(ncol(m))) {
    na <- is.na(m[, j]); if (any(na)) m[na, j] <- min(m[, j], na.rm = TRUE) / 2
  }
  pr <- stats::prcomp(m, center = TRUE, scale. = TRUE)
  list(scores = pr$x[, seq_len(min(3, ncol(pr$x)))],
       var = (pr$sdev^2 / sum(pr$sdev^2))[seq_len(min(3, length(pr$sdev)))],
       names = data$sample_name)
}, error = function(e) { message("   MStargetR PCA: ", conditionMessage(e)); NULL })

ma_pca_out <- bench_path("ma_eq_pca.csv")
ma_pca_ok <- tryCatch({
  run_ma_stage("pca", input = "shared_table.csv", out = "ma_eq_pca.csv",
               mount_dir = bench_dir(), extra_args = "--scale=AutoNorm")
  file.exists(ma_pca_out)
}, error = function(e) { message("   MA PCA: ", conditionMessage(e)); FALSE })

if (!is.null(ms_pca) && isTRUE(ma_pca_ok)) {
  ma_sc <- read_ma_corrected(ma_pca_out)
  rownames(ma_sc) <- ma_sc$sample_name
  # ms_pca$names comes from data$sample_name which may still carry ".mzML";
  # strip it so it matches the normalised MA names from read_ma_corrected().
  ms_names_norm <- sub("\\.mzML$", "", ms_pca$names, ignore.case = TRUE)
  ma_scores <- ma_sc[match(ms_names_norm, ma_sc$sample_name),
                     grep("^PC", names(ma_sc), ignore.case = TRUE)[1:min(3, sum(grepl("^PC", names(ma_sc), ignore.case = TRUE)))]]
  s <- pca_summary(ms_pca$scores, ma_scores, var_a = ms_pca$var)
  s$comparison <- "PCA_scores"
  summaries[[length(summaries) + 1]] <- s
}

# ---- write -----------------------------------------------------------------
all_cols <- unique(unlist(lapply(summaries, names)))
norm_row <- function(s) { for (c in setdiff(all_cols, names(s))) s[[c]] <- NA; s[, all_cols] }
eq <- do.call(rbind, lapply(summaries, norm_row))
eq <- eq[, c("comparison", setdiff(all_cols, "comparison"))]
outfile <- file.path(results_dir(), "bm_equivalence.csv")
utils::write.csv(eq, outfile, row.names = FALSE)
message("04: wrote ", outfile)
print(eq)
