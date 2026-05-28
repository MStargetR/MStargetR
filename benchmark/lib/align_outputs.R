# benchmark/lib/align_outputs.R
# Re-key and align two corrected matrices (MStargetR vs MetaboAnalyst) onto a
# common (sample x feature) grid before any numeric comparison. MetaboAnalyst
# reorders/renames features and samples, so naive element-wise comparison is
# wrong without this step.

# Coerce a "wide" engine output (rows = samples, cols = features + metadata)
# into a long tibble: sample_name, feature, value. `meta_cols` are dropped.
to_long <- function(df, sample_col = "sample_name",
                    meta_cols = BENCH_META_COLS) {
  feat_cols <- setdiff(names(df), meta_cols)
  feat_cols <- setdiff(feat_cols, sample_col)
  stopifnot(sample_col %in% names(df))
  long <- data.frame(
    sample_name = rep(as.character(df[[sample_col]]), times = length(feat_cols)),
    feature     = rep(feat_cols, each = nrow(df)),
    value       = as.numeric(unlist(df[, feat_cols, drop = FALSE])),
    stringsAsFactors = FALSE
  )
  long
}

# Given two long tibbles, return the aligned wide matrices restricted to the
# common (sample, feature) keys, plus the keys that were dropped from each.
align_long <- function(long_a, long_b) {
  key <- function(l) paste(l$sample_name, l$feature, sep = "\r")
  ka <- key(long_a); kb <- key(long_b)
  common <- intersect(ka, kb)

  a <- long_a[match(common, ka), ]
  b <- long_b[match(common, kb), ]

  list(
    a = a$value,
    b = b$value,
    sample_name = a$sample_name,
    feature = a$feature,
    n_common = length(common),
    dropped_from_a = sum(!ka %in% kb),
    dropped_from_b = sum(!kb %in% ka)
  )
}

# Convenience: align two engine outputs (wide data.frames) directly.
align_matrices <- function(df_a, df_b,
                           sample_col_a = "sample_name",
                           sample_col_b = "sample_name") {
  align_long(to_long(df_a, sample_col_a), to_long(df_b, sample_col_b))
}

# Pull the corrected wide matrix out of a batchCorrectR() return value.
# batchCorrectR returns a list; corrected_data holds the user-schema frame.
extract_mstargetr_corrected <- function(bc_result) {
  if (is.data.frame(bc_result)) return(bc_result)
  if (is.list(bc_result) && !is.null(bc_result$corrected_data)) {
    return(bc_result$corrected_data)
  }
  stop("extract_mstargetr_corrected: could not find corrected_data in result.")
}

# Read MetaboAnalyst's signal-drift / corrected CSV (written by ma_runner.R).
# The runner normalizes it to the canonical wide schema.  MA preserves the
# raw mzML filename as the sample identifier (e.g. "sample.mzML"), while
# MStargetR strips the .mzML extension.  Normalise here so align_matrices()
# can find common keys without manual caller intervention.
read_ma_corrected <- function(path) {
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if ("sample_name" %in% names(df)) {
    df$sample_name <- sub("\\.mzML$", "", df$sample_name, ignore.case = TRUE)
  }
  df
}
