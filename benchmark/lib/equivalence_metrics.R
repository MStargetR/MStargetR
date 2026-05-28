# benchmark/lib/equivalence_metrics.R
# Numeric-agreement metrics for the output-equivalence analysis. All operate
# on aligned vectors/matrices produced by align_outputs.R.

# Use log10 of positive-clamped values as the canonical comparison scale
# (corrected intensities span orders of magnitude; QC-RLSC "subtract" can go
# negative, so clamp before logging).
to_log10 <- function(x, floor_val = 1) log10(pmax(x, floor_val))

# ---- Element-wise agreement (on a common scale) ---------------------------
rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))
mae  <- function(a, b) mean(abs(a - b), na.rm = TRUE)

# Lin's concordance correlation coefficient (agreement, not just correlation).
ccc <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  a <- a[ok]; b <- b[ok]
  if (length(a) < 3) return(NA_real_)
  ma <- mean(a); mb <- mean(b)
  va <- stats::var(a); vb <- stats::var(b)
  cov_ab <- stats::cov(a, b)
  2 * cov_ab / (va + vb + (ma - mb)^2)
}

# Per-feature correlation distribution (Pearson + Spearman) given aligned
# long vectors with a `feature` key.
per_feature_cor <- function(aligned, method = "pearson") {
  df <- data.frame(feature = aligned$feature, a = aligned$a, b = aligned$b)
  sp <- split(df, df$feature)
  r <- vapply(sp, function(d) {
    if (nrow(d) < 3) return(NA_real_)
    suppressWarnings(stats::cor(d$a, d$b, method = method,
                                use = "pairwise.complete.obs"))
  }, numeric(1))
  r
}

# Headline summary for a corrected-matrix comparison (compares on log10 scale).
corrected_matrix_summary <- function(aligned) {
  # Guard: if no common (sample, feature) keys were found, return an NA row.
  if (is.null(aligned$n_common) || aligned$n_common == 0L) {
    return(data.frame(
      n_common_values = 0L, n_features = 0L,
      dropped_from_a = aligned$dropped_from_a,
      dropped_from_b = aligned$dropped_from_b,
      rmse_log10 = NA_real_, mae_log10 = NA_real_, ccc_log10 = NA_real_,
      median_r_pearson = NA_real_, median_r_spearman = NA_real_,
      frac_features_r_gt_095 = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  la <- to_log10(aligned$a); lb <- to_log10(aligned$b)
  r_pearson  <- per_feature_cor(list(feature = aligned$feature,
                                      a = la, b = lb), "pearson")
  r_spearman <- per_feature_cor(list(feature = aligned$feature,
                                      a = aligned$a, b = aligned$b), "spearman")
  data.frame(
    n_common_values   = aligned$n_common,
    n_features         = length(unique(aligned$feature)),
    dropped_from_a     = aligned$dropped_from_a,
    dropped_from_b     = aligned$dropped_from_b,
    rmse_log10         = rmse(la, lb),
    mae_log10          = mae(la, lb),
    ccc_log10          = ccc(la, lb),
    median_r_pearson   = stats::median(r_pearson, na.rm = TRUE),
    median_r_spearman  = stats::median(r_spearman, na.rm = TRUE),
    frac_features_r_gt_095 = mean(r_pearson > 0.95, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# ---- RSD comparison --------------------------------------------------------
# Compare two per-feature RSD vectors (named by feature). Reports CCC, paired
# Wilcoxon p, and how many features cross the 30% / 20% thresholds each way.
rsd_summary <- function(rsd_a, rsd_b) {
  common <- intersect(names(rsd_a), names(rsd_b))
  a <- rsd_a[common]; b <- rsd_b[common]
  wp <- tryCatch(stats::wilcox.test(a, b, paired = TRUE)$p.value,
                 error = function(e) NA_real_)
  data.frame(
    n_features          = length(common),
    ccc                 = ccc(a, b),
    spearman            = suppressWarnings(stats::cor(a, b, method = "spearman",
                                                      use = "pairwise.complete.obs")),
    wilcoxon_p          = wp,
    median_rsd_a        = stats::median(a, na.rm = TRUE),
    median_rsd_b        = stats::median(b, na.rm = TRUE),
    pass30_a            = mean(a < 30, na.rm = TRUE),
    pass30_b            = mean(b < 30, na.rm = TRUE),
    pass20_a            = mean(a < 20, na.rm = TRUE),
    pass20_b            = mean(b < 20, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# ---- PCA comparison (rotation / sign / scale invariant) -------------------
# RV coefficient between two score matrices (samples x components).
rv_coefficient <- function(X, Y) {
  X <- scale(as.matrix(X), center = TRUE, scale = FALSE)
  Y <- scale(as.matrix(Y), center = TRUE, scale = FALSE)
  XX <- X %*% t(X); YY <- Y %*% t(Y)
  num <- sum(diag(XX %*% YY))
  den <- sqrt(sum(diag(XX %*% XX)) * sum(diag(YY %*% YY)))
  # Guard against NA/zero denominator (e.g. zero-variance score matrix).
  if (!is.finite(den) || den == 0) return(NA_real_)
  num / den
}

# Procrustes (uses vegan if available) + variance-explained comparison.
pca_summary <- function(scores_a, scores_b, var_a = NULL, var_b = NULL) {
  k <- min(ncol(scores_a), ncol(scores_b))
  sa <- as.matrix(scores_a)[, seq_len(k), drop = FALSE]
  sb <- as.matrix(scores_b)[, seq_len(k), drop = FALSE]
  rv <- rv_coefficient(sa, sb)
  proc_ss <- NA_real_; protest_p <- NA_real_
  if (requireNamespace("vegan", quietly = TRUE)) {
    pr <- tryCatch(vegan::procrustes(sa, sb, symmetric = TRUE),
                   error = function(e) NULL)
    if (!is.null(pr)) proc_ss <- pr$ss
    pt <- tryCatch(vegan::protest(sa, sb, permutations = 199),
                   error = function(e) NULL)
    if (!is.null(pt)) protest_p <- pt$signif
  }
  var_rmse <- if (!is.null(var_a) && !is.null(var_b)) {
    m <- min(length(var_a), length(var_b))
    rmse(var_a[seq_len(m)], var_b[seq_len(m)])
  } else NA_real_
  data.frame(
    n_components = k,
    rv_coefficient = rv,
    procrustes_ss = proc_ss,
    protest_p = protest_p,
    variance_explained_rmse = var_rmse,
    stringsAsFactors = FALSE
  )
}

# ---- Set-based decisions (filter / impute) --------------------------------
jaccard <- function(set_a, set_b) {
  u <- length(union(set_a, set_b))
  if (u == 0) return(NA_real_)
  length(intersect(set_a, set_b)) / u
}
