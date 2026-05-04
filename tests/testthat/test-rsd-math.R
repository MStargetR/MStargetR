# Regression tests for RSD math unification (H1 in keen-finding-kahan.md).
#
# The QC RSD formula MUST be identical in:
#   - R/qcCheckR_filter.R (main filter pipeline)
#   - R/batchCorrectR_Utils.R::bc_calculate_rsd (standalone batchCorrectR)
#
# Unified rule: RSD = sd(v) / abs(mean(v)) * 100, with mean set to NA
# whenever |mean(v)| < .RSD_ZERO_EPS or there are < 2 non-NA values.
#
# These tests protect against regressions where either path starts using a
# signed mean or a different zero-guard.

# --- bc_calculate_rsd: denominator uses |mean| ---

test_that("bc_calculate_rsd returns non-negative RSD when QC mean is negative", {
  # Values with a negative mean (e.g. centred / log-transformed data). A
  # signed-mean RSD would return -20; the unified formula must return 20.
  df <- data.frame(
    sample_name = paste0("S", 1:3),
    batch = "b1", sample_type = "qc", run_order = 1:3,
    met = c(-4, -5, -6),
    stringsAsFactors = FALSE
  )
  rsd <- bc_calculate_rsd(df, qc_label = "qc", metabolite_cols = "met")
  expect_false(is.na(rsd[["met"]]))
  expect_gte(rsd[["met"]], 0)
  # stats::sd(c(-4,-5,-6)) = 1; abs(mean) = 5 => 20%
  expect_equal(rsd[["met"]], 20, tolerance = 1e-9)
})

# --- bc_calculate_rsd: near-zero mean guarded by shared epsilon ---

test_that("bc_calculate_rsd returns NA when |mean| < .RSD_ZERO_EPS", {
  # Values whose mean is tiny but nonzero. Exact-equality guard would pass
  # the guard and return Inf. Shared epsilon guard must return NA_real_.
  tiny <- .RSD_ZERO_EPS / 10
  df <- data.frame(
    sample_name = paste0("S", 1:3),
    batch = "b1", sample_type = "qc", run_order = 1:3,
    met = c(tiny, tiny, tiny),
    stringsAsFactors = FALSE
  )
  rsd <- bc_calculate_rsd(df, qc_label = "qc", metabolite_cols = "met")
  expect_true(is.na(rsd[["met"]]))
})

test_that("bc_calculate_rsd still computes for mean above .RSD_ZERO_EPS", {
  big_enough <- .RSD_ZERO_EPS * 100
  df <- data.frame(
    sample_name = paste0("S", 1:3),
    batch = "b1", sample_type = "qc", run_order = 1:3,
    met = c(big_enough, big_enough * 1.01, big_enough * 0.99),
    stringsAsFactors = FALSE
  )
  rsd <- bc_calculate_rsd(df, qc_label = "qc", metabolite_cols = "met")
  expect_false(is.na(rsd[["met"]]))
  expect_true(is.finite(rsd[["met"]]))
})

# --- Shared constant sanity ---

test_that(".RSD_ZERO_EPS is the shared epsilon value", {
  expect_true(exists(".RSD_ZERO_EPS"))
  expect_equal(.RSD_ZERO_EPS, .Machine$double.eps * 100)
})
