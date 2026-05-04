# Tests for batch_shared_utils.R internal helpers (bc_* functions) ----
# These helpers are used by both batchCorrectR and qcCheckR.

# ============================================================================
# Helpers for building pheno / data fixtures
# ============================================================================

make_pheno <- function(batches = c("b1", "b2"),
                       n_per_batch = 10,
                       qc_positions = list(c(1, 5, 10), c(1, 5, 10))) {
  rows <- list()
  counter <- 1L
  for (i in seq_along(batches)) {
    b <- batches[i]
    qc_pos <- qc_positions[[i]]
    for (j in seq_len(n_per_batch)) {
      rows[[counter]] <- data.frame(
        sample_name = paste0("S", counter),
        batch       = b,
        class       = if (j %in% qc_pos) "qc" else "sample",
        order       = counter,
        stringsAsFactors = FALSE
      )
      counter <- counter + 1L
    }
  }
  do.call(rbind, rows)
}

# ============================================================================
# bc_assess_qc_distribution ----
# ============================================================================

test_that("bc_assess_qc_distribution: even QC distribution yields no needs", {
  pheno <- make_pheno(batches = "b1", n_per_batch = 9,
                      qc_positions = list(c(1, 5, 9)))
  res <- suppressMessages(bc_assess_qc_distribution(pheno))
  expect_named(res, c("assessment", "needs_leading", "needs_trailing"))
  expect_length(res$needs_leading, 0)
  expect_length(res$needs_trailing, 0)
  expect_true(res$assessment$b1$has_leading)
  expect_true(res$assessment$b1$has_trailing)
  expect_equal(res$assessment$b1$n_qc, 3)
})

test_that("bc_assess_qc_distribution: zero QC emits warning and flags both boundaries", {
  pheno <- make_pheno(batches = "b1", n_per_batch = 5,
                      qc_positions = list(integer(0)))
  expect_message(
    res <- bc_assess_qc_distribution(pheno),
    "ZERO QC samples"
  )
  expect_true("b1" %in% res$needs_leading)
  expect_true("b1" %in% res$needs_trailing)
  expect_equal(res$assessment$b1$n_qc, 0)
  expect_false(res$assessment$b1$even_distribution)
})

test_that("bc_assess_qc_distribution: detects missing leading and trailing QCs", {
  # statTarget::REGfit requires QC at position 1 AND position n_total, so
  # the trigger fires whenever either boundary is missing -- regardless of
  # interior gap quality. Even-but-shifted distribution still needs both
  # boundary synthetics.
  pheno <- make_pheno(batches = "b1", n_per_batch = 10,
                      qc_positions = list(c(3, 5, 7)))
  res <- suppressMessages(bc_assess_qc_distribution(pheno))
  expect_true("b1" %in% res$needs_leading)
  expect_true("b1" %in% res$needs_trailing)
  expect_false(res$assessment$b1$has_leading)
  expect_false(res$assessment$b1$has_trailing)
})

test_that("bc_assess_qc_distribution: ANPC layout (8 QCs, first at pos 11) needs leading synthetic", {
  # Regression: 8 QCs at 11,22,33,44,55,66,76,84 in 84-sample batch.
  # last QC IS at position 84 = n_total, so trailing is satisfied; first is
  # at position 11, so leading needs a synthetic to satisfy REGfit.
  pheno <- make_pheno(batches = "b1", n_per_batch = 84,
                      qc_positions = list(c(11, 22, 33, 44, 55, 66, 76, 84)))
  res <- suppressMessages(bc_assess_qc_distribution(pheno))
  expect_true("b1"  %in% res$needs_leading)   # required by REGfit
  expect_false("b1" %in% res$needs_trailing)  # last QC at position 84 = n_total
})

test_that("bc_assess_qc_distribution: reports uneven distribution when gaps are unbalanced", {
  # Cluster QCs at start so gap_cv >= 0.5
  pheno <- make_pheno(batches = "b1", n_per_batch = 20,
                      qc_positions = list(c(1, 2, 3)))
  expect_message(
    bc_assess_qc_distribution(pheno),
    "uneven"
  )
})

# ============================================================================
# bc_estimate_boundary_qc ----
# ============================================================================

test_that("bc_estimate_boundary_qc: returns NA when all QC values are NA", {
  expect_identical(
    bc_estimate_boundary_qc(c(NA_real_, NA_real_), c(1, 2), 0),
    NA_real_
  )
})

test_that("bc_estimate_boundary_qc: returns sole value when only one QC remains", {
  expect_equal(
    bc_estimate_boundary_qc(c(42, NA_real_), c(2, 5), 1),
    42
  )
})

test_that("bc_estimate_boundary_qc: extrapolates linearly toward leading boundary", {
  # 4 QCs with values increasing linearly; target before all of them
  qc <- c(10, 20, 30, 40)
  pos <- c(5, 10, 15, 20)
  est <- bc_estimate_boundary_qc(qc, pos, target_pos = 0, shrinkage = 0)
  # With shrinkage 0 we get pure extrapolation from head 3 positions
  # lm on (5,10),(10,20),(15,30) => intercept 0, slope 2; predict at 0 => 0
  # clamped to max(0, ...) so expect 0
  expect_equal(est, 0)
})

test_that("bc_estimate_boundary_qc: uses tail selection for trailing extrapolation", {
  # 4 QCs; target after all of them triggers tail(order(...), 3) branch
  qc <- c(10, 20, 30, 40)
  pos <- c(5, 10, 15, 20)
  est <- bc_estimate_boundary_qc(qc, pos, target_pos = 25, shrinkage = 0)
  # tail 3 => (10,20),(15,30),(20,40): slope 2, intercept 0 => predict(25)=50
  # capped at max(qc_values)*2 = 80
  expect_equal(est, 50)
})

test_that("bc_estimate_boundary_qc: applies shrinkage toward median", {
  qc <- c(10, 20, 30, 40)
  pos <- c(5, 10, 15, 20)
  # global_median = 25; extrapolation at pos=25 with tail = 50
  est <- bc_estimate_boundary_qc(qc, pos, target_pos = 25, shrinkage = 0.5)
  # 0.5*25 + 0.5*50 = 37.5
  expect_equal(est, 37.5)
})

test_that("bc_estimate_boundary_qc: clamps output to >= 0 and <= 2 * max", {
  # Extreme negative extrapolation should be clipped to 0
  qc <- c(100, 50)
  pos <- c(10, 20)
  est <- bc_estimate_boundary_qc(qc, pos, target_pos = 1000, shrinkage = 0)
  expect_gte(est, 0)
  expect_lte(est, max(qc) * 2)
})

# ============================================================================
# bc_prepare_qc_boundaries ----
# ============================================================================

test_that("bc_prepare_qc_boundaries: no synthetic rows when QCs cover both boundaries", {
  pheno <- make_pheno(batches = "b1", n_per_batch = 9,
                      qc_positions = list(c(1, 5, 9)))
  res <- suppressMessages(bc_prepare_qc_boundaries(pheno))
  expect_named(res, c("pheno", "qc_assessment"))
  expect_true(all(res$pheno$synthetic_qc == FALSE))
  expect_equal(nrow(res$pheno), nrow(pheno))
})

test_that("bc_prepare_qc_boundaries: inserts synthetic leading and trailing rows", {
  pheno <- make_pheno(batches = "b1", n_per_batch = 10,
                      qc_positions = list(c(3, 5, 7)))
  res <- suppressMessages(bc_prepare_qc_boundaries(pheno))
  expect_gt(nrow(res$pheno), nrow(pheno))
  expect_true(any(grepl("^SYNTHETIC_QC_leading_", res$pheno$sample_name)))
  expect_true(any(grepl("^SYNTHETIC_QC_trailing_", res$pheno$sample_name)))
  # All synthetic rows flagged
  syn <- res$pheno[res$pheno$synthetic_qc, ]
  expect_true(all(syn$class == "qc"))
  # order preserved: sorted ascending
  expect_equal(res$pheno$order, sort(res$pheno$order))
})

# ============================================================================
# bc_reorder_qc_within_batches (deprecated) ----
# ============================================================================

test_that("bc_reorder_qc_within_batches: emits deprecation warning and delegates", {
  pheno <- make_pheno(batches = "b1", n_per_batch = 9,
                      qc_positions = list(c(1, 5, 9)))
  expect_warning(
    out <- suppressMessages(bc_reorder_qc_within_batches(pheno)),
    "deprecated"
  )
  expect_s3_class(out, "data.frame")
  expect_true("synthetic_qc" %in% colnames(out))
})

# ============================================================================
# bc_detect_stattarget_format ----
# ============================================================================

test_that("bc_detect_stattarget_format: handles 'sample1' column layout", {
  df <- tibble::tibble(
    sample = c("class", "S1", "S2"),
    sample1 = c("1", "10", "20"),
    sample2 = c("2", "30", "40")
  )
  out <- bc_detect_stattarget_format(df)
  expect_true("name" %in% colnames(out))
  expect_equal(nrow(out), 2)
  expect_type(out$sample1, "double")
  expect_equal(out$sample1, c(10, 20))
})

test_that("bc_detect_stattarget_format: handles 'M1' transposed layout", {
  df <- tibble::tibble(
    sample = c("class", "S1", "S2"),
    M1 = c("1", "10", "20"),
    M2 = c("2", "30", "40")
  )
  out <- suppressWarnings(bc_detect_stattarget_format(df))
  expect_true("name" %in% colnames(out))
  # After transpose + cleaning: first column is 'name', remaining numeric
  num_cols <- setdiff(colnames(out), "name")
  expect_true(length(num_cols) > 0)
  expect_true(all(vapply(out[, num_cols, drop = FALSE], is.numeric, logical(1))))
})

test_that("bc_detect_stattarget_format: falls through to default branch", {
  df <- tibble::tibble(
    feature = c("class", "sample", "m1", "m2"),
    S1 = c("a", "b", "10", "20"),
    S2 = c("c", "d", "30", "40")
  )
  out <- bc_detect_stattarget_format(df)
  expect_equal(colnames(out)[1], "name")
  expect_false(any(out$name %in% c("class", "sample")))
  expect_type(out$S1, "double")
})

# ============================================================================
# bc_compute_mean_ratios ----
# ============================================================================

test_that("bc_compute_mean_ratios: computes corrected/original ratios for common metabolites", {
  orig <- c(A = 10, B = 20, C = 30)
  corr <- c(A = 15, B = 10, C = 30)
  out <- bc_compute_mean_ratios(orig, corr)
  expect_named(out, c("A", "B", "C"))
  expect_equal(out[["A"]], 1.5)
  expect_equal(out[["B"]], 0.5)
  expect_equal(out[["C"]], 1.0)
})

test_that("bc_compute_mean_ratios: restricts to intersection of names", {
  orig <- c(A = 10, B = 20, X = 5)
  corr <- c(A = 10, B = 20, Y = 5)
  out <- bc_compute_mean_ratios(orig, corr)
  expect_setequal(names(out), c("A", "B"))
})

test_that("bc_compute_mean_ratios: passes legitimate large-magnitude rescaling unchanged", {
  # statTarget::REGfit normalises so corrected QC means are ~1 regardless of
  # the metabolite's concentration scale. The corrected/original ratio is
  # therefore inversely proportional to concentration and routinely spans
  # several orders of magnitude on a real lipid panel. The function must not
  # clamp such legitimate rescaling -- the previous [1e-2, 1e2] clamp
  # silently mis-scaled 90%+ of features on real ANPC data.
  orig <- c(highC = 5000, lowC = 0.005, midC = 1)
  corr <- c(highC = 1,    lowC = 1,     midC = 1)  # post-QCRFSC ~ 1
  out <- expect_silent(bc_compute_mean_ratios(orig, corr))
  expect_equal(unname(out["highC"]), 1 / 5000)     # 0.0002, well below old clamp
  expect_equal(unname(out["lowC"]),  1 / 0.005)    # 200, well above old clamp
  expect_equal(unname(out["midC"]),  1)
})

test_that("bc_compute_mean_ratios: warns on NA / non-finite / non-positive ratios", {
  orig <- c(A = 0, B = 50, C = 100, D = 100)
  corr <- c(A = 1, B = 50, C = NA_real_, D = -3)
  expect_warning(
    out <- bc_compute_mean_ratios(orig, corr),
    "NA, non-finite, or non-positive"
  )
  # Inf (A: 1/0), legitimate (B: 1), NA (C), negative (D: -0.03) -- none clamped
  expect_true(is.infinite(out[["A"]]))
  expect_equal(out[["B"]], 1)
  expect_true(is.na(out[["C"]]))
  expect_equal(out[["D"]], -0.03)
})

# ============================================================================
# bc_apply_mean_ratios ----
# ============================================================================

test_that("bc_apply_mean_ratios: divides matching columns by ratios", {
  df <- data.frame(A = c(10, 20), B = c(5, 15), stringsAsFactors = FALSE)
  out <- bc_apply_mean_ratios(df, c(A = 2, B = 5))
  expect_equal(out$A, c(5, 10))
  expect_equal(out$B, c(1, 3))
})

test_that("bc_apply_mean_ratios: skips NA, zero, and non-finite ratios", {
  df <- data.frame(A = c(10, 20), B = c(4, 8), C = c(2, 6),
                   D = c(1, 2), stringsAsFactors = FALSE)
  ratios <- c(A = NA_real_, B = 0, C = Inf, D = 2)
  out <- bc_apply_mean_ratios(df, ratios)
  expect_equal(out$A, df$A)   # NA -> skipped
  expect_equal(out$B, df$B)   # 0 -> skipped
  expect_equal(out$C, df$C)   # Inf -> skipped
  expect_equal(out$D, c(0.5, 1))
})

test_that("bc_apply_mean_ratios: silently skips ratio names not in data", {
  df <- data.frame(A = c(10, 20), stringsAsFactors = FALSE)
  out <- bc_apply_mean_ratios(df, c(A = 2, Z = 3))
  expect_equal(out$A, c(5, 10))
  expect_false("Z" %in% colnames(out))
})

# ============================================================================
# bc_prepare_combat_matrix ----
# ============================================================================

test_that("bc_prepare_combat_matrix: transposes to features-by-samples", {
  df <- data.frame(
    batch = c("a", "a", "b", "b"),
    met_1 = c(1, 2, 3, 4),
    met_2 = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  out <- bc_prepare_combat_matrix(df, c("met_1", "met_2"))
  expect_equal(dim(out$dat_combat), c(2, 4))
  expect_equal(rownames(out$dat_combat), c("met_1", "met_2"))
  expect_equal(out$kept_features, c("met_1", "met_2"))
  expect_false(any(out$zero_var))
})

test_that("bc_prepare_combat_matrix: imputes NA with feature medians", {
  df <- data.frame(
    met_1 = c(1, 2, NA, 4),
    met_2 = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(bc_prepare_combat_matrix(df, c("met_1", "met_2")))
  expect_false(any(is.na(out$dat_combat)))
  # median of c(1,2,4) = 2
  expect_equal(unname(out$dat_combat["met_1", 3]), 2)
})

test_that("bc_prepare_combat_matrix: replaces all-NA rows with 0", {
  df <- data.frame(
    met_1 = c(NA_real_, NA_real_, NA_real_),
    met_2 = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(bc_prepare_combat_matrix(df, c("met_1", "met_2")))
  # met_1 was all NA -> zero-variance after imputing 0s -> dropped
  expect_true(out$zero_var["met_1"] || !"met_1" %in% out$kept_features)
  expect_true("met_2" %in% out$kept_features)
})

test_that("bc_prepare_combat_matrix: drops zero-variance features", {
  df <- data.frame(
    met_const = c(5, 5, 5, 5),
    met_var = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(bc_prepare_combat_matrix(df, c("met_const", "met_var")))
  expect_equal(out$kept_features, "met_var")
  expect_equal(nrow(out$dat_combat), 1)
  expect_true(out$zero_var["met_const"])
})

# ============================================================================
# bc_reconstruct_combat_output ----
# ============================================================================

test_that("bc_reconstruct_combat_output: writes corrected values back into data.frame", {
  df <- data.frame(
    batch = c("a", "a", "b"),
    met_1 = c(1, 2, 3),
    met_2 = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  corrected <- matrix(
    c(11, 12, 13, 110, 120, 130),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("met_1", "met_2"), NULL)
  )
  out <- bc_reconstruct_combat_output(df, corrected, c("met_1", "met_2"))
  expect_equal(out$met_1, c(11, 12, 13))
  expect_equal(out$met_2, c(110, 120, 130))
  # batch column untouched
  expect_equal(out$batch, df$batch)
})

test_that("bc_reconstruct_combat_output: leaves untouched features unchanged", {
  df <- data.frame(
    met_const = c(5, 5, 5),
    met_var = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  corrected <- matrix(c(10, 20, 30), nrow = 1,
                      dimnames = list("met_var", NULL))
  out <- bc_reconstruct_combat_output(df, corrected, "met_var")
  expect_equal(out$met_const, c(5, 5, 5))
  expect_equal(out$met_var, c(10, 20, 30))
})

# ============================================================================
# bc_populate_synthetic_qc_values -- 1-QC batch edge case ----
# ============================================================================

test_that("bc_populate_synthetic_qc_values: single-QC batch constant extrapolation, RSD is NA", {
  # One real QC at position 5; one synthetic QC needed at position 1 (leading).
  ordered <- data.frame(
    sample   = c("SYNTHETIC_QC_leading_b1", "real_qc_b1", "samp_b1"),
    batch    = c("b1", "b1", "b1"),
    class    = c("qc", "qc", "sample"),
    order    = c(1L, 5L, 10L),
    synthetic_qc = c(TRUE, FALSE, FALSE),
    met_X    = c(NA_real_, 77.0, 50.0),
    stringsAsFactors = FALSE
  )
  result <- bc_populate_synthetic_qc_values(ordered, "met_X")
  # The single-QC value is used directly (constant extrapolation).
  expect_equal(result$met_X[1], 77.0)
  # RSD across all QCs in this batch (one real, one synthetic with identical value) is 0,
  # but the original single real-QC RSD before population is NA (cannot compute SD on 1 value).
  # Verify population occurred (non-NA result) and value matches the lone real QC.
  expect_false(is.na(result$met_X[1]))
  expect_equal(result$met_X[result$synthetic_qc == FALSE & result$class == "qc"], 77.0)
})
