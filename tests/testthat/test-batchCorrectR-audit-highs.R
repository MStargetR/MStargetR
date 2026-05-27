# Tests for High-severity audit findings: BC-002, BC-003, BC-004, BC-005,
# BC-006, BC-007

suppressPackageStartupMessages(library(dplyr))

# BC-002 / BC-003: bc_populate_synthetic_qc_values
# Hoisted class_lc/is_qc and ungroup() defensive call

test_that("bc_populate_synthetic_qc_values returns identical results with grouped tibble input (BC-002)", {
  ordered <- tibble::tibble(
    batch = c("b1", "b1", "b1"),
    class = c("qc", "qc", "qc"),
    order = c(1, 2, 3),
    synthetic_qc = c(FALSE, FALSE, TRUE),
    met1 = c(10, 20, NA)
  )
  grouped <- dplyr::group_by(ordered, batch)
  result <- MStargetR:::bc_populate_synthetic_qc_values(grouped, "met1")
  expect_false(dplyr::is_grouped_df(result))
  expect_false(is.na(result$met1[3]))
})

test_that("bc_populate_synthetic_qc_values fills synthetic QC values correctly (BC-003)", {
  ordered <- tibble::tibble(
    batch = rep("b1", 5),
    class = c("qc", "sample", "qc", "sample", "qc"),
    order = c(1, 2, 3, 4, 5),
    synthetic_qc = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    met1 = c(10, NA, 20, NA, NA),
    met2 = c(100, NA, 200, NA, NA)
  )
  result <- MStargetR:::bc_populate_synthetic_qc_values(ordered, c("met1", "met2"))
  expect_false(is.na(result$met1[5]))
  expect_false(is.na(result$met2[5]))
})

# BC-003 (vectorization): the inner per-metabolite loop was replaced by a
# single vapply + row assignment. Verify the function still produces
# identical output to the naive per-column algorithm on a wide synthetic
# fixture (guards against subtle row-assignment bugs in the vectorised path).
test_that("bc_populate_synthetic_qc_values agrees with naive column-wise reference (BC-003 vectorised)", {
  set.seed(42)
  met_cols <- paste0("m", 1:20)
  n <- 12
  ordered <- tibble::tibble(
    batch = rep(c("b1", "b2"), each = n / 2),
    class = "qc",
    order = seq_len(n),
    synthetic_qc = c(TRUE, rep(FALSE, (n / 2) - 1),
                     TRUE, rep(FALSE, (n / 2) - 1))
  )
  for (mc in met_cols) ordered[[mc]] <- c(NA, stats::rnorm((n / 2) - 1, 100, 5),
                                          NA, stats::rnorm((n / 2) - 1, 100, 5))

  # Naive reference: literal per-column copy-on-modify loop.
  ref <- dplyr::ungroup(ordered)
  synth_idx <- which(ref$synthetic_qc)
  class_lc <- tolower(ref$class); is_qc <- is.na(class_lc) | class_lc == "qc"
  for (i in synth_idx) {
    mask <- ref$batch == ref$batch[i] & !ref$synthetic_qc & is_qc
    if (!any(mask)) next
    pos <- ref$order[mask]; target <- ref$order[i]
    for (met in met_cols) {
      ref[[met]][i] <- MStargetR:::bc_estimate_boundary_qc(
        ref[[met]][mask], pos, target
      )
    }
  }

  actual <- MStargetR:::bc_populate_synthetic_qc_values(ordered, met_cols)
  for (mc in met_cols) {
    expect_equal(actual[[mc]], ref[[mc]], tolerance = 1e-12,
                 info = paste("mismatch on", mc))
  }
})

# BC-004: sample_tags containing NA should give clean error

test_that("batchCorrectR stops cleanly when sample_tags contains NA (BC-004)", {
  df <- data.frame(
    sample_name = paste0("S", 1:8),
    batch = rep(c("b1", "b2"), each = 4),
    sample_type = rep(c("qc", "sample", "sample", "qc"), 2),
    run_order = 1:8,
    met1 = rnorm(8, 100, 10),
    met2 = rnorm(8, 200, 20),
    stringsAsFactors = FALSE
  )
  expect_error(
    suppressWarnings(
      batchCorrectR(df, qc_label = "qc", sample_tags = c("sample", NA),
                    method = "QCRFSC", plot = FALSE, report = FALSE)
    ),
    "sample_tags"
  )
})

# BC-005: NA factor rows in sample_type_factor should warn, not silently drop

test_that("batchCorrectR warns when sample_type_factor has NA rows in sample_tags filter (BC-005)", {
  df <- data.frame(
    sample_name = paste0("S", 1:8),
    batch = rep(c("b1", "b2"), each = 4),
    sample_type = rep(c("qc", "sample", "blank", "qc"), 2),
    sample_type_factor = factor(
      rep(c("qc", "sample", NA, "qc"), 2),
      levels = c("qc", "sample", "blank")
    ),
    run_order = 1:8,
    met1 = rnorm(8, 100, 10),
    met2 = rnorm(8, 200, 20),
    stringsAsFactors = FALSE
  )
  warns <- testthat::capture_warnings(
    tryCatch(
      batchCorrectR(df, qc_label = "qc", sample_tags = c("sample", "blank"),
                    method = "QCRFSC", plot = FALSE, report = FALSE),
      error = function(e) NULL
    )
  )
  expect_true(any(grepl("NA in sample_type", warns)))
})

# BC-006: bc_preprocess_input preserves original sample_type

test_that("bc_preprocess_input preserves sample_type_orig when overwriting from factor (BC-006)", {
  df <- data.frame(
    sample_name = paste0("S", 1:4),
    batch = "b1",
    sample_type = c("QC", "Sample", "Sample", "Blank"),
    sample_type_factor = factor(c("qc", "sample", "sample", NA),
                                levels = c("qc", "sample", "blank")),
    run_order = 1:4,
    met1 = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  result <- MStargetR:::bc_preprocess_input(df)
  expect_true("sample_type_orig" %in% colnames(result))
  expect_equal(result$sample_type_orig, c("QC", "Sample", "Sample", "Blank"))
})

test_that("bc_preprocess_input sample_type is character after overwrite (BC-006)", {
  df <- data.frame(
    sample_name = paste0("S", 1:3),
    batch = "b1",
    sample_type = c("qc", "sample", "blank"),
    sample_type_factor = factor(c("qc", "sample", "blank"),
                                levels = c("qc", "sample", "blank")),
    run_order = 1:3,
    met1 = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  result <- MStargetR:::bc_preprocess_input(df)
  expect_type(result$sample_type, "character")
})

# BC-007: bc_validate_input rejects fractional run_order

test_that("bc_validate_input stops on fractional run_order values (BC-007)", {
  df <- data.frame(
    sample_name = paste0("S", 1:4),
    batch = rep(c("b1", "b2"), each = 2),
    sample_type = rep(c("qc", "sample"), 2),
    run_order = c(1, 2, 2.5, 3),
    met1 = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  expect_error(
    MStargetR:::bc_validate_input(df, "qc", "QCRFSC", 500, 30, 0.8, "minHalf"),
    "integer values"
  )
})

test_that("bc_validate_input passes with integer run_order (BC-007)", {
  # bc_validate_input enforces >= 2 QCs per batch, so the fixture needs
  # two QCs in each of two batches (4 QCs + 4 samples = 8 rows).
  df <- data.frame(
    sample_name = paste0("S", 1:8),
    batch = rep(c("b1", "b2"), each = 4),
    sample_type = rep(c("qc", "sample", "sample", "qc"), times = 2),
    run_order = 1:8,
    met1 = c(10, 20, 30, 40, 50, 60, 70, 80),
    stringsAsFactors = FALSE
  )
  expect_true(
    MStargetR:::bc_validate_input(df, "qc", "QCRFSC", 500, 30, 0.8, "minHalf")
  )
})
