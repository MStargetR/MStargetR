# Regression tests for canonical metadata normalisation (H3 in the plan).
#
# These cover paths introduced by commits 3b7207c (batchCorrectR metadata
# normalisation) and 41a1724 (sample_timestamp alignment with qcCheckR) so
# that future schema changes do not silently bypass the normalisation logic
# because the narrow legacy fixture lacks these columns.

test_that("parse_sample_timestamp returns POSIXct for character input", {
  ts_chr <- c("2026-01-15T09:00:00Z", "2026-01-15T09:01:00Z",
              "2026-01-15T09:02:00Z")
  parsed <- parse_sample_timestamp(ts_chr)
  expect_s3_class(parsed, "POSIXct")
  expect_equal(length(parsed), 3L)
  expect_false(any(is.na(parsed)))
  # Monotonic increasing at 60s intervals.
  expect_true(all(diff(as.numeric(parsed)) == 60))
})

test_that("parse_sample_timestamp returns POSIXct unchanged when already POSIXct", {
  ts <- as.POSIXct(c("2026-01-15 09:00:00", "2026-01-15 09:01:00"), tz = "UTC")
  expect_identical(parse_sample_timestamp(ts), ts)
})

test_that("parse_sample_timestamp handles factor input", {
  ts_f <- factor(c("2026-01-15T09:00:00Z", "2026-01-15T09:01:00Z"))
  parsed <- parse_sample_timestamp(ts_f)
  expect_s3_class(parsed, "POSIXct")
  expect_false(any(is.na(parsed)))
})

test_that("make_bc_data extended fixture carries canonical metadata", {
  df <- make_bc_data(include_extras = TRUE)
  expected_cols <- c("sample_name", "batch", "sample_type", "run_order",
                     "sample_timestamp", "sample_class", "sample_plate_id",
                     "sample_plate_order", "sample_matrix",
                     "sample_data_source", "synthetic_qc")
  expect_true(all(expected_cols %in% names(df)))
  # Timestamps are ISO-8601 strings parseable to POSIXct.
  parsed <- parse_sample_timestamp(df$sample_timestamp)
  expect_s3_class(parsed, "POSIXct")
  expect_false(any(is.na(parsed)))
})

test_that("bc_detect_metabolite_columns leaves canonical metadata out", {
  df <- make_bc_data(include_extras = TRUE)
  mets <- bc_detect_metabolite_columns(df)
  # Only the two metabolite columns are returned; every canonical metadata
  # column from .METADATA_COLS stays on the metadata side.
  expect_setequal(mets, c("metab_A", "metab_B"))
  for (col in .METADATA_COLS) {
    if (col %in% names(df)) {
      expect_false(col %in% mets,
                   info = paste0("metadata column leaked into metabolites: ",
                                 col))
    }
  }
})

test_that("bc_preprocess_input accepts extended fixture end-to-end", {
  df <- make_bc_data(include_extras = TRUE)
  # The narrow legacy schema is preserved for back-compat, but the extended
  # path must succeed without dropping canonical columns.
  out <- bc_preprocess_input(df)
  expect_true("sample_timestamp" %in% names(out))
  # Timestamp should survive (character OR POSIXct, depending on
  # preprocessor behaviour -- both are acceptable as long as it is not
  # coerced to logical / numeric / lost).
  expect_true(is.character(out$sample_timestamp) ||
                inherits(out$sample_timestamp, "POSIXct") ||
                is.factor(out$sample_timestamp))
})
