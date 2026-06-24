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

test_that("parse_sample_timestamp honours 12-hour AM/PM clocks with seconds", {
  # Skyline exports on a US-locale machine produce 12-hour AcquiredTime
  # strings *with* seconds (e.g. "6/09/2026  12:38:56 AM"). The AM/PM
  # format must be tried before the 24-hour formats so the meridiem is not
  # silently dropped (which would read 12:38:56 AM as 12:38:56 instead of
  # 00:38:56, and any PM time 12 hours early). Regression for the cohort
  # date-detection crash on AM/PM-with-seconds cohorts.
  withr::with_envvar(c(TZ = "UTC"), {
    # Midnight hour: 12:38:56 AM == 00:38:56.
    mdy_am <- parse_sample_timestamp("6/09/2026  12:38:56 AM", "mdy")
    expect_equal(as.numeric(mdy_am),
                 as.numeric(as.POSIXct("2026-06-09 00:38:56", tz = "UTC")))

    # Afternoon: 6:12:31 PM == 18:12:31.
    mdy_pm <- parse_sample_timestamp("09/27/2024 6:12:31 PM", "mdy")
    expect_equal(as.numeric(mdy_pm),
                 as.numeric(as.POSIXct("2024-09-27 18:12:31", tz = "UTC")))

    # Day-first interpretation of the same ambiguous string.
    dmy_am <- parse_sample_timestamp("6/09/2026  12:38:56 AM", "dmy")
    expect_equal(as.numeric(dmy_am),
                 as.numeric(as.POSIXct("2026-09-06 00:38:56", tz = "UTC")))

    # auto mode also now honours the meridiem instead of mis-reading it.
    auto_am <- parse_sample_timestamp("6/09/2026  12:38:56 AM", "auto")
    expect_false(is.na(auto_am))
    expect_equal(as.integer(format(auto_am, "%H", tz = "UTC")), 0L)
  })
})

test_that("parse_sample_timestamp parses slash formats as UTC regardless of session TZ", {
  # Skyline AcquiredTime strings carry no timezone. They must be parsed as
  # UTC so they stay aligned with the genuinely-UTC ISO startTimeStamp that
  # mzR provides -- extract_run_order() prefers the mzR value and falls back
  # to AcquiredTime per-sample, so a TZ-dependent parse would offset the two
  # against each other within a single plate. Run under a non-UTC session TZ
  # (Perth, UTC+8) so a missing tz="UTC" in the parser would surface as an
  # 8-hour shift. Regression for the local-vs-UTC parse inconsistency.
  withr::with_envvar(c(TZ = "Australia/Perth"), {
    # Slash/24-hour: clock time must be read as UTC, not local+8.
    mdy <- parse_sample_timestamp("06/20/2026 13:39:03", "mdy")
    expect_equal(as.numeric(mdy),
                 as.numeric(as.POSIXct("2026-06-20 13:39:03", tz = "UTC")))

    # US 12-hour with the double-space padding artifact, parsed as UTC.
    mdy_am <- parse_sample_timestamp("6/04/2026  10:25:03 AM", "mdy")
    expect_equal(as.numeric(mdy_am),
                 as.numeric(as.POSIXct("2026-06-04 10:25:03", tz = "UTC")))

    # The slash-parsed AcquiredTime and the equivalent ISO startTimeStamp
    # must resolve to the same instant, so a mixed-source column is coherent.
    iso <- parse_sample_timestamp("2026-06-20T13:39:03Z", "auto")
    expect_equal(as.numeric(mdy), as.numeric(iso))
  })
})

test_that("parse_sample_timestamp leaves existing 24-hour formats unchanged", {
  withr::with_envvar(c(TZ = "UTC"), {
    expect_equal(
      as.numeric(parse_sample_timestamp("09/27/2024 10:41:28", "mdy")),
      as.numeric(as.POSIXct("2024-09-27 10:41:28", tz = "UTC")))
    expect_equal(
      as.numeric(parse_sample_timestamp("13/03/2021 18:12:31", "dmy")),
      # dmy family historically matches "%H:%M" before "%H:%M:%S", so the
      # trailing seconds are dropped -- preserved here intentionally.
      as.numeric(as.POSIXct("2021-03-13 18:12:00", tz = "UTC")))
    expect_equal(
      as.numeric(parse_sample_timestamp("2021-03-13T18:12:31Z", "auto")),
      as.numeric(as.POSIXct("2021-03-13 18:12:31", tz = "UTC")))
  })
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
