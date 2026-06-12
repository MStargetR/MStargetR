# Regression tests for cohort AcquiredTime date-order detection.
#
# Covers the crash where a US-locale Skyline export emits 12-hour AM/PM
# timestamps with seconds (e.g. "6/09/2026  12:38:56 AM") on plates whose
# acquisition date is an ISO *prefix* (2026-06-12_...) rather than a
# _YYYYMMDD suffix. Before the fix the per-plate hint never fired and the
# detector stopped with "could not unambiguously detect the date format".

test_that("parse_detection_timestamp honours AM/PM before 24-hour formats", {
  withr::with_envvar(c(TZ = "UTC"), {
    pdt <- MStargetR:::parse_detection_timestamp

    # 12:38:56 AM == 00:38:56, not 12:38:56.
    expect_equal(
      as.numeric(pdt("6/09/2026  12:38:56 AM", "mdy")),
      as.numeric(as.POSIXct("2026-06-09 00:38:56", tz = "UTC")))
    # 6:12:31 PM == 18:12:31.
    expect_equal(
      as.numeric(pdt("09/27/2024 6:12:31 PM", "mdy")),
      as.numeric(as.POSIXct("2024-09-27 18:12:31", tz = "UTC")))
    # AM/PM without seconds still resolves correctly.
    expect_equal(
      as.numeric(pdt("13/03/2021 6:12 PM", "dmy")),
      as.numeric(as.POSIXct("2021-03-13 18:12:00", tz = "UTC")))
    # Plain 24-hour strings are unaffected.
    expect_equal(
      as.numeric(pdt("13/03/2021 18:12:31", "dmy")),
      as.numeric(as.POSIXct("2021-03-13 18:12:31", tz = "UTC")))
    # Day-first reading of an ambiguous value.
    expect_equal(
      as.numeric(pdt("6/09/2026  12:38:56 AM", "dmy")),
      as.numeric(as.POSIXct("2026-09-06 00:38:56", tz = "UTC")))
    # Unparseable input -> NA, length preserved.
    out <- pdt(c("not a date", "6/09/2026  12:38:56 AM"), "mdy")
    expect_length(out, 2L)
    expect_true(is.na(out[1]))
    expect_false(is.na(out[2]))
  })
})

test_that("extract_plate_date_hint reads a leading ISO date prefix", {
  hint <- MStargetR:::extract_plate_date_hint

  iso_for <- function(y, m, d) {
    as.POSIXct(sprintf("%04d-%02d-%02dT12:00:00", y, m, d),
               format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  }

  # Dashed ISO prefix (the format that previously yielded no hint).
  expect_equal(hint("2026-06-12_ABA1HA_tumour_tissue_MS-LIPIDS-3"),
               iso_for(2026, 6, 12))
  # Compact YYYYMMDD prefix delimited by a separator.
  expect_equal(hint("20260612_ABA1HA_x"), iso_for(2026, 6, 12))
  # Historical trailing _YYYYMMDD suffix still works.
  expect_equal(hint("foo_BHASp06_20211004"), iso_for(2021, 10, 4))
  # Suffix takes precedence when both are present.
  expect_equal(hint("2030-01-01_run_20211004"), iso_for(2021, 10, 4))
  # No date anywhere -> NA.
  expect_true(is.na(hint("ABA1HA_no_date_here")))
  # Out-of-range year prefix is rejected (guards against project codes).
  expect_true(is.na(hint("1999-06-12_x")))
  # A digit run longer than 8 is not a compact date prefix.
  expect_true(is.na(hint("202606121_x")))
})

# Helper: minimal master_list shaped like the qcCheckR object the detector
# consumes. find_matching_report() matches by grepl(plate_id, report names),
# so report names == plate IDs.
.make_ml <- function(plate_ts) {
  reports <- lapply(plate_ts, function(ts) {
    data.frame(AcquiredTime = ts, stringsAsFactors = FALSE)
  })
  names(reports) <- names(plate_ts)
  transposed <- stats::setNames(
    vector("list", length(plate_ts)), names(plate_ts))
  list(data = list(PeakForgeRReport = reports,
                   peakArea = list(transposed = transposed)))
}

test_that("detect_cohort_date_order breaks ambiguous AM/PM ties via ISO prefix", {
  withr::with_envvar(c(TZ = "UTC"), {
    # Every value is digit-ambiguous (both positions <= 12) and uses the
    # 12-hour AM/PM clock with seconds. The plate prefix 2026-06-12 selects
    # month-first: mdy reads 6/09..6/11 as June (close to the prefix),
    # dmy reads them as the 6th of Sep/Oct/Nov (far away).
    ml <- .make_ml(list(
      "2026-06-12_ABA1HA_tumour_tissue_MS-LIPIDS-3" = c(
        "6/09/2026  12:38:56 AM",
        "6/10/2026  1:15:00 PM",
        "6/11/2026  2:00:00 PM")
    ))
    order <- suppressMessages(MStargetR:::detect_cohort_date_order(ml))
    expect_equal(order, "mdy")
  })
})

test_that("detect_cohort_date_order still errors when truly ambiguous with no hint", {
  withr::with_envvar(c(TZ = "UTC"), {
    ml <- .make_ml(list(
      "ABA1HA_tumour_tissue_no_date" = c(
        "6/09/2026  12:38:56 AM",
        "7/10/2026  1:15:00 PM")
    ))
    expect_error(
      suppressMessages(MStargetR:::detect_cohort_date_order(ml)),
      "could not unambiguously detect the date format")
  })
})

test_that("detect_cohort_date_order locks DMY when a day part exceeds 12", {
  withr::with_envvar(c(TZ = "UTC"), {
    ml <- .make_ml(list(
      "2026-06-12_plateA" = c(
        "13/06/2026  9:00:00 AM",   # day=13 -> only DMY can parse
        "9/06/2026  10:00:00 AM")
    ))
    order <- suppressMessages(MStargetR:::detect_cohort_date_order(ml))
    expect_equal(order, "dmy")
  })
})
