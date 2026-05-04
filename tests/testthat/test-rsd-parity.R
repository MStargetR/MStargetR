# Regression tests for the QC Checker / Result Explorer RSD parity bug.
#
# The QC Checker tab and the Result Explorer tab must report the *same*
# per-metabolite RSD values and the *same* pass count. Historically they
# diverged because:
#   1. QC Checker read `dataSource == "concentration"` (pre-correction)
#      while Result Explorer preferred `"concentration[statTarget]"`
#      (post-correction).
#   2. Result Explorer counted "pass" as RSD below the warn threshold,
#      which excluded the "warning" band; qcCheckR's histogram used a
#      single RSD < 30% cut-off.
#   3. Result Explorer silently fell back to computing RSD over *all*
#      samples when it could not identify QC samples.
#
# These tests exercise the pure helpers (`get_qc_rsd_values` and
# `mstargetr_results_rsd_core`) extracted into
# `inst/shiny/MStargetR_app/R/helpers.R` so both tabs share one code path.

# ---- Load shiny-app helpers (they are not exported from the package) -------
locate_shiny_helpers <- function() {
  # 1. Source tree first (takes precedence so in-repo edits are picked up
  #    even when a previous version of the package is already installed).
  candidates <- c(
    file.path(getwd(), "..", "..", "inst", "shiny", "MStargetR_app", "R",
              "helpers.R"),
    file.path(getwd(), "inst", "shiny", "MStargetR_app", "R", "helpers.R")
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(normalizePath(cand, mustWork = FALSE))
  }

  # 2. Fall back to the installed package copy.
  p <- tryCatch(
    system.file("shiny", "MStargetR_app", "R", "helpers.R",
                package = "MStargetR"),
    error = function(e) ""
  )
  if (nzchar(p) && file.exists(p)) return(p)

  ""
}

shiny_helpers_path <- locate_shiny_helpers()
if (nzchar(shiny_helpers_path) && file.exists(shiny_helpers_path)) {
  source(shiny_helpers_path, local = TRUE)
}

# ---- Fixtures --------------------------------------------------------------

# Matches the specification in REVIEW_REPORT.md (lines 102-118): a fixture
# with three metabolites where only `M_good` has QC RSD below 30%.
rsd_parity_fixture <- function() {
  # Seeded so the sample values are reproducible; the QC triples are fixed.
  set.seed(20260422)
  tibble::tibble(
    sample_name = paste0("S", 1:10),
    sample_type = c(rep("sample", 7), rep("qc", 3)),
    M_good = c(stats::rnorm(7, 100, 40), 100, 101, 99),   # QC RSD ~ 1%
    M_bad  = c(stats::rnorm(7, 100, 10), 100, 150, 50),   # QC RSD ~ 50%
    M_edge = c(stats::rnorm(7, 100, 20), 100, 130, 70)    # QC RSD ~ 30%
  )
}

# Build a minimal qcCheckR-shaped result object with just the fields the
# RSD helpers read: `$filters$rsd` is a data.frame with a `dataSource`
# column, a `dataBatch` column, and one column per metabolite containing
# the pre-computed QC RSD (as a percentage).
fake_qc_result <- function(df,
                            stages = c("concentration",
                                       "concentration[statTarget]",
                                       "peakArea")) {
  qc <- df[df$sample_type == "qc", , drop = FALSE]
  met_cols <- setdiff(names(df), c("sample_name", "sample_type"))

  rsd_row <- vapply(met_cols, function(m) {
    v <- qc[[m]]
    v <- v[!is.na(v)]
    if (length(v) < 2 || mean(v) == 0) return(NA_real_)
    stats::sd(v) / abs(mean(v)) * 100
  }, numeric(1))

  # Same RSD values for every stage in this fixture so either tab's pick
  # should produce identical numbers.
  rsd_tbl <- do.call(rbind, lapply(stages, function(src) {
    row <- as.data.frame(as.list(rsd_row), stringsAsFactors = FALSE)
    names(row) <- names(rsd_row)
    cbind(
      data.frame(dataSource = src, dataBatch = "allBatches",
                 stringsAsFactors = FALSE),
      row
    )
  }))

  list(filters = list(rsd = rsd_tbl))
}

# ---- Tests ----------------------------------------------------------------

test_that("shiny app RSD helpers are loaded from helpers.R", {
  expect_true(exists("get_qc_rsd_values", mode = "function"),
              info = "Shiny helpers.R could not be sourced in this environment.")
  expect_true(exists("mstargetr_results_rsd_core", mode = "function"),
              info = "Shiny helpers.R could not be sourced in this environment.")
})

test_that("get_qc_rsd_values returns the qcCheckR RSD row unchanged", {
  # Shiny helpers must have been loaded by the setup test above. Fail
  # loudly here rather than silently skipping: a missing helpers.R means
  # RSD parity is entirely unverified.
  expect_true(exists("get_qc_rsd_values", mode = "function"),
              info = "Shiny helpers.R must be sourced for RSD parity tests.")
  df  <- rsd_parity_fixture()
  qcr <- fake_qc_result(df)

  vals <- get_qc_rsd_values(qcr, stage = "concentration")
  expect_true(!is.null(vals))
  expect_named(vals, c("M_good", "M_bad", "M_edge"))
  expect_lt(vals[["M_good"]], 5)   # ~1%
  expect_gt(vals[["M_bad"]], 40)   # ~50%
})

test_that("QC Checker and Result Explorer report identical metabolite RSDs", {
  expect_true(exists("get_qc_rsd_values", mode = "function"),
              info = "Shiny helpers.R must be sourced for RSD parity tests.")
  df  <- rsd_parity_fixture()
  qcr <- fake_qc_result(df)

  # QC Checker reads the "concentration" row directly.
  qc_rsd_qccheck  <- get_qc_rsd_values(qcr, stage = "concentration")
  # Result Explorer iterates stages; with identical values both tabs
  # should produce exactly the same vector.
  qc_rsd_explorer <- mstargetr_results_rsd_core(qcr)

  # Align names before comparing — qcCheckR returns a named vector in an
  # arbitrary column order.
  common <- intersect(names(qc_rsd_qccheck), names(qc_rsd_explorer))
  expect_equal(
    unname(qc_rsd_qccheck[common]),
    unname(qc_rsd_explorer[common]),
    tolerance = 1e-9
  )
})

test_that("sum(RSD < 30) matches the QC Checker pass count (1 for fixture)", {
  expect_true(exists("mstargetr_results_rsd_core", mode = "function"),
              info = "Shiny helpers.R must be sourced for RSD parity tests.")
  df  <- rsd_parity_fixture()
  qcr <- fake_qc_result(df)

  qc_rsd_explorer <- mstargetr_results_rsd_core(qcr)
  # Only M_good is strictly below 30% in the fixture.
  expect_equal(sum(qc_rsd_explorer < 30, na.rm = TRUE), 1L)
})

test_that("mstargetr_results_rsd_core prefers the post-correction stage", {
  expect_true(exists("mstargetr_results_rsd_core", mode = "function"),
              info = "Shiny helpers.R must be sourced for RSD parity tests.")
  df  <- rsd_parity_fixture()

  # Build a fake result where the statTarget row carries different (halved)
  # RSDs from the raw concentration row, so we can prove the Explorer picks
  # the statTarget stage first.
  qcr <- fake_qc_result(df)
  st_row <- qcr$filters$rsd[qcr$filters$rsd$dataSource ==
                              "concentration[statTarget]", , drop = FALSE]
  met_cols <- setdiff(names(st_row), c("dataSource", "dataBatch"))
  st_row[1, met_cols] <- st_row[1, met_cols] / 2
  qcr$filters$rsd[qcr$filters$rsd$dataSource ==
                    "concentration[statTarget]", ] <- st_row

  picked <- mstargetr_results_rsd_core(qcr)
  raw    <- get_qc_rsd_values(qcr, stage = "concentration")
  expect_equal(
    unname(picked[names(raw)]),
    unname(raw / 2),
    tolerance = 1e-9
  )
})

test_that("get_qc_rsd_values returns NULL when the stage is missing", {
  expect_true(exists("get_qc_rsd_values", mode = "function"),
              info = "Shiny helpers.R must be sourced for RSD parity tests.")
  df  <- rsd_parity_fixture()
  qcr <- fake_qc_result(df, stages = "concentration")  # only one stage

  expect_null(get_qc_rsd_values(qcr, stage = "concentration[statTarget]"))
  expect_null(get_qc_rsd_values(qcr, stage = "peakArea"))
})
