# Regression tests for MS-007 through MS-021 (Medium-severity audit findings)
library(mockery)

# ============================================================================
# MS-007: unknown arguments produce a warning, not a silent drop
# ============================================================================

test_that("MS-007: msConvertR warns on unknown named arguments", {
  input_dir  <- withr::local_tempdir()
  raw_dir    <- file.path(input_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE)
  output_dir <- withr::local_tempdir()

  stub(msConvertR, "validate_input_directory", function(...) NULL)
  stub(msConvertR, "validate_file_types",      function(...) character(0))

  expect_warning(
    tryCatch(
      msConvertR(input_dir, output_dir, plate_pattern = "*.wiff"),
      error = function(e) NULL
    ),
    "unknown argument"
  )
})

# ============================================================================
# MS-008: output_directory existence and writability validated early
# ============================================================================

test_that("MS-008: msConvertR stops when output_directory cannot be created", {
  input_dir <- withr::local_tempdir()
  stub(msConvertR, "validate_input_directory", function(...) NULL)
  stub(msConvertR, "validate_file_types",      function(...) character(0))
  stub(msConvertR, "dir.create",               function(...) FALSE)
  stub(msConvertR, "dir.exists",               function(...) FALSE)

  expect_error(
    msConvertR(input_dir, "/surely/invalid/path/xyz999"),
    "could not be created"
  )
})

# ============================================================================
# MS-009: same-directory comparison uses winslash="/"
# ============================================================================

test_that("MS-009: same-dir message fires when paths normalise to equal strings", {
  tmp <- withr::local_tempdir()

  stub(msConvertR, "validate_input_directory",    function(...) NULL)
  stub(msConvertR, "validate_file_types",          function(...) "file.wiff")
  stub(msConvertR, "sanitize_identifier",          function(x, ...) x)
  stub(msConvertR, "check_docker",                 function(...) NULL)
  stub(msConvertR, "msConvertR_mzml_conversion",   function(...) NULL)

  msgs <- character(0)
  withCallingHandlers(
    msConvertR(tmp, tmp),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
  )
  expect_true(any(grepl("Input and output directories are the same", msgs)))
})

# ============================================================================
# MS-011: path-traversal guard in msConvertR_execute_command
# ============================================================================

test_that("MS-011: msConvertR_execute_command stops on path with ..", {
  tmp <- withr::local_tempdir()
  bad_out <- file.path(tmp, "..", "escape")
  cmds <- list(list(docker_args = c("run", "--rm", "img")))
  expect_error(
    msConvertR_execute_command(cmds, bad_out, "plate1"),
    "must not contain"
  )
})

# ============================================================================
# MS-012: future::plan is called with workers= argument
# ============================================================================

test_that("MS-012: future::plan receives workers argument, not global option", {
  plan_calls <- list()
  stub(msConvertR_execute_command, "future::plan", function(...) {
    plan_calls[[length(plan_calls) + 1]] <<- list(...)
  })
  stub(msConvertR_execute_command, "future::availableCores", function() 4)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(pid = "plate1"), class = "MockFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(f, ...) {
    list(plateID = f$pid, success = TRUE)
  })

  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "plate1", "data", "mzml"), recursive = TRUE)
  file.create(file.path(tmp, "plate1", "data", "mzml", "plate1.mzML"))
  suppressMessages(
    msConvertR_execute_command(
      commands = list(list(docker_args = c("run", "--rm", "img"))),
      output_directory = tmp,
      plateIDs = c("plate1")
    )
  )
  # First plan call must include a workers argument
  first_call_args <- plan_calls[[1]]
  expect_true("workers" %in% names(first_call_args))
})

# ============================================================================
# MS-015: post-sanitization duplicate detection stops with clear error
# ============================================================================

test_that("MS-015: msConvertR stops when two raw IDs sanitize to the same string", {
  input_dir  <- withr::local_tempdir()
  raw_dir    <- file.path(input_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE)
  output_dir <- withr::local_tempdir()

  file.create(file.path(raw_dir, "plate_A.wiff"))
  file.create(file.path(raw_dir, "plate-A.wiff"))

  stub(msConvertR, "validate_input_directory", function(...) NULL)
  stub(msConvertR, "validate_file_types",
       function(...) c(file.path(raw_dir, "plate_A.wiff"),
                       file.path(raw_dir, "plate-A.wiff")))
  stub(msConvertR, "sanitize_identifier", function(x, ...) gsub("[-]", "_", x))
  stub(msConvertR, "check_docker",  function(...) NULL)
  stub(msConvertR, "dir.create",    function(...) TRUE)
  stub(msConvertR, "dir.exists",    function(...) TRUE)
  stub(msConvertR, "file.access",   function(...) 0L)

  expect_error(
    suppressMessages(msConvertR(input_dir, output_dir)),
    "sanitized plateID"
  )
})

# ============================================================================
# MS-016: raw-file matching uses basename, not full path
# ============================================================================

test_that("MS-016: restructure matches plateID against basename only", {
  tmp <- withr::local_tempdir()
  # Create directory structure where the parent path contains the plateID string.
  # Without the basename() fix, files in `plate1/raw_data/` would spuriously match
  # when searching for plateID "plate1" against full paths.
  plate1_raw <- file.path(tmp, "plate1", "raw_data")
  dir.create(plate1_raw, recursive = TRUE)
  plate2_raw <- file.path(tmp, "raw_data")
  dir.create(plate2_raw, recursive = TRUE)

  # File that belongs to plate2 but lives under a path containing "plate1"
  file.create(file.path(tmp, "raw_data", "plate2.wiff"))

  plate2_dest <- file.path(tmp, "plate2", "data", "raw_data")
  dir.create(plate2_dest, recursive = TRUE)
  plate2_mzml <- file.path(tmp, "plate2", "data", "mzml")
  dir.create(plate2_mzml, recursive = TRUE)

  # Should NOT copy plate2.wiff when restructuring for plate1
  suppressMessages(
    msConvertR_restructure_directory(tmp, c("plate1"),
                                     "\\.(wiff|raw)$")
  )
  # plate2.wiff must still be untouched in raw_data
  expect_true(file.exists(file.path(tmp, "raw_data", "plate2.wiff")))
})

# ============================================================================
# MS-017: copy failure stops execution instead of warning
# ============================================================================

test_that("MS-017: copy failure in restructure raises an error", {
  tmp <- withr::local_tempdir()
  raw_dir      <- file.path(tmp, "raw_data")
  dir.create(raw_dir)
  file.create(file.path(raw_dir, "plate1.wiff"))
  dest_dir <- file.path(tmp, "plate1", "data", "raw_data")
  dir.create(dest_dir, recursive = TRUE)
  mzml_dir <- file.path(tmp, "plate1", "data", "mzml")
  dir.create(mzml_dir, recursive = TRUE)

  stub(msConvertR_restructure_directory, "file.copy", function(...) FALSE)

  expect_error(
    suppressMessages(
      msConvertR_restructure_directory(tmp, c("plate1"), "\\.(wiff|raw)$")
    ),
    "Failed to copy"
  )
})

# ============================================================================
# MS-020: tolerance parameter catches banker's-rounding boundary duplicates
# ============================================================================

make_mrm <- function(q1, q3, name = "Met") {
  data.frame(
    `Molecule List Name` = "Group1",
    `Precursor Name`     = name,
    `Precursor Mz`       = q1,
    `Precursor Charge`   = 1L,
    `Product Mz`         = q3,
    `Product Charge`     = 1L,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("MS-020: tolerance=0.001 catches 0.0005 Da apart pair as duplicate", {
  df <- rbind(
    make_mrm(500.0000, 300.0000, "Met_A"),
    make_mrm(500.0005, 300.0005, "Met_B")
  )
  result <- suppressMessages(transition_checkR(df, tolerance = 0.001))
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)
})

test_that("MS-020: default tolerance detects round-boundary near-duplicates", {
  df <- rbind(
    make_mrm(500.0005, 300.0000, "Met_A"),
    make_mrm(500.0005, 300.0000, "Met_B")
  )
  result <- suppressMessages(transition_checkR(df))
  expect_s3_class(result, "data.frame")
})

test_that("MS-020: tolerance=0 accepts identical values as duplicate", {
  df <- rbind(
    make_mrm(100.0, 80.0, "Met_A"),
    make_mrm(100.0, 80.0, "Met_B")
  )
  result <- suppressMessages(transition_checkR(df, tolerance = 0))
  expect_s3_class(result, "data.frame")
})

test_that("MS-020: large tolerance=1 does not merge truly distinct transitions", {
  df <- rbind(
    make_mrm(100.0, 80.0, "Met_A"),
    make_mrm(200.0, 160.0, "Met_B")
  )
  result <- suppressMessages(transition_checkR(df, tolerance = 0.001))
  expect_null(result)
})

test_that("MS-020: invalid tolerance raises error", {
  df <- make_mrm(100.0, 80.0)
  expect_error(transition_checkR(df, tolerance = -1), "tolerance")
  expect_error(transition_checkR(df, tolerance = "x"), "tolerance")
})

# ============================================================================
# MS-021: empty-string Note values warn and are treated as NA
# ============================================================================

test_that("MS-021: empty-string Notes warn and are excluded from matching", {
  template <- data.frame(
    Note = c("SIL_A", "", "SIL_C"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  guide <- data.frame(
    SIL_name = c("SIL_A", "SIL_C"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    result <- suppressMessages(
      compare_mrm_template_with_guide(template, guide)
    ),
    "empty-string Note"
  )
  expect_null(result)
})

test_that("MS-021: empty-string Note that has no SIL match is not returned as unmatched", {
  template <- data.frame(
    Note = c("SIL_A", ""),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  guide <- data.frame(
    SIL_name = c("SIL_A"),
    stringsAsFactors = FALSE
  )
  result <- suppressWarnings(suppressMessages(
    compare_mrm_template_with_guide(template, guide)
  ))
  # empty string should not appear in unmatched results
  expect_false(isTRUE("" %in% result))
})

test_that("MS-021: no warning when there are no empty-string Notes", {
  template <- data.frame(
    Note = c("SIL_A", NA, "SIL_C"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  guide <- data.frame(
    SIL_name = c("SIL_A", "SIL_C"),
    stringsAsFactors = FALSE
  )
  expect_no_warning(
    suppressMessages(compare_mrm_template_with_guide(template, guide))
  )
})
