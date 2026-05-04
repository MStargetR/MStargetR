# Additional coverage tests for msConvertR_Utils.R ----
# Targets zero-coverage lines not exercised by the existing test suite.
library(mockery)

# ============================================================================
# msConvertR_construct_command_for_terminal -- no match / multiple matches
# (lines 178-180)
# ============================================================================

test_that("msConvertR_construct_command_for_terminal errors when no file matches plateID", {
  temp <- withr::local_tempdir()
  raw_dir <- file.path(temp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  # Create a file that does NOT match the plateID
  file.create(file.path(raw_dir, "other_plate.wiff"))

  output_dir <- file.path(temp, "output")
  dir.create(file.path(output_dir, "plate1", "data", "mzml"), recursive = TRUE)

  expect_error(
    suppressMessages(
      msConvertR_construct_command_for_terminal(temp, output_dir, c("plate1"))
    ),
    "Expected exactly one file matching plateID"
  )
})

test_that("msConvertR_construct_command_for_terminal errors when multiple files match plateID", {
  temp <- withr::local_tempdir()
  raw_dir <- file.path(temp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  # Create two non-.scan files matching the plateID
  file.create(file.path(raw_dir, "plate1.wiff"))
  file.create(file.path(raw_dir, "plate1.raw"))

  output_dir <- file.path(temp, "output")
  dir.create(file.path(output_dir, "plate1", "data", "mzml"), recursive = TRUE)

  expect_error(
    suppressMessages(
      msConvertR_construct_command_for_terminal(temp, output_dir, c("plate1"))
    ),
    "Expected exactly one file matching plateID"
  )
})

# ============================================================================
# msConvertR_execute_command -- failure path (lines 234-254, 268-275)
# ============================================================================

test_that("msConvertR_execute_command stops when a plate conversion fails", {
  temp <- withr::local_tempdir()

  # future::value now returns list(plateID, success) per future (see
  # R/msConvertR_Utils.R:361-363). Stubs must mirror that shape or the
  # result-collector hits `res$plateID` on an atomic.
  future_vals <- list(list(plateID = "plate1", success = FALSE))
  call_idx <- 0L

  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 4)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(), class = "Future")
  })
  stub(msConvertR_execute_command, "future::value", function(...) {
    call_idx <<- call_idx + 1L
    future_vals[[call_idx]]
  })
  stub(msConvertR_execute_command, "dir.create", function(...) NULL)

  expect_error(
    suppressMessages(
      msConvertR_execute_command(
        commands = list(list(docker_args = c("run", "test1"))),
        output_directory = temp,
        plateIDs = c("plate1")
      )
    ),
    "failed conversion"
  )
})

test_that("msConvertR_execute_command reports multiple failed plates", {
  temp <- withr::local_tempdir()

  future_vals <- list(
    list(plateID = "plateA", success = FALSE),
    list(plateID = "plateB", success = FALSE)
  )
  call_idx <- 0L

  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 4)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(), class = "Future")
  })
  stub(msConvertR_execute_command, "future::value", function(...) {
    call_idx <<- call_idx + 1L
    future_vals[[call_idx]]
  })
  stub(msConvertR_execute_command, "dir.create", function(...) NULL)

  expect_error(
    suppressMessages(
      msConvertR_execute_command(
        commands = list(
          list(docker_args = c("run", "test1")),
          list(docker_args = c("run", "test2"))
        ),
        output_directory = temp,
        plateIDs = c("plateA", "plateB")
      )
    ),
    "2 of 2 plate\\(s\\) failed"
  )
})

# ============================================================================
# msConvertR_restructure_directory -- failed file copy warnings
# (lines 328-332, 346-349, 365-369)
# ============================================================================

test_that("msConvertR_restructure_directory stops when raw file copy fails", {
  # MS-017 hardened copy-failure handling from warning() to stop() so
  # partially-restructured output doesn't look successful. The test was
  # originally authored pre-remediation with expect_warning(); now it
  # asserts the loud failure path.
  temp <- withr::local_tempdir()

  raw_dir <- file.path(temp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)
  plate_data <- file.path(temp, "plate1", "data")
  dir.create(file.path(plate_data, "raw_data"), recursive = TRUE)
  dir.create(file.path(plate_data, "mzml"), recursive = TRUE)
  mzml_out <- file.path(temp, "msConvert_mzml_output")
  dir.create(mzml_out, recursive = TRUE)

  file.create(file.path(raw_dir, "plate1.wiff"))

  stub(msConvertR_restructure_directory, "file.copy", function(...) FALSE)

  expect_error(
    suppressMessages(
      msConvertR_restructure_directory(temp, c("plate1"), "\\.wiff$")
    ),
    "Failed to copy"
  )
})

test_that("msConvertR_restructure_directory warns when .d directory copy fails", {
  temp <- withr::local_tempdir()

  raw_dir <- file.path(temp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)
  plate_data <- file.path(temp, "plate1", "data")
  dir.create(file.path(plate_data, "raw_data"), recursive = TRUE)
  dir.create(file.path(plate_data, "mzml"), recursive = TRUE)
  mzml_out <- file.path(temp, "msConvert_mzml_output")
  dir.create(mzml_out, recursive = TRUE)

  # Create a .d directory matching the plateID
  d_dir <- file.path(raw_dir, "plate1.d")
  dir.create(d_dir, recursive = TRUE)
  file.create(file.path(d_dir, "data.bin"))

  # Stub file.copy to always return FALSE
  stub(msConvertR_restructure_directory, "file.copy", function(...) FALSE)

  # MS-017 escalated copy failures from warning() to stop(); the assertion
  # now checks the error path.
  expect_error(
    suppressMessages(
      msConvertR_restructure_directory(temp, c("plate1"), "\\.d$")
    ),
    "Failed to copy"
  )
})

test_that("msConvertR_restructure_directory warns when mzML file copy fails", {
  temp <- withr::local_tempdir()

  raw_dir <- file.path(temp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)
  plate_data <- file.path(temp, "plate1", "data")
  dir.create(file.path(plate_data, "raw_data"), recursive = TRUE)
  dir.create(file.path(plate_data, "mzml"), recursive = TRUE)
  mzml_out <- file.path(temp, "msConvert_mzml_output")
  dir.create(mzml_out, recursive = TRUE)

  # Create an mzML file matching the plateID (not COND/BLANK/ISTDs)
  file.create(file.path(mzml_out, "plate1_sample1.mzML"))

  # Stub file.copy to return FALSE
  stub(msConvertR_restructure_directory, "file.copy", function(...) FALSE)

  # MS-017 escalated copy failures from warning() to stop().
  expect_error(
    suppressMessages(
      msConvertR_restructure_directory(temp, c("plate1"), "\\.wiff$")
    ),
    "Failed to copy"
  )
})

# ============================================================================
# msConvertR_execute_command -- low core count branch (line 226)
# ============================================================================

test_that("msConvertR_execute_command handles low core count (<=2)", {
  temp <- withr::local_tempdir()
  # Post-run verification (R/msConvertR_Utils.R:384+) scans
  # <output>/<plateID>/data/mzml for a non-empty .mzML; create one so the
  # verifier's happy-path fires.
  mzml_dir <- file.path(temp, "plate1", "data", "mzml")
  dir.create(mzml_dir, recursive = TRUE)
  writeLines("<mzML/>", file.path(mzml_dir, "plate1.mzML"))

  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 2)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(), class = "Future")
  })
  stub(msConvertR_execute_command, "future::value",
       function(...) list(plateID = "plate1", success = TRUE))

  result <- suppressMessages(
    msConvertR_execute_command(
      commands = list(list(docker_args = c("run", "test"),
                            saneID = "plate1")),
      output_directory = temp,
      plateIDs = c("plate1")
    )
  )

  expect_true(result[["plate1"]])
})

# ============================================================================
# msConvertR_execute_command -- future body lines 234-254 (success + failure)
# ============================================================================

test_that("msConvertR_execute_command runs docker in future (lines 234-254)", {
  # The future body runs in a separate R process and cannot be mocked.
  # Test that the function creates the log directory and handles results.
  temp <- withr::local_tempdir()
  mzml_dir <- file.path(temp, "plate1", "data", "mzml")
  dir.create(mzml_dir, recursive = TRUE)
  writeLines("<mzML/>", file.path(mzml_dir, "plate1.mzML"))

  # future::value must now return list(plateID, success); the future body
  # (R/msConvertR_Utils.R:351-363) wraps its return in that shape so the
  # collector can print per-plate status.
  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 2)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(value = list(plateID = "plate1", success = TRUE)),
              class = "SequentialFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(x, ...) x$value)

  result <- suppressMessages(
    msConvertR_execute_command(
      commands = list(list(docker_args = c("run", "--rm", "image"),
                            saneID = "plate1")),
      output_directory = temp,
      plateIDs = c("plate1")
    )
  )

  expect_true(result[["plate1"]])
  # Log directory should be created
  expect_true(dir.exists(file.path(temp, "MStargetR_logs")))
})

test_that("msConvertR_execute_command handles failed conversion (lines 240-254)", {
  temp <- withr::local_tempdir()

  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 2)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(value = list(plateID = "plate1", success = FALSE)),
              class = "SequentialFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(x, ...) x$value)

  expect_error(
    suppressMessages(
      msConvertR_execute_command(
        commands = list(list(docker_args = c("run", "--rm", "image"))),
        output_directory = temp,
        plateIDs = c("plate1")
      )
    ),
    "failed conversion"
  )
})
