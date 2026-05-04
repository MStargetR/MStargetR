# Test User exported wrapper function
library(mockery)
library(withr)
library(tibble)

# Tests for PeakForgeR User function ----
test_that("PeakForgeR handles valid inputs and sets plateIDs correctly", {
  # Create a clean test directory with only excluded subdirs
  base_temp <- tempdir()
  test_project_dir <- file.path(base_temp, "peakforger_test_project")
  if (dir.exists(test_project_dir)) unlink(test_project_dir, recursive = TRUE)
  dir.create(test_project_dir, showWarnings = FALSE)

  # Create only excluded-name subdirectory so plateIDs comes out empty
  dir.create(file.path(test_project_dir, "raw_data"), showWarnings = FALSE)

  # Create mzml directories for each plate (needed by dir.exists check)
  dir.create(file.path(test_project_dir, "PLATE_1", "data", "mzml"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(test_project_dir, "PLATE_2", "data", "mzml"), recursive = TRUE, showWarnings = FALSE)

  # Mock dependencies
  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "PeakForgeR_setup_project", function(...) list())
  stub(PeakForgeR, "import_mzml", function(...) list())
  stub(PeakForgeR, "peak_picking", function(...) list())
  stub(PeakForgeR, "log_error", function(msg) message(msg))
  stub(PeakForgeR, "archive_raw_files", function(project_directory) message("Archived"))

  # Mock list.files: return mzml files for plate dirs, empty for project dir
  # (so plateIDs discovery returns empty and plateID_outputs branch is used)
  stub(PeakForgeR, "list.files", function(path, ...) {
    if (grepl("mzml$", path)) {
      return(c("sample.mzML"))
    }
    return(character(0))
  })

  # Capture messages and check for the expected one
  msgs <- character(0)
  suppressWarnings(withCallingHandlers(
    PeakForgeR(
      user_name = "TestUser",
      project_directory = test_project_dir,
      mrm_template_list = list("template1.tsv"),
      QC_sample_label = "LTR",
      plateID_outputs = c("PLATE_1", "PLATE_2")
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  ))
  expect_true(any(grepl("Valid plateID_outputs provided", msgs)))
})

test_that("PeakForgeR handles invalid plateID_outputs appropriately", {
  suppressMessages({
  # Setup test directory with only excluded-name subdirs
  base_temp <- tempdir()
  test_project_dir <- file.path(base_temp, "peakforger_test_project_invalid")
  if (dir.exists(test_project_dir)) unlink(test_project_dir, recursive = TRUE)
  dir.create(test_project_dir, showWarnings = FALSE)

  # Create only excluded-name subdirectory so plateIDs comes out empty
  dir.create(file.path(test_project_dir, "raw_data"), showWarnings = FALSE)

  # Mock dependencies
  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "PeakForgeR_setup_project", function(...) list())
  stub(PeakForgeR, "import_mzml", function(...) list())
  stub(PeakForgeR, "peak_picking", function(...) list())
  stub(PeakForgeR, "log_error", function(msg) message(msg))
  stub(PeakForgeR, "archive_raw_files", function(project_directory) message("Archived"))

  # Run with invalid plateID_outputs (no mzml files exist for INVALID_PLATE)
  expect_error(suppressMessages(
    PeakForgeR(
      user_name = "TestUser",
      project_directory = test_project_dir,
      mrm_template_list = list("template1.tsv"),
      QC_sample_label = "LTR",
      plateID_outputs = c("INVALID_PLATE")
    )
  ), regexp = "Zero mzML files for plateID|mzML directory not found")
  })
})








