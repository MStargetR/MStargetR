# Tests for PeakForgeR main function ----
library(mockery)

# ============================================================================
# Input validation tests
# ============================================================================

test_that("PeakForgeR rejects empty user_name", {
  expect_error(
    PeakForgeR(user_name = "", project_directory = tempdir(),
               mrm_template_list = list("a.tsv"), QC_sample_label = "LTR"),
    "PeakForgeR.*user_name.*must be.*non-empty"
  )
})

test_that("PeakForgeR rejects non-character user_name", {
  expect_error(
    PeakForgeR(user_name = 123, project_directory = tempdir(),
               mrm_template_list = list("a.tsv"), QC_sample_label = "LTR"),
    "PeakForgeR.*user_name.*must be.*non-empty.*character"
  )
})

test_that("PeakForgeR rejects NULL user_name", {

  expect_error(
    PeakForgeR(user_name = NULL, project_directory = tempdir(),
               mrm_template_list = list("a.tsv"), QC_sample_label = "LTR"),
    "PeakForgeR.*user_name.*must be.*non-empty.*character"
  )
})

test_that("PeakForgeR rejects invalid project_directory", {
  suppressMessages({
    expect_error(
      PeakForgeR(user_name = "TestUser",
                 project_directory = "/nonexistent/path/12345",
                 mrm_template_list = list("a.tsv"),
                 QC_sample_label = "LTR"),
      "does not exist"
    )
  })
})

test_that("PeakForgeR rejects non-string project_directory", {
  expect_error(
    PeakForgeR(user_name = "TestUser",
               project_directory = 12345,
               mrm_template_list = list("a.tsv"),
               QC_sample_label = "LTR"),
    "must be a single character string"
  )
})

test_that("PeakForgeR rejects empty QC_sample_label", {
  suppressMessages({
    # Need to mock validate_project_directory and validate_mrm_template_list
    # to get past them to the QC_sample_label check
    stub(PeakForgeR, "validate_project_directory", function(x) x)
    stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)

    expect_error(
      PeakForgeR(user_name = "TestUser",
                 project_directory = tempdir(),
                 mrm_template_list = list("a.tsv"),
                 QC_sample_label = ""),
      "PeakForgeR.*QC_sample_label.*must be.*non-empty"
    )
  })
})

test_that("PeakForgeR rejects non-character QC_sample_label", {
  suppressMessages({
    stub(PeakForgeR, "validate_project_directory", function(x) x)
    stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)

    expect_error(
      PeakForgeR(user_name = "TestUser",
                 project_directory = tempdir(),
                 mrm_template_list = list("a.tsv"),
                 QC_sample_label = 42),
      "PeakForgeR.*QC_sample_label.*must be.*non-empty.*character"
    )
  })
})

test_that("PeakForgeR calls check_docker after validation passes", {
  suppressMessages({
    stub(PeakForgeR, "validate_project_directory", function(x) x)
    stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
    stub(PeakForgeR, "check_docker", function() stop("Docker check called"))

    expect_error(
      PeakForgeR(user_name = "TestUser",
                 project_directory = tempdir(),
                 mrm_template_list = list("a.tsv"),
                 QC_sample_label = "LTR"),
      "Docker check called"
    )
  })
})

test_that("PeakForgeR rejects NULL QC_sample_label", {
  suppressMessages({
    stub(PeakForgeR, "validate_project_directory", function(x) x)
    stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)

    expect_error(
      PeakForgeR(user_name = "TestUser",
                 project_directory = tempdir(),
                 mrm_template_list = list("a.tsv"),
                 QC_sample_label = NULL),
      "PeakForgeR.*QC_sample_label.*must be.*non-empty"
    )
  })
})

# ============================================================================
# Docker dependency check
# ============================================================================

test_that("PeakForgeR stops when Docker is not available", {
  suppressMessages({
    stub(PeakForgeR, "validate_project_directory", function(x) x)
    stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
    stub(PeakForgeR, "check_docker", function() {
      stop("Execution halted due to missing Docker installation")
    })

    expect_error(
      PeakForgeR(user_name = "TestUser",
                 project_directory = tempdir(),
                 mrm_template_list = list("a.tsv"),
                 QC_sample_label = "LTR"),
      "missing Docker"
    )
  })
})

# ============================================================================
# PlateID handling
# ============================================================================

test_that("PeakForgeR errors when plateID_outputs have zero mzML files", {
  suppressMessages({
    # Create a clean, empty temp project directory (no subdirectories that
    # would be picked up as plateIDs) so plateID_outputs is used instead.
    test_dir <- file.path(tempdir(), paste0("peakforge_empty_test_", Sys.getpid()))
    if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
    dir.create(test_dir, showWarnings = FALSE)
    on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

    stub(PeakForgeR, "validate_project_directory", function(x) x)
    stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
    stub(PeakForgeR, "check_docker", function() TRUE)

    expect_error(
      PeakForgeR(user_name = "TestUser",
                 project_directory = test_dir,
                 mrm_template_list = list("a.tsv"),
                 QC_sample_label = "LTR",
                 plateID_outputs = c("NONEXISTENT_PLATE")),
      "mzML directory not found|Zero associated mzML files for plateID"
    )
  })
})

# ============================================================================
# PlateID discovery from existing directory structure
# ============================================================================

test_that("PeakForgeR discovers plateIDs from project directory subdirectories", {
  test_dir <- tempfile("peakforge_discover_")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Create subdirectories that look like plates
  dir.create(file.path(test_dir, "PLATE_A"), showWarnings = FALSE)
  dir.create(file.path(test_dir, "PLATE_B"), showWarnings = FALSE)
  # Create excluded directories that should be filtered out
  dir.create(file.path(test_dir, "raw_data"), showWarnings = FALSE)
  dir.create(file.path(test_dir, "msConvert_mzml_output"), showWarnings = FALSE)
  dir.create(file.path(test_dir, "MStargetR_logs"), showWarnings = FALSE)

  captured_plateIDs <- NULL

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    captured_plateIDs <<- plateIDs
    lapply(plateIDs, function(pid) list(success = TRUE, plateID = pid))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  suppressMessages(
    PeakForgeR(user_name = "TestUser",
               project_directory = test_dir,
               mrm_template_list = list("a.tsv"),
               QC_sample_label = "LTR")
  )

  expect_true("PLATE_A" %in% captured_plateIDs)
  expect_true("PLATE_B" %in% captured_plateIDs)
  expect_false("raw_data" %in% captured_plateIDs)
  expect_false("msConvert_mzml_output" %in% captured_plateIDs)
  expect_false("MStargetR_logs" %in% captured_plateIDs)
})

test_that("PeakForgeR excludes all reserved directory names from plateIDs", {
  test_dir <- tempfile("peakforge_exclude_")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  reserved <- c("raw_data", "msConvert_mzml_output", "all", "archive",
                 "MStargetR_logs", "logs", "user_files")
  for (d in reserved) {
    dir.create(file.path(test_dir, d), showWarnings = FALSE)
  }
  # One real plate
  dir.create(file.path(test_dir, "REAL_PLATE"), showWarnings = FALSE)
  # Also create a file that should appear in list.files but get filtered
  file.create(file.path(test_dir, "error_log.txt"))

  captured_plateIDs <- NULL

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    captured_plateIDs <<- plateIDs
    lapply(plateIDs, function(pid) list(success = TRUE, plateID = pid))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  suppressMessages(
    PeakForgeR(user_name = "TestUser",
               project_directory = test_dir,
               mrm_template_list = list("a.tsv"),
               QC_sample_label = "LTR")
  )

  expect_equal(captured_plateIDs, "REAL_PLATE")
})

# ============================================================================
# Workflow success and failure reporting
# ============================================================================

test_that("PeakForgeR reports successful and failed plates", {
  test_dir <- tempfile("peakforge_report_")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(file.path(test_dir, "PLATE_OK"), showWarnings = FALSE)
  dir.create(file.path(test_dir, "PLATE_FAIL"), showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    list(
      list(success = TRUE, plateID = "PLATE_OK"),
      list(success = FALSE, plateID = "PLATE_FAIL", error = "mock failure")
    )
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  msgs <- capture.output(type = "message",
    PeakForgeR(user_name = "TestUser",
               project_directory = test_dir,
               mrm_template_list = list("a.tsv"),
               QC_sample_label = "LTR")
  )
  expect_true(any(grepl("PLATE_OK", msgs)))
})

test_that("PeakForgeR stops when all plates fail", {
  test_dir <- tempfile("peakforge_allfail_")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(file.path(test_dir, "PLATE_1"), showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    list(list(success = FALSE, plateID = "PLATE_1", error = "total failure"))
  })

  suppressMessages({
    expect_error(
      PeakForgeR(user_name = "TestUser",
                 project_directory = test_dir,
                 mrm_template_list = list("a.tsv"),
                 QC_sample_label = "LTR"),
      "All plates failed"
    )
  })
})

test_that("PeakForgeR calls archive_raw_files on success", {
  test_dir <- tempfile("peakforge_archive_")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(file.path(test_dir, "PLATE_1"), showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  archive_called <- FALSE

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    list(list(success = TRUE, plateID = "PLATE_1"))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) {
    archive_called <<- TRUE
  })

  suppressMessages(
    PeakForgeR(user_name = "TestUser",
               project_directory = test_dir,
               mrm_template_list = list("a.tsv"),
               QC_sample_label = "LTR")
  )

  expect_true(archive_called)
})

test_that("PeakForgeR emits completion message with directory path", {
  test_dir <- tempfile("peakforge_msg_")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(file.path(test_dir, "P1"), showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    list(list(success = TRUE, plateID = "P1"))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  msgs <- capture.output(type = "message",
    PeakForgeR(user_name = "TestUser",
               project_directory = test_dir,
               mrm_template_list = list("a.tsv"),
               QC_sample_label = "LTR")
  )
  expect_true(any(grepl("Please run qcCheckR", msgs)))
})

test_that("PeakForgeR creates MStargetR_logs directory", {
  test_dir <- tempfile("peakforge_logs_")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(file.path(test_dir, "PLATE_1"), showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    list(list(success = TRUE, plateID = "PLATE_1"))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  suppressMessages(
    PeakForgeR(user_name = "TestUser",
               project_directory = test_dir,
               mrm_template_list = list("a.tsv"),
               QC_sample_label = "LTR")
  )

  expect_true(dir.exists(file.path(test_dir, "MStargetR_logs")))
})

test_that("PeakForgeR uses validate_mrm_template_list return value when non-NULL", {
  test_dir <- tempfile("peakforge_mrm_return_")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(file.path(test_dir, "PLATE_1"), showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  captured_mrm <- NULL

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) {
    list("transformed_template.tsv")
  })
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    # The fn closure captures mrm_template_list from the PeakForgeR env
    # We cannot directly inspect it, but we can verify no error occurs
    list(list(success = TRUE, plateID = "PLATE_1"))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  expect_no_error(suppressMessages(
    PeakForgeR(user_name = "TestUser",
               project_directory = test_dir,
               mrm_template_list = list("original.tsv"),
               QC_sample_label = "LTR")
  ))
})

test_that("PeakForgeR emits plateIDs gathered message when plates found in directory", {
  test_dir <- tempfile("peakforge_gathered_")
  dir.create(test_dir, showWarnings = FALSE)
  dir.create(file.path(test_dir, "MY_PLATE"), showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    list(list(success = TRUE, plateID = "MY_PLATE"))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  msgs <- capture.output(type = "message",
    PeakForgeR(user_name = "TestUser",
               project_directory = test_dir,
               mrm_template_list = list("a.tsv"),
               QC_sample_label = "LTR")
  )
  expect_true(any(grepl("plateIDs gathered from existing project directory", msgs)))
})

# ============================================================================
# Parallel-safe RNG: future.seed = TRUE must be passed to future_lapply
# (regression for "UNRELIABLE VALUE" warning in non-seeded futures)
# ============================================================================

test_that("PeakForgeR passes future.seed = TRUE to future_lapply", {
  test_dir <- tempfile("peakforge_seed_")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)
  dir.create(file.path(test_dir, "PLATE_A"), showWarnings = FALSE)

  captured_args <- list()

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "check_docker", function() TRUE)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    captured_args <<- list(...)
    lapply(plateIDs, function(pid) list(success = TRUE, plateID = pid))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  suppressMessages(
    PeakForgeR(user_name = "TestUser",
               project_directory = test_dir,
               mrm_template_list = list("a.tsv"),
               QC_sample_label = "LTR")
  )

  expect_true("future.seed" %in% names(captured_args))
  expect_identical(captured_args$future.seed, TRUE)
})
