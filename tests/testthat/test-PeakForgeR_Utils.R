# Tests for PeakForgeR_Utils functions ----
library(mockery)

# ============================================================================
# validate_mzR_parameters tests
# ============================================================================

test_that("validate_mzR_parameters accepts valid inputs silently", {
  valid_mzR <- list(plate1 = list(sample1 = list()))
  valid_guide <- data.frame(col1 = 1, col2 = 2)
  valid_qc <- "LTR"

  expect_silent(validate_mzR_parameters(valid_mzR, valid_guide, valid_qc))
})

test_that("validate_mzR_parameters rejects non-list FUNC_mzR", {
  expect_error(
    validate_mzR_parameters("not_a_list", data.frame(), "LTR"),
    "FUNC_mzR.*must be a list"
  )
  expect_error(
    validate_mzR_parameters(123, data.frame(), "LTR"),
    "FUNC_mzR.*must be a list"
  )
})

test_that("validate_mzR_parameters rejects non-data.frame FUNC_mrm_guide", {
  expect_error(
    validate_mzR_parameters(list(), "not_a_df", "LTR"),
    "FUNC_mrm_guide.*must be a data.frame"
  )
  expect_error(
    validate_mzR_parameters(list(), list(), "LTR"),
    "FUNC_mrm_guide.*must be a data.frame"
  )
})

test_that("validate_mzR_parameters rejects non-character FUNC_OPTION_qc_type", {
  expect_error(
    validate_mzR_parameters(list(), data.frame(), 123),
    "FUNC_OPTION_qc_type.*must be a single character string"
  )
})

test_that("validate_mzR_parameters rejects multi-length FUNC_OPTION_qc_type", {
  expect_error(
    validate_mzR_parameters(list(), data.frame(), c("LTR", "PQC")),
    "FUNC_OPTION_qc_type.*must be a single character string"
  )
})

# ============================================================================
# get_mzML_filelist tests
# ============================================================================

test_that("get_mzML_filelist extracts names from single plate", {
  mock_mzR <- list(
    plate1 = list(
      "sample_001.mzML" = list(),
      "sample_002.mzML" = list()
    )
  )

  result <- get_mzML_filelist(mock_mzR)

  expect_equal(result, c("sample_001.mzML", "sample_002.mzML"))
})

test_that("get_mzML_filelist extracts names from multiple plates", {
  mock_mzR <- list(
    plate1 = list(
      "sample_A.mzML" = list(),
      "sample_B.mzML" = list()
    ),
    plate2 = list(
      "sample_C.mzML" = list()
    )
  )

  result <- get_mzML_filelist(mock_mzR)

  expect_equal(result, c("sample_A.mzML", "sample_B.mzML", "sample_C.mzML"))
})

test_that("get_mzML_filelist returns NULL for empty input", {
  mock_mzR <- list()
  result <- get_mzML_filelist(mock_mzR)
  expect_null(result)
})

test_that("get_mzML_filelist handles plate with no samples", {
  mock_mzR <- list(plate1 = list())
  result <- get_mzML_filelist(mock_mzR)
  expect_null(result)
})

# ============================================================================
# filter_mzML_filelist_qc tests
# ============================================================================

test_that("filter_mzML_filelist_qc filters for QC samples correctly", {
  filelist <- c("LTR_001.mzML", "SAMPLE_001.mzML", "LTR_002.mzML", "BLANK_001.mzML")
  result <- filter_mzML_filelist_qc(filelist, "LTR")
  expect_equal(result, c("LTR_001.mzML", "LTR_002.mzML"))
})

test_that("filter_mzML_filelist_qc returns empty when no matches", {
  filelist <- c("SAMPLE_001.mzML", "BLANK_001.mzML")
  expect_warning(
    result <- filter_mzML_filelist_qc(filelist, "LTR"),
    "No QC files found"
  )
  expect_length(result, 0)
})

test_that("filter_mzML_filelist_qc matches case-insensitively", {
  filelist <- c("ltr_001.mzML", "LTR_001.mzML")
  result <- filter_mzML_filelist_qc(filelist, "LTR")
  expect_equal(sort(result), sort(c("ltr_001.mzML", "LTR_001.mzML")))
})

# ============================================================================
# set_project_details tests
# ============================================================================

test_that("set_project_details populates master_list correctly", {
  master_list <- list(
    project_details = list()
  )

  result <- set_project_details(
    master_list,
    user_name = "John Smith",
    project_directory = "/path/to/project",
    plateID = "plate_001",
    QC_sample_label = "LTR"
  )

  expect_equal(result$project_details$project_dir, "/path/to/project")
  expect_true(is.character(result$project_details$PeakForgeR_version))
  expect_true(nchar(result$project_details$PeakForgeR_version) > 0)
  expect_equal(result$project_details$user_name, "John Smith")
  expect_equal(result$project_details$project_name, "project")
  expect_equal(result$project_details$plateID, "plate_001")
  expect_equal(result$project_details$qc_type, "LTR")
})

test_that("set_project_details extracts project_name from directory path", {
  master_list <- list(project_details = list())

  result <- set_project_details(
    master_list, "user", "/some/deep/path/MyProject", "plate1", "PQC"
  )

  expect_equal(result$project_details$project_name, "MyProject")
})

test_that("set_project_details sets start_time as POSIXct", {
  master_list <- list(project_details = list())

  result <- set_project_details(
    master_list, "user", "/path/to/project", "plate1", "LTR"
  )

  expect_s3_class(result$project_details$script_log$timestamps$start_time, "POSIXct")
})

test_that("set_project_details preserves existing master_list entries", {
  master_list <- list(
    project_details = list(),
    data = list(existing = "data"),
    templates = list(mrm_guides = list())
  )

  result <- set_project_details(
    master_list, "user", "/path", "plate1", "LTR"
  )

  expect_equal(result$data$existing, "data")
  expect_true("mrm_guides" %in% names(result$templates))
})

# ============================================================================
# validate_directories tests
# ============================================================================

test_that("validate_directories sends message for non-existent source", {
  temp <- withr::local_tempdir()
  fake_source <- file.path(temp, "nonexistent_source")
  dest <- file.path(temp, "dest_dir")

  expect_message(
    validate_directories(fake_source, dest),
    "Source directory does not exist"
  )
})

test_that("validate_directories creates destination directory when missing", {
  temp <- withr::local_tempdir()
  source_dir <- file.path(temp, "source")
  dir.create(source_dir)
  dest_dir <- file.path(temp, "new_destination")

  expect_false(dir.exists(dest_dir))
  suppressMessages(validate_directories(source_dir, dest_dir))
  expect_true(dir.exists(dest_dir))
})

test_that("validate_directories succeeds silently when both dirs exist", {
  temp <- withr::local_tempdir()
  source_dir <- file.path(temp, "source")
  dest_dir <- file.path(temp, "dest")
  dir.create(source_dir)
  dir.create(dest_dir)

  expect_silent(validate_directories(source_dir, dest_dir))
})

test_that("validate_directories creates nested destination directories", {
  temp <- withr::local_tempdir()
  source_dir <- file.path(temp, "source")
  dir.create(source_dir)
  dest_dir <- file.path(temp, "a", "b", "c", "deep_dest")

  expect_false(dir.exists(dest_dir))
  suppressMessages(validate_directories(source_dir, dest_dir))
  expect_true(dir.exists(dest_dir))
})

# ============================================================================
# execute_PeakForgeR_command tests
# ============================================================================

test_that("execute_PeakForgeR_command returns a docker command string", {
  temp <- withr::local_tempdir()
  plate_dir <- file.path(temp, "plate1", "data")
  dir.create(plate_dir, recursive = TRUE)

  master_list <- list(
    project_details = list(
      project_dir = temp
    )
  )

  result <- execute_PeakForgeR_command(master_list, "plate1")

  expect_type(result, "character")
  # Result is now a docker argument vector
  expect_true(length(result) > 1)
  expect_true("run" %in% result)
})

test_that("execute_PeakForgeR_command includes the plate_idx in file paths", {
  temp <- withr::local_tempdir()
  plate_dir <- file.path(temp, "test_plate", "data")
  dir.create(plate_dir, recursive = TRUE)

  master_list <- list(
    project_details = list(
      project_dir = temp
    )
  )

  result <- execute_PeakForgeR_command(master_list, "test_plate")
  result_str <- paste(result, collapse = " ")

  expect_true(grepl("test_plate", result_str))
})

test_that("execute_PeakForgeR_command includes SkylineCmd in the command", {
  temp <- withr::local_tempdir()
  plate_dir <- file.path(temp, "plate1", "data")
  dir.create(plate_dir, recursive = TRUE)

  master_list <- list(
    project_details = list(
      project_dir = temp
    )
  )

  result <- execute_PeakForgeR_command(master_list, "plate1")

  expect_true("SkylineCmd" %in% result)
  expect_true("wine" %in% result)
})

test_that("execute_PeakForgeR_command includes expected flags", {
  temp <- withr::local_tempdir()
  plate_dir <- file.path(temp, "plate1", "data")
  dir.create(plate_dir, recursive = TRUE)

  master_list <- list(
    project_details = list(
      project_dir = temp
    )
  )

  result <- execute_PeakForgeR_command(master_list, "plate1")
  result_str <- paste(result, collapse = " ")

  expect_true(grepl("--import-transition-list", result_str))
  expect_true(grepl("--import-all", result_str))
  expect_true(grepl("--import-peak-boundaries", result_str))
  expect_true(grepl("--report-format=csv", result_str))
  expect_true(grepl("--chromatogram-file", result_str))
})

# DOCK-C6: pin --security-opt seccomp=unconfined for the Wine-based pwiz image.
# SkylineCmd runs under Wine and the default Docker seccomp profile blocks the
# socket syscalls Wine needs, producing "wine: socket : Function not implemented"
# on a fresh machine. See R/PeakForgeR_docker.R DOCK-C6 comment.
test_that("execute_PeakForgeR_command includes --security-opt seccomp=unconfined (DOCK-C6)", {
  temp <- withr::local_tempdir()
  plate_dir <- file.path(temp, "plate1", "data")
  dir.create(plate_dir, recursive = TRUE)

  master_list <- list(
    project_details = list(
      project_dir = temp
    )
  )

  result <- execute_PeakForgeR_command(master_list, "plate1")

  expect_true("--security-opt" %in% result)
  expect_true("seccomp=unconfined" %in% result)
  idx <- which(result == "--security-opt")
  expect_length(idx, 1)
  expect_identical(result[idx + 1L], "seccomp=unconfined")
})

test_that("execute_PeakForgeR_command includes current date in filenames", {
  temp <- withr::local_tempdir()
  plate_dir <- file.path(temp, "plate1", "data")
  dir.create(plate_dir, recursive = TRUE)

  master_list <- list(
    project_details = list(
      project_dir = temp
    )
  )

  d <- Sys.Date()
  result <- execute_PeakForgeR_command(master_list, "plate1")
  result_str <- paste(result, collapse = " ")

  expect_true(grepl(as.character(d), result_str))
})

# ============================================================================
# run_system_command tests
# ============================================================================

test_that("run_system_command calls system2 with docker args vector", {
  captured_command <- NULL
  captured_args <- NULL

  stub(run_system_command, "system2", function(command, args, ...) {
    captured_command <<- command
    captured_args <<- args
    return("mock output")
  })

  temp <- withr::local_tempdir()
  output_file <- file.path(temp, "output.txt")

  # Test with argument vector (new docker args pattern)
  suppressMessages(
    run_system_command(c("run", "--rm", "image", "wine", "SkylineCmd"), output_file)
  )

  expect_equal(captured_command, "docker")
  expect_true("run" %in% captured_args)
  expect_true("SkylineCmd" %in% captured_args)
})

test_that("run_system_command writes output to file when provided", {
  stub(run_system_command, "system2", function(command, args, ...) {
    return(c("line1", "line2"))
  })

  temp <- withr::local_tempdir()
  output_file <- file.path(temp, "output.txt")

  suppressMessages(
    run_system_command(c("run", "--rm", "image"), output_file)
  )

  expect_true(file.exists(output_file))
  content <- readLines(output_file)
  # Log now includes the full command and timestamped sections
  expect_true(any(grepl("Full Docker command", content)))
  expect_true(any(grepl("Skyline CMD output start", content)))
  expect_true(any(grepl("line1", content)))
  expect_true(any(grepl("line2", content)))
})

test_that("run_system_command handles system errors gracefully", {
  stub(run_system_command, "system2", function(command, args, ...) {
    stop("Command not found")
  })

  temp <- withr::local_tempdir()
  output_file <- file.path(temp, "output.txt")

  expect_warning(
    expect_error(
      suppressMessages(run_system_command("bad_command", output_file)),
      "Skyline command failed to execute"
    ),
    "single-string command is deprecated"
  )
})

test_that("run_system_command returns result on success", {
  stub(run_system_command, "system2", function(command, args, ...) {
    return(c("output_line_1", "output_line_2"))
  })

  temp <- withr::local_tempdir()
  output_file <- file.path(temp, "output.txt")

  expect_warning(
    suppressMessages({
      result <- run_system_command("echo test", output_file)
    }),
    "single-string command is deprecated"
  )

  expect_equal(result, c("output_line_1", "output_line_2"))
})

test_that("run_system_command errors when result is NULL", {
  stub(run_system_command, "system2", function(command, args, ...) {
    stop("failed")
  })

  expect_warning(
    expect_error(
      suppressMessages(run_system_command("bad_cmd", NULL)),
      "Skyline command failed to execute"
    ),
    "single-string command is deprecated"
  )
})

# ============================================================================
# initialise_master_list tests
# ============================================================================

test_that("initialise_master_list returns list with expected structure", {
  result <- initialise_master_list()

  expect_type(result, "list")
  expect_true("environment" %in% names(result))
  expect_true("templates" %in% names(result))
  expect_true("project_details" %in% names(result))
  expect_true("data" %in% names(result))
  expect_true("summary_tables" %in% names(result))
  expect_true("process_lists" %in% names(result))
})

test_that("initialise_master_list creates empty nested structures", {
  result <- initialise_master_list()

  expect_type(result$environment$user_functions, "list")
  expect_length(result$environment$user_functions, 0)
  expect_type(result$templates$mrm_guides, "list")
  expect_length(result$templates$mrm_guides, 0)
})

# ============================================================================
# setup_project_directories tests
# ============================================================================

test_that("setup_project_directories creates expected directory structure", {
  temp <- withr::local_tempdir()

  master_list <- list(
    project_details = list(
      project_dir = temp,
      plateID = "test_plate"
    )
  )

  setup_project_directories(master_list)

  base <- file.path(temp, "test_plate")
  expect_true(dir.exists(base))
  expect_true(dir.exists(file.path(base, "data")))
  expect_true(dir.exists(file.path(base, "data", "mzml")))
  expect_true(dir.exists(file.path(base, "data", "rda")))
  expect_true(dir.exists(file.path(base, "data", "PeakForgeR")))
  expect_true(dir.exists(file.path(base, "data", "raw_data")))
  expect_true(dir.exists(file.path(base, "data", "batch_correction")))
  expect_true(dir.exists(file.path(base, "html_report")))
})

test_that("setup_project_directories handles multiple plates", {
  temp <- withr::local_tempdir()

  master_list <- list(
    project_details = list(
      project_dir = temp,
      plateID = c("plate_A", "plate_B")
    )
  )

  setup_project_directories(master_list)

  expect_true(dir.exists(file.path(temp, "plate_A", "data", "mzml")))
  expect_true(dir.exists(file.path(temp, "plate_B", "data", "mzml")))
})

test_that("setup_project_directories does not error if dirs already exist", {
  temp <- withr::local_tempdir()
  dir.create(file.path(temp, "plate1", "data", "mzml"), recursive = TRUE)

  master_list <- list(
    project_details = list(
      project_dir = temp,
      plateID = "plate1"
    )
  )

  expect_silent(setup_project_directories(master_list))
})
