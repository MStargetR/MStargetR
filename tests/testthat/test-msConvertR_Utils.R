# Tests for msConvertR_Utils functions ----
library(mockery)

# ============================================================================
# msConvertR_execute_command tests
# ============================================================================

test_that("msConvertR_execute_command emits message with plate IDs", {
  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 4)
  idx_env <- new.env(parent = emptyenv()); idx_env$i <- 0L
  pids <- c("plate1", "plate2")
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    idx_env$i <- idx_env$i + 1L
    structure(list(pid = pids[idx_env$i]), class = "MockFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(f, ...) {
    list(plateID = f$pid, success = TRUE)
  })
  stub(msConvertR_execute_command, "dir.create", function(...) NULL)

  temp <- withr::local_tempdir()
  dir.create(file.path(temp, "plate1", "data", "mzml"), recursive = TRUE)
  dir.create(file.path(temp, "plate2", "data", "mzml"), recursive = TRUE)
  file.create(file.path(temp, "plate1", "data", "mzml", "plate1.mzML"))
  file.create(file.path(temp, "plate2", "data", "mzml", "plate2.mzML"))

  # commands must be a list of list(docker_args = character, saneID = char)
  # per R/msConvertR_Utils.R:249 — bare strings trigger "$ operator is
  # invalid for atomic vectors" in the iteration at L269/L329.
  expect_message(
    msConvertR_execute_command(
      commands = list(
        list(docker_args = c("run", "--rm", "test1"), saneID = "plate1"),
        list(docker_args = c("run", "--rm", "test2"), saneID = "plate2")
      ),
      output_directory = temp,
      plateIDs = c("plate1", "plate2")
    ),
    "Converting vendor files"
  )
})

test_that("msConvertR_execute_command creates logs directory", {
  temp <- withr::local_tempdir()
  logs_dir <- file.path(temp, "MStargetR_logs")

  dir_create_calls <- character(0)

  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 4)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(pid = "plate1"), class = "MockFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(f, ...) {
    list(plateID = f$pid, success = TRUE)
  })
  stub(msConvertR_execute_command, "dir.create", function(path, ...) {
    dir_create_calls <<- c(dir_create_calls, path)
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  })

  dir.create(file.path(temp, "plate1", "data", "mzml"), recursive = TRUE)
  file.create(file.path(temp, "plate1", "data", "mzml", "plate1.mzML"))

  suppressMessages(
    msConvertR_execute_command(
      commands = list(list(docker_args = c("run", "--rm", "cmd1"),
                            saneID = "plate1")),
      output_directory = temp,
      plateIDs = c("plate1")
    )
  )

  expect_true(any(grepl("MStargetR_logs", dir_create_calls)))
})

test_that("msConvertR_execute_command runs without error and produces messages", {
  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 2)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(pid = "plate1"), class = "MockFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(f, ...) {
    list(plateID = f$pid, success = TRUE)
  })
  stub(msConvertR_execute_command, "dir.create", function(...) NULL)
  stub(msConvertR_execute_command, "getOption", function(name) NULL)
  stub(msConvertR_execute_command, "options", function(...) NULL)

  temp <- withr::local_tempdir()
  dir.create(file.path(temp, "plate1", "data", "mzml"), recursive = TRUE)
  file.create(file.path(temp, "plate1", "data", "mzml", "plate1.mzML"))

  expect_message(
    msConvertR_execute_command(
      commands = list(list(docker_args = c("run", "--rm", "cmd1"),
                            saneID = "plate1")),
      output_directory = temp,
      plateIDs = c("plate1")
    ),
    "Converting vendor files"
  )
})

# ============================================================================
# msConvertR_construct_command_for_terminal tests
# ============================================================================

test_that("msConvertR_construct_command_for_terminal builds docker commands", {
  temp <- withr::local_tempdir()
  raw_dir <- file.path(temp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  # Create mock vendor files
  file.create(file.path(raw_dir, "plate1.wiff"))
  file.create(file.path(raw_dir, "plate1.wiff.scan"))

  output_dir <- file.path(temp, "output")
  dir.create(file.path(output_dir, "plate1", "data", "mzml"), recursive = TRUE)

  result <- msConvertR_construct_command_for_terminal(temp, output_dir, c("plate1"))

  expect_type(result, "list")
  expect_length(result, 1)
  cmd_str <- paste(result[[1]]$docker_args, collapse = " ")
  expect_true(grepl("run", cmd_str))
  expect_true(grepl("msconvert", cmd_str))
})

test_that("msConvertR_construct_command_for_terminal excludes .scan files", {
  temp <- withr::local_tempdir()
  raw_dir <- file.path(temp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  file.create(file.path(raw_dir, "plate1.wiff"))
  file.create(file.path(raw_dir, "plate1.wiff.scan"))

  output_dir <- file.path(temp, "output")
  dir.create(file.path(output_dir, "plate1", "data", "mzml"), recursive = TRUE)

  result <- msConvertR_construct_command_for_terminal(temp, output_dir, c("plate1"))

  cmd_str <- paste(result[[1]]$docker_args, collapse = " ")
  expect_false(grepl("\\.scan", cmd_str))
})

test_that("msConvertR_construct_command_for_terminal handles multiple plates", {
  temp <- withr::local_tempdir()
  raw_dir <- file.path(temp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  file.create(file.path(raw_dir, "plateA.wiff"))
  file.create(file.path(raw_dir, "plateB.wiff"))

  output_dir <- file.path(temp, "output")
  dir.create(file.path(output_dir, "plateA", "data", "mzml"), recursive = TRUE)
  dir.create(file.path(output_dir, "plateB", "data", "mzml"), recursive = TRUE)

  result <- msConvertR_construct_command_for_terminal(temp, output_dir, c("plateA", "plateB"))

  expect_length(result, 2)
  cmd_str_a <- paste(result[[1]]$docker_args, collapse = " ")
  cmd_str_b <- paste(result[[2]]$docker_args, collapse = " ")
  expect_true(grepl("plateA", cmd_str_a))
  expect_true(grepl("plateB", cmd_str_b))
})

# ============================================================================
# msConvertR_set_working_directory tests
# ============================================================================

test_that("msConvertR_set_working_directory sets wd to output_directory", {
  temp <- withr::local_tempdir()

  setwd_calls <- character(0)
  stub(msConvertR_set_working_directory, "setwd", function(dir) {
    setwd_calls <<- c(setwd_calls, dir)
  })

  msConvertR_set_working_directory(temp)

  # The first setwd call should be to the requested output directory
  expect_equal(setwd_calls[1], temp)
})

test_that("msConvertR_set_working_directory calls setwd on the target directory", {
  # The function delegates to setwd(); the caller (msConvertR_mzml_conversion)
  # is responsible for saving/restoring the working directory via on.exit().
  temp <- withr::local_tempdir()

  setwd_target <- NULL
  stub(msConvertR_set_working_directory, "setwd", function(dir) {
    setwd_target <<- dir
  })

  msConvertR_set_working_directory(temp)

  expect_equal(setwd_target, temp)
})

# ============================================================================
# validate_input_directory tests
# ============================================================================

test_that("validate_input_directory accepts valid directory with message", {
  temp <- withr::local_tempdir()

  expect_message(
    validate_input_directory(temp),
    "Accessing project directory"
  )
})

test_that("validate_input_directory rejects non-string input", {
  expect_error(
    validate_input_directory(123),
    "must be a single character string"
  )
  expect_error(
    validate_input_directory(c("dir1", "dir2")),
    "must be a single character string"
  )
})

test_that("validate_input_directory rejects empty string", {
  expect_error(
    validate_input_directory(""),
    "must not be an empty string"
  )
})

test_that("validate_input_directory rejects non-existent directory", {
  fake_dir <- file.path(tempdir(), "surely_nonexistent_dir_12345")

  expect_error(
    validate_input_directory(fake_dir),
    "input directory does not exist"
  )
})

# ============================================================================
# msConvertR_setup_project_directories tests
# ============================================================================

test_that("msConvertR_setup_project_directories creates expected structure", {
  temp <- withr::local_tempdir()

  msConvertR_setup_project_directories(temp, c("plate1"))

  expect_true(dir.exists(file.path(temp, "user_files")))
  expect_true(dir.exists(file.path(temp, "plate1", "data", "mzml")))
  expect_true(dir.exists(file.path(temp, "plate1", "data", "rda")))
  expect_true(dir.exists(file.path(temp, "plate1", "data", "PeakForgeR")))
  expect_true(dir.exists(file.path(temp, "plate1", "data", "raw_data")))
  expect_true(dir.exists(file.path(temp, "plate1", "data", "batch_correction")))
  expect_true(dir.exists(file.path(temp, "plate1", "html_report")))
})

test_that("msConvertR_setup_project_directories handles multiple plates", {
  temp <- withr::local_tempdir()

  msConvertR_setup_project_directories(temp, c("plateA", "plateB"))

  expect_true(dir.exists(file.path(temp, "plateA", "data", "mzml")))
  expect_true(dir.exists(file.path(temp, "plateB", "data", "mzml")))
})

test_that("msConvertR_setup_project_directories is idempotent", {
  temp <- withr::local_tempdir()

  suppressMessages(msConvertR_setup_project_directories(temp, c("plate1")))
  expect_no_error(suppressMessages(msConvertR_setup_project_directories(temp, c("plate1"))))

  expect_true(dir.exists(file.path(temp, "plate1", "data", "mzml")))
})
