# Tests for mst_with_logging (R/config.R)

test_that("mst_with_logging writes message() text to the plate log file", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  mst_with_logging("plateX", tmp, message("hello"))

  log_file <- file.path(tmp, "MStargetR_logs", "plateX_MStargetR_log.txt")
  expect_true(file.exists(log_file),
              info = "log file should be created")
  content <- paste(readLines(log_file), collapse = "\n")
  expect_true(grepl("hello", content, fixed = TRUE),
              info = "log file should contain the message text")
})

test_that("mst_with_logging returns the value of the expression", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- mst_with_logging("p", tmp, 1 + 1)
  expect_equal(result, 2)
})

test_that("mst_with_logging creates the log directory when absent", {
  tmp <- tempfile()
  # do NOT pre-create tmp
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  mst_with_logging("plateY", tmp, message("dir creation test"))

  log_dir <- file.path(tmp, "MStargetR_logs")
  expect_true(dir.exists(log_dir),
              info = "MStargetR_logs/ should be created automatically")
})

test_that("mst_with_logging still emits messages to the console", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # expect_message captures the propagated message (split = TRUE keeps console)
  expect_message(
    mst_with_logging("plateZ", tmp, message("console echo test")),
    "console echo test"
  )
})
