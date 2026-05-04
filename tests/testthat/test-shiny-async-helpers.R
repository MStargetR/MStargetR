# Tests for the async-pipeline helpers in
# inst/shiny/MStargetR_app/R/helpers.R:
#   mst_spawn_pkg_fn()
#   mst_tail_log()
#   mst_poll_pipeline()
#   mst_cleanup_pipeline()
#
# These power SH-010 / SH-013 (qcCheckR / batchCorrectR run in a background
# R process so Cancel can actually kill the worker and the Shiny main
# thread stays responsive). A broken helper here would silently put the
# whole app back into the pre-audit synchronous behaviour.

helpers_path <- system.file("shiny", "MStargetR_app", "R", "helpers.R",
                             package = "MStargetR")
if (!nzchar(helpers_path)) {
  helpers_path <- file.path("inst", "shiny", "MStargetR_app", "R", "helpers.R")
}
if (file.exists(helpers_path)) source(helpers_path, local = TRUE)

# ---------------------------------------------------------------------------
# mst_tail_log: no file -> empty + unchanged offset
# ---------------------------------------------------------------------------
test_that("mst_tail_log returns empty result when file is missing", {
  skip_if(!exists("mst_tail_log", mode = "function"))
  res <- mst_tail_log(tempfile(), offset = 42L)
  expect_equal(res$text, "")
  expect_equal(res$new_offset, 42L)
})

# ---------------------------------------------------------------------------
# mst_tail_log: reads bytes appended since offset, advances offset
# ---------------------------------------------------------------------------
test_that("mst_tail_log reads new bytes and advances offset", {
  skip_if(!exists("mst_tail_log", mode = "function"))
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  cat("hello", file = f)
  res1 <- mst_tail_log(f, offset = 0L)
  expect_equal(res1$text, "hello")
  expect_equal(res1$new_offset, 5L)

  # Second call with the advanced offset should see no new text.
  res2 <- mst_tail_log(f, offset = res1$new_offset)
  expect_equal(res2$text, "")
  expect_equal(res2$new_offset, 5L)

  # Append more and tail again.
  cat(" world", file = f, append = TRUE)
  res3 <- mst_tail_log(f, offset = res2$new_offset)
  expect_equal(res3$text, " world")
  expect_equal(res3$new_offset, 11L)
})

# ---------------------------------------------------------------------------
# mst_spawn_pkg_fn / mst_poll_pipeline: end-to-end smoke test
# ---------------------------------------------------------------------------
# We monkey-patch `mst_spawn_pkg_fn` to stand in a bare `callr::r_bg` call
# running a tiny expression that doesn't require MStargetR — otherwise the
# subprocess would need the installed package, which the test environment
# may not have. This still exercises the poll/tail/result plumbing.
test_that("mst_poll_pipeline harvests result and drains log", {
  skip_if_not_installed("callr")
  skip_if(!exists("mst_poll_pipeline", mode = "function"))
  log_file <- tempfile("mst_async_", fileext = ".log")
  file.create(log_file)
  on.exit(unlink(log_file), add = TRUE)

  handle <- callr::r_bg(
    func = function(log_file) {
      con <- file(log_file, open = "a")
      sink(con, type = "output")
      sink(con, type = "message")
      on.exit({
        try(sink(NULL, type = "message"), silent = TRUE)
        try(sink(NULL, type = "output"), silent = TRUE)
        try(close(con), silent = TRUE)
      }, add = TRUE)
      message("step-1")
      Sys.sleep(0.1)
      message("step-2")
      42L
    },
    args = list(log_file = log_file)
  )

  # Wait for completion, tailing the log every 100 ms the way the real
  # Shiny poller would.
  deadline <- Sys.time() + 10
  offset <- 0L
  all_text <- ""
  last_tick <- NULL
  while (Sys.time() < deadline) {
    tick <- mst_poll_pipeline(handle, log_file, offset)
    offset <- tick$new_offset
    all_text <- paste0(all_text, tick$log_text)
    last_tick <- tick
    if (tick$done) break
    Sys.sleep(0.1)
  }

  expect_true(!is.null(last_tick) && isTRUE(last_tick$done),
              info = "subprocess did not exit within 10s deadline")
  expect_true(isTRUE(last_tick$result$success),
              info = "harvested result should succeed")
  expect_equal(last_tick$result$value, 42L)
  expect_true(grepl("step-1", all_text, fixed = TRUE))
  expect_true(grepl("step-2", all_text, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# mst_cleanup_pipeline: kills a live subprocess and deletes the log file
# ---------------------------------------------------------------------------
test_that("mst_cleanup_pipeline kills worker and removes log", {
  skip_if_not_installed("callr")
  skip_if(!exists("mst_cleanup_pipeline", mode = "function"))
  log_file <- tempfile("mst_async_", fileext = ".log")
  file.create(log_file)

  handle <- callr::r_bg(
    func = function() { Sys.sleep(60); TRUE },
    args = list()
  )
  expect_true(handle$is_alive())

  mst_cleanup_pipeline(handle, log_file)

  # Give the OS a moment to reap the process.
  for (i in 1:20) {
    if (!handle$is_alive()) break
    Sys.sleep(0.1)
  }
  expect_false(handle$is_alive(),
               info = "cleanup should have killed the subprocess")
  expect_false(file.exists(log_file),
               info = "cleanup should have removed the log file")
})

# ---------------------------------------------------------------------------
# mst_poll_pipeline: subprocess error is routed into result$success = FALSE
# ---------------------------------------------------------------------------
test_that("mst_poll_pipeline reports subprocess errors as failure", {
  skip_if_not_installed("callr")
  skip_if(!exists("mst_poll_pipeline", mode = "function"))
  log_file <- tempfile("mst_async_", fileext = ".log")
  file.create(log_file)
  on.exit(unlink(log_file), add = TRUE)

  handle <- callr::r_bg(
    func = function() stop("boom from subprocess"),
    args = list()
  )
  deadline <- Sys.time() + 10
  while (Sys.time() < deadline && handle$is_alive()) Sys.sleep(0.05)

  tick <- mst_poll_pipeline(handle, log_file, 0L)
  expect_true(tick$done)
  expect_false(isTRUE(tick$result$success))
  expect_true(grepl("boom", tick$result$message %||% ""))
})
