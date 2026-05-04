# Tests for Config & package support audit High findings

library(mockery)

# ============================================================================
# CF-001: check_docker() uses timeout on system2 calls
# ============================================================================

test_that("check_docker passes timeout to system2 for version check", {
  calls <- list()
  stub(check_docker, "system2", function(cmd, args, ...) {
    dots <- list(...)
    calls[[length(calls) + 1L]] <<- dots
    stop("not found")
  })
  suppressMessages(try(check_docker(), silent = TRUE))
  # At least one call should carry a timeout argument
  timeouts <- vapply(calls, function(x) !is.null(x$timeout), logical(1))
  expect_true(any(timeouts))
})

test_that("check_docker passes timeout to system2 for daemon check", {
  call_count <- 0L
  timeout_values <- numeric(0)
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    dots <- list(...)
    if (!is.null(dots$timeout)) timeout_values <<- c(timeout_values, dots$timeout)
    if (call_count == 1L) return("Docker version 24.0.0")
    stop("daemon hung")
  })
  suppressMessages(try(check_docker(), silent = TRUE))
  expect_true(length(timeout_values) > 0)
  expect_true(all(timeout_values > 0))
})

# ============================================================================
# CF-001: daemon detection relies solely on exit status (no locale grep)
# ============================================================================

test_that("check_docker treats exit status 0 as daemon running regardless of text", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) return("Docker version 24.0.0")
    if (call_count == 2L) {
      # Exit 0 but output contains "error" text — should NOT be treated as failure
      result <- "error: something printed but exit is 0"
      attr(result, "status") <- 0L
      return(result)
    }
    # image check — return empty (not found)
    return(character(0))
  })
  # With auto_pull=FALSE the function should not stop at the daemon step
  expect_no_error(suppressMessages(check_docker(auto_pull = FALSE)))
})

test_that("check_docker treats non-zero exit status as daemon not running", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) return("Docker version 24.0.0")
    if (call_count == 2L) {
      result <- character(0)
      attr(result, "status") <- 1L
      return(result)
    }
    return("")
  })
  expect_error(check_docker(), "daemon is not running")
})

# ============================================================================
# CF-002: check_docker() auto_pull gate
# ============================================================================

test_that("check_docker with auto_pull=FALSE emits message and returns without pulling", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) return("Docker version 24.0.0")
    if (call_count == 2L) return("Server: Docker Engine")
    # images -q returns empty (image not found)
    return(character(0))
  })
  expect_message(check_docker(auto_pull = FALSE), "not found locally")
  # pull should NOT have been called (only 3 system2 calls total)
  expect_equal(call_count, 3L)
})

test_that("check_docker with auto_pull=TRUE pulls when image missing", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) return("Docker version 24.0.0")
    if (call_count == 2L) return("Server: Docker Engine")
    if (call_count == 3L) return(character(0))  # image not found
    if (call_count == 4L) {
      result <- "Pull complete"
      attr(result, "status") <- 0L
      return(result)
    }
    return("")
  })
  expect_message(check_docker(auto_pull = TRUE), "Pulling ProteoWizard Docker image")
  expect_equal(call_count, 4L)
})

test_that("check_docker default auto_pull equals interactive()", {
  # Verify the formal default is interactive()
  formals_val <- formals(check_docker)$auto_pull
  expect_equal(deparse(formals_val), "interactive()")
})

# ============================================================================
# CF-004/CF-005: remotes is in Imports so BiocManager can delegate GitHub slugs
# ============================================================================

test_that("remotes is listed in DESCRIPTION Imports", {
  desc_path <- system.file("DESCRIPTION", package = "MStargetR")
  if (!nzchar(desc_path)) {
    # Fallback: read from source tree during dev
    desc_path <- file.path(
      dirname(dirname(dirname(testthat::test_path()))), "DESCRIPTION"
    )
  }
  if (file.exists(desc_path)) {
    desc <- read.dcf(desc_path, fields = "Imports")
    imports_str <- as.character(desc[1, "Imports"])
    expect_true(grepl("remotes", imports_str),
                info = "remotes must be in Imports so install_MStargetR() is self-contained")
  } else {
    skip("DESCRIPTION not found")
  }
})
