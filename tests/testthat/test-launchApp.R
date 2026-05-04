# Tests for launchMStargetR (launchApp.R) ----
library(mockery)

# ============================================================================
# Missing package detection
# ============================================================================

test_that("launchMStargetR errors when required GUI packages are missing", {
  # Stub requireNamespace to return FALSE for shiny
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) {
    if (pkg == "shiny") return(FALSE)
    return(TRUE)
  })

  expect_error(
    launchMStargetR(),
    "following packages are required.*shiny"
  )
})

test_that("launchMStargetR lists all missing packages in error message", {
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) {
    return(FALSE)
  })

  expect_error(
    launchMStargetR(),
    "shiny"
  )
  expect_error(
    launchMStargetR(),
    "bslib"
  )
})

test_that("launchMStargetR errors include install instructions", {
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) {
    return(FALSE)
  })

  expect_error(
    launchMStargetR(),
    "install\\.packages"
  )
})

# ============================================================================
# App directory validation
# ============================================================================

test_that("launchMStargetR errors when app directory not found", {
  # All packages present but system.file returns empty string
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  stub(launchMStargetR, "system.file", function(...) "")

  expect_error(
    launchMStargetR(),
    "Could not find the MStargetR Shiny application directory"
  )
})

# ============================================================================
# Parameter forwarding
# ============================================================================

test_that("launchMStargetR passes parameters to shiny::runApp", {
  # Track what runApp receives
  captured_args <- NULL

  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  stub(launchMStargetR, "system.file", function(...) "/fake/app/dir")
  stub(launchMStargetR, "shiny::runApp", function(...) {
    captured_args <<- list(...)
  })

  suppressMessages(
    launchMStargetR(port = 4242, launch.browser = FALSE, host = "127.0.0.1")
  )

  expect_equal(captured_args$port, 4242)
  expect_equal(captured_args$launch.browser, FALSE)
  expect_equal(captured_args$host, "127.0.0.1")
})

test_that("launchMStargetR uses default host 127.0.0.1", {
  captured_args <- NULL

  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  stub(launchMStargetR, "system.file", function(...) "/fake/app/dir")
  stub(launchMStargetR, "shiny::runApp", function(...) {
    captured_args <<- list(...)
  })

  suppressMessages(launchMStargetR())

  expect_equal(captured_args$host, "127.0.0.1")
})

# ============================================================================
# Input validation edge cases
# ============================================================================

test_that("launchMStargetR rejects non-integer port", {
  expect_error(
    launchMStargetR(port = 3.5),
    "port.*must be a single integer"
  )
})

test_that("launchMStargetR rejects port below 1", {
  expect_error(
    launchMStargetR(port = 0),
    "port.*must be a single integer"
  )
})

test_that("launchMStargetR rejects port above 65535", {
  expect_error(
    launchMStargetR(port = 70000),
    "port.*must be a single integer"
  )
})

test_that("launchMStargetR rejects character port", {
  expect_error(
    launchMStargetR(port = "8080"),
    "port.*must be a single integer"
  )
})

test_that("launchMStargetR rejects NA launch.browser", {
  expect_error(
    launchMStargetR(launch.browser = NA),
    "launch.browser.*must be TRUE or FALSE"
  )
})

test_that("launchMStargetR rejects character launch.browser", {
  expect_error(
    launchMStargetR(launch.browser = "yes"),
    "launch.browser.*must be TRUE or FALSE"
  )
})

test_that("launchMStargetR rejects empty string host", {
  expect_error(
    launchMStargetR(host = ""),
    "host.*must be a single non-empty character"
  )
})

test_that("launchMStargetR rejects numeric host", {
  expect_error(
    launchMStargetR(host = 127),
    "host.*must be a single non-empty character"
  )
})

test_that("launchMStargetR rejects malformed host strings", {
  # Out-of-range IPv4 octet
  expect_error(
    launchMStargetR(host = "999.999.999.999"),
    "does not look like a valid IPv4 address or hostname"
  )
  # Contains shell-ish punctuation that is neither IPv4 nor a DNS label
  expect_error(
    launchMStargetR(host = "bad!!host"),
    "does not look like a valid IPv4 address or hostname"
  )
})

test_that("launchMStargetR accepts localhost, ::1, and DNS hostnames", {
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  stub(launchMStargetR, "system.file", function(...) "/fake/app/dir")
  captured <- list()
  stub(launchMStargetR, "shiny::runApp", function(...) {
    captured[[length(captured) + 1L]] <<- list(...)
  })

  for (h in c("localhost", "::1", "example.com", "my-server.local",
              "0.0.0.0", "127.0.0.1")) {
    expect_silent(
      suppressMessages(launchMStargetR(host = h))
    )
  }
  expect_equal(length(captured), 6L)
})

test_that("launchMStargetR accepts valid port at boundary 1", {
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  stub(launchMStargetR, "system.file", function(...) "/fake/app/dir")
  captured_args <- NULL
  stub(launchMStargetR, "shiny::runApp", function(...) {
    captured_args <<- list(...)
  })

  suppressMessages(launchMStargetR(port = 1))
  expect_equal(captured_args$port, 1)
})

test_that("launchMStargetR accepts valid port at boundary 65535", {
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  stub(launchMStargetR, "system.file", function(...) "/fake/app/dir")
  captured_args <- NULL
  stub(launchMStargetR, "shiny::runApp", function(...) {
    captured_args <<- list(...)
  })

  suppressMessages(launchMStargetR(port = 65535))
  expect_equal(captured_args$port, 65535)
})

test_that("launchMStargetR accepts NULL port (default)", {
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  stub(launchMStargetR, "system.file", function(...) "/fake/app/dir")
  captured_args <- NULL
  stub(launchMStargetR, "shiny::runApp", function(...) {
    captured_args <<- list(...)
  })

  suppressMessages(launchMStargetR(port = NULL))
  expect_null(captured_args$port)
})

test_that("launchMStargetR passes app directory path to shiny::runApp", {
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  stub(launchMStargetR, "system.file", function(...) "/my/custom/app/path")
  captured_appDir <- NULL
  stub(launchMStargetR, "shiny::runApp", function(appDir, ...) {
    captured_appDir <<- appDir
  })

  suppressMessages(launchMStargetR())
  expect_equal(captured_appDir, "/my/custom/app/path")
})

test_that("launchMStargetR constructs app path via system.file('shiny','MStargetR_app')", {
  # Verify launchMStargetR asks system.file() for the correct resource path
  # (the "shiny" subdir and "MStargetR_app" folder inside the installed pkg).
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  captured_sysfile <- NULL
  stub(launchMStargetR, "system.file", function(...) {
    captured_sysfile <<- list(...)
    "/resolved/app/path"
  })
  stub(launchMStargetR, "shiny::runApp", function(...) invisible(NULL))

  suppressMessages(launchMStargetR())

  # Positional args should include the "shiny" and "MStargetR_app" segments.
  unnamed <- captured_sysfile[names(captured_sysfile) == "" |
                                is.null(names(captured_sysfile))]
  flat <- unlist(unnamed, use.names = FALSE)
  expect_true("shiny" %in% flat)
  expect_true("MStargetR_app" %in% flat)
  # The package= argument must request MStargetR.
  expect_equal(captured_sysfile$package, "MStargetR")
})

test_that("launchMStargetR: missing app dir error mentions package installation", {
  # When system.file returns "" the function must not silently launch;
  # it must error with a message that guides the user to re-install.
  stub(launchMStargetR, "requireNamespace", function(pkg, ...) TRUE)
  stub(launchMStargetR, "system.file", function(...) "")
  run_called <- FALSE
  stub(launchMStargetR, "shiny::runApp", function(...) {
    run_called <<- TRUE
  })

  expect_error(launchMStargetR(), "properly installed")
  expect_false(run_called)
})
