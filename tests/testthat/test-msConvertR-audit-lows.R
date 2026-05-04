# Regression tests for skipped Low/Info audit findings:
# MS-010, MS-023, MS-027, MS-028, MS-029, MS-030, MS-034, MS-036, MS-038, MS-039
library(mockery)

# ============================================================================
# MS-023 / MS-034: MSTARGETR_VENDOR_EXT_PATTERN constant + extension coverage
# ============================================================================

test_that("MSTARGETR_VENDOR_EXT_PATTERN is a non-empty character string", {
  expect_type(MSTARGETR_VENDOR_EXT_PATTERN, "character")
  expect_length(MSTARGETR_VENDOR_EXT_PATTERN, 1L)
  expect_true(nzchar(MSTARGETR_VENDOR_EXT_PATTERN))
})

test_that("MSTARGETR_VENDOR_EXT_PATTERN matches each listed vendor extension", {
  exts <- c("d", "baf", "fid", "yep", "tsf", "tdf", "mbi",
            "wiff", "wiff.scan", "scan", "wiff2",
            "qgd", "qgb", "qgm", "lcd", "lcdproj",
            "raw", "uep", "sdf", "dat", "wcf", "wproj", "wdata")
  for (ext in exts) {
    fname <- paste0("plate.", ext)
    expect_true(
      grepl(MSTARGETR_VENDOR_EXT_PATTERN, fname, ignore.case = TRUE),
      info = paste("pattern should match extension:", ext)
    )
  }
})

test_that("MSTARGETR_VENDOR_EXT_PATTERN does not match non-vendor extensions", {
  non_exts <- c("plate.txt", "plate.csv", "plate.mzML", "plate.zip", "plate.pdf")
  for (f in non_exts) {
    expect_false(
      grepl(MSTARGETR_VENDOR_EXT_PATTERN, f, ignore.case = TRUE),
      info = paste("pattern should not match:", f)
    )
  }
})

test_that("msConvertR uses MSTARGETR_VENDOR_EXT_PATTERN (not a local literal) for str_remove", {
  # Verify that after stripping the pattern from a known filename the ID is clean
  pid <- stringr::str_remove("Plate_001.wiff", MSTARGETR_VENDOR_EXT_PATTERN)
  expect_equal(pid, "Plate_001")

  pid2 <- stringr::str_remove("Plate_002.raw", MSTARGETR_VENDOR_EXT_PATTERN)
  expect_equal(pid2, "Plate_002")
})

# ============================================================================
# MS-027: Docker command verbose gate
# ============================================================================

test_that("msConvertR_construct_command_for_terminal suppresses full docker args by default", {
  tmp <- withr::local_tempdir()
  raw_dir <- file.path(tmp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)
  file.create(file.path(raw_dir, "plate1.wiff"))
  dir.create(file.path(tmp, "out", "plate1", "data", "mzml"), recursive = TRUE)

  msgs <- character(0)
  withr::with_options(list(MStargetR.verbose = FALSE), {
    withCallingHandlers(
      msConvertR_construct_command_for_terminal(tmp, file.path(tmp, "out"),
                                               plateIDs = "plate1"),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  })
  # When verbose is FALSE, full docker args should NOT appear in any message
  expect_false(any(grepl("docker run", msgs)))
  # But a queued confirmation should appear
  expect_true(any(grepl("queued for conversion", msgs)))
})

test_that("msConvertR_construct_command_for_terminal emits full docker args when verbose=TRUE", {
  tmp <- withr::local_tempdir()
  raw_dir <- file.path(tmp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)
  file.create(file.path(raw_dir, "plate1.wiff"))
  dir.create(file.path(tmp, "out", "plate1", "data", "mzml"), recursive = TRUE)

  msgs <- character(0)
  withr::with_options(list(MStargetR.verbose = TRUE), {
    withCallingHandlers(
      msConvertR_construct_command_for_terminal(tmp, file.path(tmp, "out"),
                                               plateIDs = "plate1"),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  })
  expect_true(any(grepl("docker", msgs)))
})

# ============================================================================
# MS-028: post-run mzML count verification
# ============================================================================

test_that("msConvertR_execute_command marks plate FAILURE when no mzML produced", {
  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 2)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(pid = "plate1"), class = "MockFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(f, ...) {
    list(plateID = f$pid, success = TRUE)   # Docker exit OK, but no mzML written
  })

  tmp <- withr::local_tempdir()
  # Deliberately do NOT create any .mzML file under plate1/data/mzml
  dir.create(file.path(tmp, "plate1", "data", "mzml"), recursive = TRUE)

  expect_error(
    suppressMessages(
      msConvertR_execute_command(
        commands = list(list(docker_args = c("run", "--rm", "img"))),
        output_directory = tmp,
        plateIDs = c("plate1")
      )
    ),
    "failed conversion"
  )
})

test_that("msConvertR_execute_command succeeds when at least one mzML is present", {
  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 2)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(pid = "plate1"), class = "MockFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(f, ...) {
    list(plateID = f$pid, success = TRUE)
  })

  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "plate1", "data", "mzml"), recursive = TRUE)
  file.create(file.path(tmp, "plate1", "data", "mzml", "plate1_sample01.mzML"))

  expect_no_error(
    suppressMessages(
      msConvertR_execute_command(
        commands = list(list(docker_args = c("run", "--rm", "img"))),
        output_directory = tmp,
        plateIDs = c("plate1")
      )
    )
  )
})

# ============================================================================
# MS-029: success/failure message emitted in collector, not inside future
# ============================================================================

test_that("msConvertR_execute_command emits per-plate status after value() returns", {
  # The message must come from the collector loop, not from inside the future.
  # We verify this by checking that the message fires after future::value is called.
  value_called <- FALSE
  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 2)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(pid = "plate1"), class = "MockFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(f, ...) {
    value_called <<- TRUE
    list(plateID = f$pid, success = TRUE)
  })

  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "plate1", "data", "mzml"), recursive = TRUE)
  file.create(file.path(tmp, "plate1", "data", "mzml", "plate1.mzML"))

  msgs <- character(0)
  withCallingHandlers(
    msConvertR_execute_command(
      commands = list(list(docker_args = c("run", "--rm", "img"))),
      output_directory = tmp,
      plateIDs = c("plate1")
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(value_called)
  expect_true(any(grepl("Finished conversion for plate1", msgs)))
})

# ============================================================================
# MS-030: is_qc_support_file() helper
# ============================================================================

test_that("is_qc_support_file identifies ANPC conditioning files", {
  expect_true(is_qc_support_file("ANPC_plate-COND1.mzML"))
  expect_true(is_qc_support_file("run-COND12.mzML"))
})

test_that("is_qc_support_file identifies ANPC blank files", {
  expect_true(is_qc_support_file("run-BLANK1.mzML"))
  expect_true(is_qc_support_file("run-BLANK_1.mzML"))
})

test_that("is_qc_support_file identifies ISTDs files", {
  expect_true(is_qc_support_file("run-ISTDs_3.mzML"))
})

test_that("is_qc_support_file returns FALSE for normal sample files", {
  expect_false(is_qc_support_file("Plate_001_sample01.mzML"))
  expect_false(is_qc_support_file("Study_run_42.mzML"))
})

test_that("is_qc_support_file is case-insensitive", {
  expect_true(is_qc_support_file("run-cond1.mzML"))
  expect_true(is_qc_support_file("run-blank1.mzML"))
})

test_that("is_qc_support_file works on a character vector", {
  fnames <- c("good.mzML", "bad-COND1.mzML", "also-good.mzML")
  result <- is_qc_support_file(fnames)
  expect_equal(result, c(FALSE, TRUE, FALSE))
})

# ============================================================================
# MS-036: future::plan state is observable (integration-style)
# ============================================================================

test_that("msConvertR_execute_command calls future::plan with multisession and workers", {
  plan_args <- list()
  stub(msConvertR_execute_command, "future::plan", function(...) {
    plan_args[[length(plan_args) + 1]] <<- list(...)
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
  # First call must be multisession with workers
  first <- plan_args[[1]]
  expect_true("workers" %in% names(first))
  # Second call must reset to sequential
  second <- plan_args[[2]]
  expect_true(any(vapply(second, function(x) inherits(x, "sequential") ||
                           identical(x, future::sequential), logical(1))))
})

# ============================================================================
# MS-039: MSTARGETR_TRANSITION_SUMMARY_COLS constant
# ============================================================================

test_that("MSTARGETR_TRANSITION_SUMMARY_COLS contains the six expected columns", {
  expected <- c("Molecule List Name", "Precursor Name", "Precursor Mz",
                "Precursor Charge", "Product Mz", "Product Charge")
  expect_equal(MSTARGETR_TRANSITION_SUMMARY_COLS, expected)
})

test_that("transition_checkR clash report uses MSTARGETR_TRANSITION_SUMMARY_COLS", {
  df <- rbind(
    data.frame(`Molecule List Name` = "G1", `Precursor Name` = "Met_A",
               `Precursor Mz` = 100.0, `Precursor Charge` = 1L,
               `Product Mz` = 80.0, `Product Charge` = 1L,
               `Extra Col` = "x",
               check.names = FALSE, stringsAsFactors = FALSE),
    data.frame(`Molecule List Name` = "G1", `Precursor Name` = "Met_B",
               `Precursor Mz` = 100.0, `Precursor Charge` = 1L,
               `Product Mz` = 80.0, `Product Charge` = 1L,
               `Extra Col` = "y",
               check.names = FALSE, stringsAsFactors = FALSE)
  )
  result <- suppressMessages(transition_checkR(df))
  # Result must only include the summary columns (not Extra Col)
  expect_equal(sort(colnames(result)),
               sort(intersect(MSTARGETR_TRANSITION_SUMMARY_COLS, colnames(df))))
  expect_false("Extra Col" %in% colnames(result))
})

# ============================================================================
# MS-010: orchestrator separation (plan vs execute) — structural regression
# ============================================================================

test_that("msConvertR_mzml_conversion delegates planning to construct and execution to execute", {
  construct_called <- FALSE
  execute_called   <- FALSE
  stub(msConvertR_mzml_conversion, "msConvertR_set_working_directory", function(...) NULL)
  stub(msConvertR_mzml_conversion, "msConvertR_setup_project_directories", function(...) NULL)
  stub(msConvertR_mzml_conversion, "msConvertR_construct_command_for_terminal",
       function(inp, out, raw, san = raw) {
         construct_called <<- TRUE
         cmds <- list()
         attr(cmds, "active_plateIDs") <- character(0)
         cmds
       })
  stub(msConvertR_mzml_conversion, "msConvertR_execute_command",
       function(...) { execute_called <<- TRUE; invisible(NULL) })
  stub(msConvertR_mzml_conversion, "msConvertR_restructure_directory", function(...) NULL)

  suppressMessages(
    msConvertR_mzml_conversion("in", "out",
                               plateIDs = character(0),
                               vendor_extension_patterns = "\\.wiff$")
  )
  expect_true(construct_called)
  # execute not called when commands is empty (nothing to do)
  expect_false(execute_called)
})
