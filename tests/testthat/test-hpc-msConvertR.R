library(mockery)

# ---------------------------------------------------------------------------
# test-hpc-msConvertR.R
#
# Exercises the enable_HPC = TRUE branch of msConvertR() and its helpers on a
# Windows workstation where Apptainer is not installed.  All seven tests run
# completely offline: Apptainer, Docker, and the SIF file are all stubbed.
#
# Shared helpers (local_hpc_mode, new_apptainer_recorder, etc.) live in
# tests/testthat/helper-hpc-mocks.R and are auto-loaded by testthat.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Test 1: Front-door dispatch picks apptainer
# ---------------------------------------------------------------------------
test_that("msConvertR front-door: assert_runtime_available called with 'apptainer'", {
  suppressMessages({
    local_hpc_mode()

    stub(msConvertR, "validate_input_directory", function(dir) TRUE)
    stub(msConvertR, "derive_plate_groups",
         function(...) hpc_fake_groups("plate1"))

    # Capture which runtime was requested
    assert_calls <- character(0)
    stub(msConvertR, "assert_runtime_available", function(runtime) {
      assert_calls <<- c(assert_calls, runtime)
      invisible(runtime)
    })

    # Capture enable_HPC as passed through to msConvertR_mzml_conversion
    captured_hpc <- NULL
    stub(msConvertR, "msConvertR_mzml_conversion",
         function(..., enable_HPC = FALSE) {
           captured_hpc <<- enable_HPC
           invisible(NULL)
         })

    # check_docker must NOT be called in HPC mode; stub it to error so we
    # notice if the wrong branch fires
    stub(msConvertR, "check_docker", function() {
      stop("check_docker should not be called when enable_HPC = TRUE")
    })

    tmp_out <- withr::local_tempdir()

    expect_no_error(
      msConvertR("input_dir", tmp_out, enable_HPC = TRUE)
    )

    # assert_runtime_available must have been called with "apptainer" exactly once
    expect_true("apptainer" %in% assert_calls)
    expect_equal(sum(assert_calls == "apptainer"), 1L)
    # must NOT have been called with "docker"
    expect_false("docker" %in% assert_calls)
    # enable_HPC must have been forwarded as TRUE
    expect_true(isTRUE(captured_hpc))
  })
})

# ---------------------------------------------------------------------------
# Test 2: Default arg reads MStargetR.enable_HPC option
# ---------------------------------------------------------------------------
test_that("msConvertR picks apptainer when MStargetR.enable_HPC option is TRUE", {
  suppressMessages({
    local_hpc_mode()   # sets options(MStargetR.enable_HPC = TRUE)

    stub(msConvertR, "validate_input_directory", function(dir) TRUE)
    stub(msConvertR, "derive_plate_groups",
         function(...) hpc_fake_groups("plate1"))

    assert_calls <- character(0)
    stub(msConvertR, "assert_runtime_available", function(runtime) {
      assert_calls <<- c(assert_calls, runtime)
      invisible(runtime)
    })

    stub(msConvertR, "msConvertR_mzml_conversion", function(...) invisible(NULL))
    stub(msConvertR, "check_docker", function() {
      stop("check_docker should not be called when enable_HPC = TRUE")
    })

    tmp_out <- withr::local_tempdir()

    # Call WITHOUT passing enable_HPC explicitly -- must pick it up from option
    expect_no_error(
      msConvertR("input_dir", tmp_out)
    )

    expect_true("apptainer" %in% assert_calls)
    expect_false("docker" %in% assert_calls)
  })
})

# ---------------------------------------------------------------------------
# Test 3: run_container receives correct image_command and binds in HPC mode
# ---------------------------------------------------------------------------
test_that("msConvertR_execute_command passes image_command and binds to run_container", {
  suppressMessages({
    local_hpc_mode()

    tmp_out <- withr::local_tempdir()

    # Build distinct host directories for input and output so the two bind
    # mounts can be told apart by their host path.
    host_in_dir  <- file.path(tmp_out, "raw_input")
    host_out_dir <- file.path(tmp_out, "mzml_output")
    dir.create(host_in_dir,  recursive = TRUE, showWarnings = FALSE)
    dir.create(host_out_dir, recursive = TRUE, showWarnings = FALSE)

    host_in  <- normalizePath(host_in_dir,  winslash = "/", mustWork = TRUE)
    host_out <- normalizePath(host_out_dir, winslash = "/", mustWork = TRUE)

    image_cmd <- c("wine", "msconvert", "/data/plate1.wiff", "-o", "/output")
    binds <- list(
      list(host = host_in,  container = "/data",   ro = TRUE),
      list(host = host_out, container = "/output", ro = FALSE)
    )
    commands <- list(list(
      image_command     = image_cmd,
      binds             = binds,
      docker_extra_args = NULL,
      docker_args       = NULL,
      saneID            = "plate1",
      junctions         = list()
    ))
    attr(commands, "active_plateIDs") <- "plate1"

    # Recorder captures every run_container() call
    recorder <- new_apptainer_recorder(exit_status = 0L)

    # Create the mzML output file so post-run verification passes.
    # msConvertR_execute_command looks for mzML files under
    # output_directory/<pid>/data/mzml/ -- use tmp_out as output_directory.
    mzml_dir <- file.path(tmp_out, "plate1", "data", "mzml")
    dir.create(mzml_dir, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(mzml_dir, "plate1_s1.mzML"))

    # Stub run_container, resolve_sif, and future::plan inside
    # msConvertR_execute_command.  With future::sequential (set by
    # local_hpc_mode()) the future body runs inline so the stubs are visible.
    stub(msConvertR_execute_command, "run_container", recorder$fn)
    stub(msConvertR_execute_command, "resolve_sif",
         function() getOption("MStargetR.sif_path"))
    # Intercept plan calls so multisession reset doesn't fight sequential
    stub(msConvertR_execute_command, "future::plan", function(...) invisible(NULL))
    stub(msConvertR_execute_command, "future::availableCores", function() 4L)

    suppressMessages(
      msConvertR_execute_command(commands, tmp_out, "plate1", enable_HPC = TRUE)
    )

    calls <- recorder$calls()
    expect_length(calls, 1L)
    expect_true(isTRUE(calls[[1]]$enable_HPC))

    # image_command must propagate unchanged
    expect_equal(calls[[1]]$image_command, image_cmd)

    # binds must contain both mounts
    call_binds <- calls[[1]]$binds
    expect_true(length(call_binds) >= 2L)

    hosts <- vapply(call_binds, function(b) b$host, character(1))
    expect_true(any(hosts == host_in))
    expect_true(any(hosts == host_out))

    # Input mount should be read-only
    input_bind <- call_binds[[which(hosts == host_in)]]
    expect_true(isTRUE(input_bind$ro))
  })
})

# ---------------------------------------------------------------------------
# Test 4: Worker count formula is honoured
# ---------------------------------------------------------------------------
test_that("msConvertR_execute_command uses availableCores - 2, floor 1", {
  suppressMessages({
    local_hpc_mode()

    # ---- Case A: 8 cores -> workers = 6 ----
    tmp_out_a <- withr::local_tempdir()

    plan_workers_a <- NULL
    stub(msConvertR_execute_command, "future::plan",
         function(strategy, workers = NULL, ...) {
           if (!is.null(workers)) plan_workers_a <<- workers
           invisible(NULL)
         })
    stub(msConvertR_execute_command, "future::availableCores", function() 8L)
    stub(msConvertR_execute_command, "run_container",
         function(...) { out <- character(0); attr(out, "status") <- 0L; out })
    stub(msConvertR_execute_command, "resolve_sif",
         function() getOption("MStargetR.sif_path"))

    # Empty commands list -- plan setup still runs, but no futures are spawned
    expect_no_error(
      msConvertR_execute_command(list(), tmp_out_a, character(0), enable_HPC = TRUE)
    )
    expect_equal(plan_workers_a, 6L)

    # ---- Case B: 1 core -> workers = 1 (floor) ----
    tmp_out_b <- withr::local_tempdir()

    plan_workers_b <- NULL
    stub(msConvertR_execute_command, "future::plan",
         function(strategy, workers = NULL, ...) {
           if (!is.null(workers)) plan_workers_b <<- workers
           invisible(NULL)
         })
    stub(msConvertR_execute_command, "future::availableCores", function() 1L)
    # run_container and resolve_sif stubs still needed
    stub(msConvertR_execute_command, "run_container",
         function(...) { out <- character(0); attr(out, "status") <- 0L; out })
    stub(msConvertR_execute_command, "resolve_sif",
         function() getOption("MStargetR.sif_path"))

    expect_no_error(
      msConvertR_execute_command(list(), tmp_out_b, character(0), enable_HPC = TRUE)
    )
    expect_equal(plan_workers_b, 1L)
  })
})

# ---------------------------------------------------------------------------
# Test 5: Per-plate dispatch produces N apptainer calls + log files
# ---------------------------------------------------------------------------
test_that("msConvertR_execute_command dispatches run_container once per plate in HPC mode", {
  suppressMessages({
    local_hpc_mode()

    tmp_out <- withr::local_tempdir()

    # Three plates
    plate_ids <- c("plateA", "plateB", "plateC")

    # Build a 3-element commands list
    commands <- lapply(plate_ids, function(pid) {
      list(
        image_command     = c("wine", "msconvert", paste0("/data/", pid, ".wiff"),
                              "-o", "/output"),
        binds             = list(
          list(host = normalizePath(tmp_out, winslash = "/"), container = "/data",   ro = TRUE),
          list(host = normalizePath(tmp_out, winslash = "/"), container = "/output", ro = FALSE)
        ),
        docker_extra_args = NULL,
        docker_args       = NULL,
        saneID            = pid,
        junctions         = list()
      )
    })
    attr(commands, "active_plateIDs") <- plate_ids

    # Pre-create mzML output directories + stub files so post-run check passes
    for (pid in plate_ids) {
      mzml_dir <- file.path(tmp_out, pid, "data", "mzml")
      dir.create(mzml_dir, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(mzml_dir, paste0(pid, "_s1.mzML")))
    }

    recorder <- new_apptainer_recorder(exit_status = 0L)

    stub(msConvertR_execute_command, "run_container", recorder$fn)
    stub(msConvertR_execute_command, "resolve_sif",
         function() getOption("MStargetR.sif_path"))
    stub(msConvertR_execute_command, "future::plan",    function(...) invisible(NULL))
    stub(msConvertR_execute_command, "future::availableCores", function() 4L)

    expect_no_error(
      msConvertR_execute_command(commands, tmp_out, plate_ids, enable_HPC = TRUE)
    )

    calls <- recorder$calls()

    # Exactly 3 run_container invocations
    expect_length(calls, 3L)

    # Every call must have enable_HPC = TRUE
    for (call in calls) {
      expect_true(isTRUE(call$enable_HPC))
    }

    # Per-plate log files must exist and contain "Runtime: Apptainer"
    for (pid in plate_ids) {
      log_lines <- read_pipeline_log(tmp_out, pid)
      expect_false(is.null(log_lines),
                   label = paste("log exists for", pid))
      expect_true(any(grepl("Runtime: Apptainer", log_lines)),
                  label = paste("log contains Runtime: Apptainer for", pid))
    }
  })
})

# ---------------------------------------------------------------------------
# Test 6: Post-run verification fires in HPC mode (FAILURE when no mzML)
# ---------------------------------------------------------------------------
test_that("msConvertR_execute_command marks plates as FAILURE when mzML absent after HPC run", {
  suppressMessages({
    local_hpc_mode()

    tmp_out <- withr::local_tempdir()

    plate_ids <- c("plateX", "plateY", "plateZ")

    commands <- lapply(plate_ids, function(pid) {
      list(
        image_command     = c("wine", "msconvert", paste0("/data/", pid, ".wiff"),
                              "-o", "/output"),
        binds             = list(
          list(host = normalizePath(tmp_out, winslash = "/"), container = "/data",   ro = TRUE),
          list(host = normalizePath(tmp_out, winslash = "/"), container = "/output", ro = FALSE)
        ),
        docker_extra_args = NULL,
        docker_args       = NULL,
        saneID            = pid,
        junctions         = list()
      )
    })
    attr(commands, "active_plateIDs") <- plate_ids

    # Create the mzml directories but leave them EMPTY -- no .mzML files.
    # The recorder reports exit_status = 0 (container appeared to succeed),
    # but the post-run verification should catch the missing mzML output.
    for (pid in plate_ids) {
      dir.create(file.path(tmp_out, pid, "data", "mzml"),
                 recursive = TRUE, showWarnings = FALSE)
    }

    recorder <- new_apptainer_recorder(exit_status = 0L)

    stub(msConvertR_execute_command, "run_container", recorder$fn)
    stub(msConvertR_execute_command, "resolve_sif",
         function() getOption("MStargetR.sif_path"))
    stub(msConvertR_execute_command, "future::plan",    function(...) invisible(NULL))
    stub(msConvertR_execute_command, "future::availableCores", function() 4L)

    expect_error(
      msConvertR_execute_command(commands, tmp_out, plate_ids, enable_HPC = TRUE),
      regexp = "of.*plate\\(s\\) failed conversion"
    )

    # All three plates should have been attempted (recorder has 3 entries)
    expect_length(recorder$calls(), 3L)
  })
})

# ---------------------------------------------------------------------------
# Test 7: Negative path - SIF missing causes resolve_sif to stop
# ---------------------------------------------------------------------------
test_that("resolve_sif stops with clear error when MStargetR.sif_path does not exist", {
  withr::local_options(list(
    MStargetR.sif_path = "C:/no/such/file.sif"
  ))

  expect_error(
    resolve_sif(),
    regexp = "file does not exist"
  )
})
