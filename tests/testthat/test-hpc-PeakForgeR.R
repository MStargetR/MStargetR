# test-hpc-PeakForgeR.R
#
# Exercises the enable_HPC = TRUE branch of PeakForgeR() on a Windows
# workstation by stubbing the runtime-detection chokepoints.
#
# Shared helpers (local_hpc_mode, new_apptainer_recorder, stub_apptainer_runtime,
# stub_pipeline_output, read_pipeline_log) are loaded automatically from
# tests/testthat/helper-hpc-mocks.R.

library(mockery)

# ---------------------------------------------------------------------------
# Shared setup helpers used across several tests
# ---------------------------------------------------------------------------

# Build a clean project directory that has the mzML structure PeakForgeR needs
# when plateID_outputs are supplied (no prior msConvertR run).
make_hpc_project_dir <- function(plate_ids, envir = parent.frame()) {
  proj <- withr::local_tempdir(.local_envir = envir)
  for (pid in plate_ids) {
    stub_pipeline_output("msConvertR", pid, proj)
  }
  proj
}

# ---------------------------------------------------------------------------
# Test 1: Front-door dispatch picks apptainer + pre-resolves SIF
# ---------------------------------------------------------------------------

test_that("HPC dispatch calls assert_runtime_available('apptainer') and resolve_sif once on main session", {
  fake_sif <- local_hpc_mode()
  proj     <- make_hpc_project_dir(c("plate_A", "plate_B"))

  assert_calls  <- list()
  resolve_calls <- 0L

  stub(PeakForgeR, "validate_project_directory",  function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list",  function(x, y) x)
  stub(PeakForgeR, "assert_runtime_available", function(runtime) {
    assert_calls[[length(assert_calls) + 1L]] <<- runtime
    invisible(runtime)
  })
  stub(PeakForgeR, "resolve_sif", function() {
    resolve_calls <<- resolve_calls + 1L
    fake_sif
  })
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4L)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    lapply(plateIDs, function(pid) list(success = TRUE, plateID = pid))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  suppressMessages({
    PeakForgeR(
      user_name        = "test_user",
      project_directory = proj,
      mrm_template_list = list("a.tsv"),
      QC_sample_label  = "QC",
      plateID_outputs  = c("plate_A", "plate_B"),
      enable_HPC       = TRUE
    )
  })

  expect_true(
    any(vapply(assert_calls, identical, logical(1), "apptainer")),
    info = "assert_runtime_available('apptainer') must be called at least once"
  )
  expect_equal(resolve_calls, 1L,
    info = "resolve_sif() must be called exactly once on the main session")
})

# ---------------------------------------------------------------------------
# Test 2: Default arg reads MStargetR.enable_HPC option
# ---------------------------------------------------------------------------

test_that("PeakForgeR reads MStargetR.enable_HPC option when enable_HPC not passed", {
  fake_sif <- local_hpc_mode()    # sets options(MStargetR.enable_HPC = TRUE, ...)
  proj     <- make_hpc_project_dir(c("plate_A", "plate_B"))

  assert_calls <- list()

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "assert_runtime_available", function(runtime) {
    assert_calls[[length(assert_calls) + 1L]] <<- runtime
    invisible(runtime)
  })
  stub(PeakForgeR, "resolve_sif", function() fake_sif)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4L)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    lapply(plateIDs, function(pid) list(success = TRUE, plateID = pid))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  # Call WITHOUT enable_HPC -- should pick it up from the option
  suppressMessages({
    PeakForgeR(
      user_name         = "test_user",
      project_directory = proj,
      mrm_template_list = list("a.tsv"),
      QC_sample_label   = "QC",
      plateID_outputs   = c("plate_A", "plate_B")
    )
  })

  expect_true(
    any(vapply(assert_calls, identical, logical(1), "apptainer")),
    info = "Apptainer path must be taken when MStargetR.enable_HPC option is TRUE"
  )
})

# ---------------------------------------------------------------------------
# Test 3: future_lapply runs with sequential plan and invokes run_container
#         once per plate
# ---------------------------------------------------------------------------

test_that("HPC mode dispatches run_container once per plate under sequential plan", {
  # local_hpc_mode() forces sequential futures so mockery stubs are visible
  # inside the closures executed by future_lapply.
  local_hpc_mode()
  proj <- make_hpc_project_dir(c("plate_A", "plate_B"))

  recorder <- new_apptainer_recorder(stdout = character(0), exit_status = 0L)

  plan_calls <- list()

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "assert_runtime_available", function(runtime) invisible(runtime))
  stub(PeakForgeR, "resolve_sif", function() getOption("MStargetR.sif_path"))
  # Capture the future::plan() call but do NOT apply multisession so the
  # sequential plan set by local_hpc_mode() stays in effect.
  stub(PeakForgeR, "future::plan", function(...) {
    plan_calls[[length(plan_calls) + 1L]] <<- list(...)
    NULL
  })
  stub(PeakForgeR, "future::availableCores", function() 4L)
  # Allow future_lapply to actually execute the per-plate closure sequentially
  # so we exercise the inner code path.
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    lapply(plateIDs, fn)
  })
  # Stub the inner per-plate helpers that would normally touch the filesystem
  # or real containers.
  stub(PeakForgeR, "PeakForgeR_setup_project", function(...) {
    list(project_details = list(project_dir = proj),
         templates = list(mrm_guides = list()),
         data = list())
  })
  stub(PeakForgeR, "import_mzml", function(plateID, master_list) master_list)
  stub(PeakForgeR, "peak_picking", function(plateID, master_list, enable_HPC = FALSE) {
    # Simulate a run_container call so the recorder captures it
    recorder$fn(
      image_command = c("wine", "SkylineCmd", "--dir=/data"),
      binds         = list(list(host = "/fake/data", container = "/data")),
      enable_HPC    = TRUE
    )
    master_list
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  suppressMessages({
    PeakForgeR(
      user_name         = "test_user",
      project_directory = proj,
      mrm_template_list = list("a.tsv"),
      QC_sample_label   = "QC",
      plateID_outputs   = c("plate_A", "plate_B"),
      enable_HPC        = TRUE
    )
  })

  calls <- recorder$calls()
  expect_equal(length(calls), 2L,
    info = "run_container must be invoked once per plateID (2 plates => 2 calls)")
  expect_true(all(vapply(calls, function(c) isTRUE(c$enable_HPC), logical(1))),
    info = "Every run_container call must have enable_HPC = TRUE")
})

# ---------------------------------------------------------------------------
# Test 4: Reproducibility — future_lapply is invoked with future.seed = TRUE
# ---------------------------------------------------------------------------

test_that("PeakForgeR passes future.seed = TRUE to future_lapply in HPC mode", {
  fake_sif <- local_hpc_mode()
  proj     <- make_hpc_project_dir(c("plate_A", "plate_B"))

  captured_dots <- list()

  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "assert_runtime_available", function(runtime) invisible(runtime))
  stub(PeakForgeR, "resolve_sif", function() fake_sif)
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4L)
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    captured_dots <<- list(...)
    lapply(plateIDs, function(pid) list(success = TRUE, plateID = pid))
  })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  suppressMessages({
    PeakForgeR(
      user_name         = "test_user",
      project_directory = proj,
      mrm_template_list = list("a.tsv"),
      QC_sample_label   = "QC",
      plateID_outputs   = c("plate_A", "plate_B"),
      enable_HPC        = TRUE
    )
  })

  expect_true("future.seed" %in% names(captured_dots),
    info = "future_lapply must receive a future.seed argument")
  expect_identical(captured_dots$future.seed, TRUE,
    info = "future.seed must be TRUE to force L'Ecuyer-CMRG streams per worker")
})

# ---------------------------------------------------------------------------
# Test 5: Per-plate log records the runtime + a success marker.
# PeakForgeR writes "Runtime: Apptainer" (when enable_HPC=TRUE) or
# "Runtime: Docker" alongside "Status: SUCCESS" so log inspection on a
# cluster is symmetric with msConvertR's log format.
# Path: MStargetR_logs/<plateID>_MStargetR_log.txt.
# ---------------------------------------------------------------------------

test_that("Per-plate log records Runtime: Apptainer and Status: SUCCESS under HPC mode", {
  local_hpc_mode()
  proj <- make_hpc_project_dir(c("plate_A", "plate_B"))

  # We drive the actual futures closure so the write_log() calls inside the
  # closure execute.  All inner helpers that would touch a real container or
  # mzML files are stubbed to no-ops that return a valid master_list shape.
  stub(PeakForgeR, "validate_project_directory", function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list", function(x, y) x)
  stub(PeakForgeR, "assert_runtime_available", function(runtime) invisible(runtime))
  stub(PeakForgeR, "resolve_sif", function() getOption("MStargetR.sif_path"))
  stub(PeakForgeR, "future::plan", function(...) NULL)
  stub(PeakForgeR, "future::availableCores", function() 4L)
  # Execute the closure sequentially so log files are created on this host
  stub(PeakForgeR, "future.apply::future_lapply", function(plateIDs, fn, ...) {
    lapply(plateIDs, fn)
  })
  stub(PeakForgeR, "PeakForgeR_setup_project", function(...) {
    list(project_details = list(project_dir = proj),
         templates = list(mrm_guides = list()),
         data = list())
  })
  stub(PeakForgeR, "import_mzml", function(plateID, master_list) master_list)
  stub(PeakForgeR, "peak_picking",
       function(plateID, master_list, enable_HPC = FALSE) master_list)
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  suppressMessages({
    PeakForgeR(
      user_name         = "test_user",
      project_directory = proj,
      mrm_template_list = list("a.tsv"),
      QC_sample_label   = "QC",
      plateID_outputs   = c("plate_A", "plate_B"),
      enable_HPC        = TRUE
    )
  })

  for (pid in c("plate_A", "plate_B")) {
    log_lines <- read_pipeline_log(proj, pid)
    expect_false(is.null(log_lines),
      info = paste("Log for plate", pid, "must exist"))
    expect_true(
      any(grepl("Status:\\s+SUCCESS", log_lines)),
      info = paste("Log for plate", pid, "must contain 'Status: SUCCESS'")
    )
    expect_true(
      any(grepl("Runtime:\\s+Apptainer", log_lines)),
      info = paste("Log for plate", pid, "must contain 'Runtime: Apptainer'")
    )
  }
})

# ---------------------------------------------------------------------------
# Test 6: Negative path — SIF resolution fails when file does not exist
# ---------------------------------------------------------------------------

test_that("resolve_sif() stops with informative error when sif_path file is missing", {
  withr::local_options(list(
    MStargetR.enable_HPC = TRUE,
    MStargetR.sif_path   = "C:/no/such.sif"
  ))

  expect_error(
    resolve_sif(),
    regexp = "file does not exist|does not exist",
    info = "resolve_sif must stop when the option points to a non-existent file"
  )
})
