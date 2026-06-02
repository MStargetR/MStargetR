# Shared HPC test fixtures for MStargetR
# Loaded automatically by testthat before any test-hpc-*.R runs.
#
# The MStargetR HPC code path (enable_HPC = TRUE) targets Apptainer/Singularity
# on a Linux cluster. These helpers stand in for that environment on a Windows
# workstation so the dispatch logic in R/container_runtime.R, R/msConvertR.R
# and R/PeakForgeR.R can be exercised without a real SIF, apptainer binary,
# or worker nodes.
#
# Helpers exported from this file:
#   local_hpc_mode()              -- toggle HPC options + force sequential plan
#   new_apptainer_recorder()      -- captures every run_container() call
#   stub_apptainer_runtime()      -- mocks the runtime-detection chokepoints
#   stub_pipeline_output()        -- drops a known fixture for the next stage
#   read_pipeline_log()           -- read a per-plate MStargetR log file

#' Activate HPC-mode options and force sequential futures for one test scope
#'
#' Sets the options that the package consults to decide between Docker and
#' Apptainer dispatch, then forces `future::plan(sequential)`. Sequential
#' execution is required because mockery::stub() rewrites the local copy of
#' the function under test; with the package's default `multisession` plan,
#' the per-plate closure is serialised to a worker that re-resolves package
#' symbols from the namespace and bypasses the stub.
#'
#' All side effects are undone when the calling test exits.
#'
#' @param verbose passed through to `options(MStargetR.verbose = ...)`.
#' @param envir scope governing teardown (defaults to the calling test).
#' @return Invisibly, the absolute path to the fake SIF file. Useful when a
#'   test wants to assert the file appeared in a captured apptainer argv.
local_hpc_mode <- function(verbose = TRUE, envir = parent.frame()) {
  fake_sif <- tempfile(pattern = "mstargetr-pwiz-", fileext = ".sif")
  file.create(fake_sif)

  withr::local_options(
    list(
      MStargetR.enable_HPC = TRUE,
      MStargetR.sif_path   = fake_sif,
      MStargetR.verbose    = verbose
    ),
    .local_envir = envir
  )

  old_plan <- future::plan(future::sequential)
  withr::defer({
    try(future::plan(old_plan), silent = TRUE)
    if (file.exists(fake_sif)) unlink(fake_sif)
  }, envir = envir)

  invisible(normalizePath(fake_sif, winslash = "/", mustWork = TRUE))
}

#' Create a recorder that captures every run_container() invocation
#'
#' Returns a list with two members:
#'   - `fn`: drop-in replacement for `run_container()` to pass to
#'     `mockery::stub(target_fn, "run_container", recorder$fn)`. Captures the
#'     full argument list of each call.
#'   - `calls()`: returns the captured calls as a list of named lists with
#'     elements `image_command`, `binds`, `enable_HPC`, `docker_extra_args`,
#'     `apptainer_extra_args`.
#'
#' The replacement returns a character vector with `attr(., "status")` set to
#' `exit_status` so callers that inspect the system2-style result still work.
#'
#' @param stdout character vector returned as the fake container stdout.
#' @param exit_status integer attached as `status` attribute. 0 = success.
new_apptainer_recorder <- function(stdout = character(0), exit_status = 0L) {
  calls <- list()
  fn <- function(image_command,
                 binds = list(),
                 enable_HPC = getOption("MStargetR.enable_HPC", FALSE),
                 docker_extra_args = NULL,
                 apptainer_extra_args = NULL,
                 stdout = TRUE,
                 stderr = TRUE) {
    calls[[length(calls) + 1L]] <<- list(
      image_command        = image_command,
      binds                = binds,
      enable_HPC           = enable_HPC,
      docker_extra_args    = docker_extra_args,
      apptainer_extra_args = apptainer_extra_args
    )
    out <- stdout
    attr(out, "status") <- exit_status
    out
  }
  list(fn = fn, calls = function() calls)
}

#' Stub the apptainer runtime detection + SIF resolution for one test scope
#'
#' Replaces the package internals that touch the host environment with
#' deterministic stand-ins. Wires the stubs into `target_fn` via mockery so
#' the calling test can drive the function under test without an actual
#' apptainer binary on PATH.
#'
#' Intended use:
#'   recorder <- new_apptainer_recorder()
#'   stub_apptainer_runtime(my_test_target)
#'   mockery::stub(my_test_target, "run_container", recorder$fn)
#'
#' @param target_fn the function under test, in its caller's frame, that
#'   eventually calls `assert_runtime_available()`, `resolve_sif()`, or
#'   `mstargetr_find_apptainer()`.
#' @param sif_path path returned by the stubbed `resolve_sif()`. Defaults to
#'   `getOption("MStargetR.sif_path")` set by `local_hpc_mode()`.
#' @return Invisibly NULL. Side effect: stubs are installed on `target_fn`.
stub_apptainer_runtime <- function(target_fn,
                                   sif_path = getOption("MStargetR.sif_path")) {
  if (is.null(sif_path) || !nzchar(sif_path)) {
    stop("stub_apptainer_runtime: no SIF path -- did you call local_hpc_mode() first?",
         call. = FALSE)
  }
  mockery::stub(target_fn, "assert_runtime_available",
                function(runtime) {
                  if (identical(runtime, "apptainer")) invisible("apptainer")
                  else if (identical(runtime, "docker")) invisible("docker")
                  else stop("stub_apptainer_runtime: unknown runtime '", runtime, "'")
                })
  mockery::stub(target_fn, "resolve_sif",
                function() normalizePath(sif_path, winslash = "/", mustWork = TRUE))
  mockery::stub(target_fn, "mstargetr_find_apptainer",
                function() "apptainer")
  mockery::stub(target_fn, "check_docker", function() TRUE)
  invisible(NULL)
}

#' Drop a known-good fixture into a plate output dir for the next stage
#'
#' The msConvertR -> PeakForgeR -> qcCheckR -> batchCorrectR chain expects each
#' stage to leave outputs in a specific subtree. When the upstream stage is
#' mocked (no real container ran), this helper fakes its outputs so the next
#' stage has something to consume.
#'
#' @param stage one of `"msConvertR"`, `"PeakForgeR"`.
#' @param plate_id sanitised plate identifier.
#' @param output_directory project output root (the `output_directory` arg the
#'   real pipeline receives).
#' @return absolute path to the file that was created.
stub_pipeline_output <- function(stage, plate_id, output_directory) {
  stopifnot(is.character(stage), length(stage) == 1L,
            is.character(plate_id), length(plate_id) == 1L,
            is.character(output_directory), length(output_directory) == 1L)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  if (identical(stage, "msConvertR")) {
    mzml_dir <- file.path(output_directory, plate_id, "data", "mzml")
    dir.create(mzml_dir, recursive = TRUE, showWarnings = FALSE)
    stub_path <- file.path(mzml_dir, paste0(plate_id, "_sample1.mzML"))
    writeLines(
      c('<?xml version="1.0" encoding="ISO-8859-1"?>',
        '<!-- stub mzML for MStargetR HPC tests -->',
        '<mzML xmlns="http://psi.hupo.org/ms/mzml" version="1.1.0"/>'),
      con = stub_path
    )
    return(normalizePath(stub_path, winslash = "/", mustWork = TRUE))
  }

  if (identical(stage, "PeakForgeR")) {
    pf_dir <- file.path(output_directory, plate_id, "data", "PeakForgeR")
    dir.create(pf_dir, recursive = TRUE, showWarnings = FALSE)
    src <- system.file("extdata", "Example_PeakForgeR_report.csv",
                       package = "MStargetR")
    if (!nzchar(src) || !file.exists(src)) {
      stop("stub_pipeline_output: Example_PeakForgeR_report.csv not found in ",
           "inst/extdata/. Run devtools::load_all() so system.file() resolves.",
           call. = FALSE)
    }
    dest <- file.path(pf_dir, paste0(plate_id, "_PeakForgeR_report.csv"))
    file.copy(src, dest, overwrite = TRUE)
    return(normalizePath(dest, winslash = "/", mustWork = TRUE))
  }

  stop("stub_pipeline_output: unknown stage '", stage,
       "'. Expected one of: msConvertR, PeakForgeR.", call. = FALSE)
}

#' Build a minimal plate-grouping table (as derive_plate_groups() returns)
#'
#' Used to stub `derive_plate_groups()` inside `msConvertR()` for HPC front-door
#' dispatch tests. Defaults to flat .wiff plates (plate_level == TRUE) so the
#' refuse-and-prompt guard never fires.
#'
#' @param plates character vector of plate IDs.
#' @return a data.frame with the columns msConvertR expects.
hpc_fake_groups <- function(plates = "plate1") {
  data.frame(
    raw_path          = file.path("raw_data", paste0(plates, ".wiff")),
    file_name         = paste0(plates, ".wiff"),
    rel_dir           = "",
    raw_plateID       = plates,
    sanitized_plateID = plates,
    is_dir            = FALSE,
    plate_level       = TRUE,
    source            = "flat",
    stringsAsFactors  = FALSE
  )
}

#' Read a per-plate MStargetR log file
#'
#' Returns the log contents as a character vector. Used by tests that want to
#' assert the runtime field, the captured Apptainer command, or the success
#' marker landed in the log produced by the futures path.
#'
#' @param output_directory project output root.
#' @param plate_id sanitised plate identifier.
#' @return character vector of log lines, or NULL if the log does not exist.
read_pipeline_log <- function(output_directory, plate_id) {
  log_path <- file.path(output_directory, "MStargetR_logs",
                        paste0(plate_id, "_MStargetR_log.txt"))
  if (!file.exists(log_path)) return(NULL)
  readLines(log_path, warn = FALSE)
}
