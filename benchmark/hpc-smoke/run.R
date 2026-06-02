#!/usr/bin/env Rscript
# benchmark/hpc-smoke/run.R
#
# Chained HPC-mode smoke test of the MStargetR pipeline.
# Mocks the Apptainer dispatch so this can run on any Windows/Linux box
# without a SIF file or real cluster access.
#
# Writes benchmark/hpc-smoke/summary.md alongside this script.
# Exits with status 0 on success, 1 on any stage failure.
#
# Usage (from repo root):
#   Rscript benchmark/hpc-smoke/run.R

suppressPackageStartupMessages({
  library(devtools)
  load_all(".", quiet = TRUE)
  library(testthat)
  library(mockery)
  library(withr)
  library(qs2)
})

source("tests/testthat/helper-hpc-mocks.R")
source("tests/testthat/helper-fixtures.R")

# ---------------------------------------------------------------------------
# Record-stage scaffolding
# ---------------------------------------------------------------------------
results    <- list()
mock_counts <- list()

record_stage <- function(name, fn) {
  t0  <- proc.time()["elapsed"]
  ok  <- tryCatch({
    fn()
    TRUE
  }, error = function(e) {
    elapsed <- proc.time()["elapsed"] - t0
    results[[name]] <<- list(ok = FALSE, msg = conditionMessage(e),
                             elapsed = as.numeric(elapsed),
                             mock_calls = mock_counts[[name]])
    FALSE
  })
  if (isTRUE(ok)) {
    elapsed <- proc.time()["elapsed"] - t0
    results[[name]] <<- list(ok = TRUE, msg = NA_character_,
                             elapsed = as.numeric(elapsed),
                             mock_calls = mock_counts[[name]])
  }
}

# ---------------------------------------------------------------------------
# Shared temporary directory for the whole smoke run
# ---------------------------------------------------------------------------
tmp <- tempfile(pattern = "mstargetr-hpc-smoke-")
dir.create(tmp, recursive = TRUE)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

plate_ids <- c("smokeA", "smokeB")

# ---------------------------------------------------------------------------
# Stage 1: msConvertR -- Apptainer dispatch via recorder
# ---------------------------------------------------------------------------
record_stage("msConvertR", function() {
  suppressMessages(local_hpc_mode(envir = parent.frame()))

  # Drop mzML stubs into the project tree
  for (pid in plate_ids) {
    stub_pipeline_output("msConvertR", pid, tmp)
  }

  # Build a minimal commands list
  commands <- lapply(plate_ids, function(pid) {
    host_path <- normalizePath(tmp, winslash = "/", mustWork = TRUE)
    list(
      image_command     = c("wine", "msconvert",
                            paste0("/data/", pid, ".wiff"), "-o", "/output"),
      binds             = list(
        list(host = host_path, container = "/data",   ro = TRUE),
        list(host = host_path, container = "/output", ro = FALSE)
      ),
      docker_extra_args = NULL,
      docker_args       = NULL,
      saneID            = pid,
      junctions         = list()
    )
  })
  attr(commands, "active_plateIDs") <- plate_ids

  # Pre-create mzML outputs so post-run verification passes
  for (pid in plate_ids) {
    mzml_dir <- file.path(tmp, pid, "data", "mzml")
    dir.create(mzml_dir, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(mzml_dir, paste0(pid, "_s1.mzML")))
  }

  recorder <- new_apptainer_recorder(exit_status = 0L)

  stub(msConvertR_execute_command, "run_container", recorder$fn)
  stub(msConvertR_execute_command, "resolve_sif",
       function() getOption("MStargetR.sif_path"))
  stub(msConvertR_execute_command, "future::plan",
       function(...) invisible(NULL))
  stub(msConvertR_execute_command, "future::availableCores",
       function() 4L)

  suppressMessages(
    msConvertR_execute_command(commands, tmp, plate_ids, enable_HPC = TRUE)
  )

  calls <- recorder$calls()
  if (length(calls) != length(plate_ids)) {
    stop(sprintf("Expected %d run_container calls, got %d",
                 length(plate_ids), length(calls)))
  }

  mock_counts[["msConvertR"]] <<- length(calls)
})

# ---------------------------------------------------------------------------
# Stage 2: PeakForgeR -- drop fixture reports, assert future_lapply intercepted
# ---------------------------------------------------------------------------
record_stage("PeakForgeR", function() {
  suppressMessages(local_hpc_mode(envir = parent.frame()))

  fake_sif <- getOption("MStargetR.sif_path")

  # Drop PeakForgeR CSV fixtures for both plates
  for (pid in plate_ids) {
    stub_pipeline_output("PeakForgeR", pid, tmp)
  }

  future_lapply_calls <- 0L

  stub(PeakForgeR, "validate_project_directory",  function(x) x)
  stub(PeakForgeR, "validate_mrm_template_list",  function(x, y) x)
  stub(PeakForgeR, "assert_runtime_available",
       function(runtime) invisible(runtime))
  stub(PeakForgeR, "resolve_sif", function() fake_sif)
  stub(PeakForgeR, "future::plan", function(...) invisible(NULL))
  stub(PeakForgeR, "future::availableCores", function() 4L)
  stub(PeakForgeR, "future.apply::future_lapply",
       function(plateIDs, fn, ...) {
         future_lapply_calls <<- future_lapply_calls + 1L
         lapply(plateIDs, function(pid) list(success = TRUE, plateID = pid))
       })
  stub(PeakForgeR, "archive_raw_files", function(...) NULL)

  suppressMessages(
    PeakForgeR(
      user_name         = "smoke_user",
      project_directory = tmp,
      mrm_template_list = list("stub.tsv"),
      QC_sample_label   = "QC",
      plateID_outputs   = plate_ids,
      enable_HPC        = TRUE
    )
  )

  mock_counts[["PeakForgeR"]] <<- future_lapply_calls
})

# ---------------------------------------------------------------------------
# Stage 3: qcCheckR -- consume PeakForgeR fixture reports
# ---------------------------------------------------------------------------
record_stage("qcCheckR", function() {
  suppressMessages(local_hpc_mode(envir = parent.frame()))

  stub(qcCheckR, "qcCheckR_transpose_data",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_sort_data",
       function(ml, ...) ml)
  stub(qcCheckR, "qcCheckR_impute_data",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_calculate_response_concentration",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_statTarget_batch_correction",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_set_qc",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_sample_filter",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_sil_IntStd_filter",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_lipid_filter",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_RSD_filter",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_summary_report",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_plot_options",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_PCA",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_run_order_plots",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_target_control_charts",
       function(ml) ml)
  stub(qcCheckR, "qcCheckR_export_all",
       function(ml, ...) ml)

  qc_result <- suppressMessages(
    qcCheckR(
      user_name         = "ANPC",
      project_directory = tmp,
      write_rda         = FALSE
    )
  )

  if (!is.list(qc_result)) {
    stop("qcCheckR did not return a list")
  }
  if (length(qc_result$data$PeakForgeRReport) < 2L) {
    stop(sprintf(
      "Expected >= 2 PeakForgeRReport entries, got %d",
      length(qc_result$data$PeakForgeRReport)
    ))
  }

  # Stash for stage 4
  assign("qc_result_smoke", qc_result, envir = globalenv())
  mock_counts[["qcCheckR"]] <<- 0L   # no mocked container calls (pure R)
})

# ---------------------------------------------------------------------------
# Stage 4: batchCorrectR + qs2 export
# ---------------------------------------------------------------------------
record_stage("batchCorrectR + qs2 export", function() {
  suppressMessages(local_hpc_mode(envir = parent.frame()))

  df <- make_bc_data(n_samples = 20, n_batches = 2, n_qc_per_batch = 4)

  stub(batchCorrectR, "bc_plot_correction_results",
       function(...) list())
  stub(batchCorrectR, "bc_generate_correction_report",
       function(...) "stubbed_report")
  stub(batchCorrectR, "bc_export_html_report",
       function(...) NA_character_)

  bc_result <- suppressMessages(suppressWarnings(
    batchCorrectR(
      data   = df,
      method = "ComBat",
      plot   = FALSE,
      report = FALSE
    )
  ))

  if (!is.list(bc_result)) {
    stop("batchCorrectR did not return a list")
  }
  if (is.null(bc_result$corrected_data)) {
    stop("batchCorrectR$corrected_data is NULL")
  }

  # Build the master_list for qs2 export
  master_list <- list(
    project_details = list(
      project_dir  = tmp,
      project_name = "hpc_smoke",
      script_log   = list(
        timestamps = list(plot_generation = Sys.time()),
        runtimes   = list(),
        messages   = list()
      )
    ),
    batchCorrectR = bc_result[c("corrected_data", "correction_summary")]
  )

  stub(export_master_list_qs, "update_script_log", function(ml, ...) ml)

  suppressMessages(
    export_master_list_qs(master_list, qs_nthreads = 1L, qs_compress_level = 1L)
  )

  qs2_files <- list.files(tmp, pattern = "\\.qs2$", recursive = TRUE,
                          full.names = TRUE)
  if (length(qs2_files) == 0L) {
    stop("No .qs2 file found after export_master_list_qs()")
  }

  round <- qs2::qs_read(qs2_files[[1L]])
  if (!identical(round$project_details$project_name, "hpc_smoke")) {
    stop("project_name did not survive qs2 round-trip")
  }

  rda_files <- list.files(tmp, pattern = "\\.rda$", recursive = TRUE)
  if (length(rda_files) > 0L) {
    stop(paste("Unexpected .rda files found:", paste(rda_files, collapse = ", ")))
  }

  mock_counts[["batchCorrectR + qs2 export"]] <<- 0L   # pure R, no container
})

# ---------------------------------------------------------------------------
# Write summary.md
# ---------------------------------------------------------------------------
all_pass <- all(vapply(results, function(r) isTRUE(r$ok), logical(1)))
total_elapsed <- sum(vapply(results, function(r) r$elapsed, numeric(1)),
                     na.rm = TRUE)

verdict <- if (all_pass) "PASS" else "FAIL"

table_rows <- vapply(names(results), function(n) {
  r  <- results[[n]]
  mc <- if (!is.null(r$mock_calls) && !is.na(r$mock_calls))
          as.character(r$mock_calls) else "n/a"
  status <- if (isTRUE(r$ok)) "PASS" else paste0("FAIL: ", r$msg)
  sprintf("| %s | %s | %.3f | %s |", n, status, r$elapsed, mc)
}, character(1))

summary_lines <- c(
  "# MStargetR HPC smoke run",
  "",
  paste0("Run at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  paste0("**Verdict: ", verdict, "**"),
  "",
  paste0("Total elapsed: ", round(total_elapsed, 3), " s"),
  "",
  "## Per-stage results",
  "",
  "| Stage | Result | Elapsed (s) | Mock calls |",
  "|---|---|---|---|",
  table_rows,
  "",
  "## Notes",
  "",
  "- Apptainer dispatch is fully mocked (no real SIF or cluster required).",
  "- msConvertR mock call count = number of `run_container()` invocations.",
  "- PeakForgeR mock call count = number of `future_lapply()` invocations.",
  "- qcCheckR and batchCorrectR are pure R; mock call count is always 0.",
  "- .qs2 output verified via round-trip read after export."
)

script_dir <- "benchmark/hpc-smoke"
dir.create(script_dir, recursive = TRUE, showWarnings = FALSE)
summary_path <- file.path(script_dir, "summary.md")
writeLines(summary_lines, summary_path)

cat("\n", paste(summary_lines, collapse = "\n"), "\n", sep = "")

if (!all_pass) {
  message("\nOne or more stages FAILED. See summary.md for details.")
  quit(status = 1L)
} else {
  message("\nAll stages PASSED.")
}
