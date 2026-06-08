# tests/testthat/test-hpc-e2e.R
#
# Wave-2 end-to-end chained HPC test.
#
# Threads all four pipeline stages together under HPC mode with the Apptainer
# dispatch mocked. The goal is to prove the chain -- msConvertR fixtures ->
# PeakForgeR fixtures -> qcCheckR import -> batchCorrectR -> qs2 export --
# completes without error and that the .qs2 file round-trips cleanly.
#
# Shared helpers (local_hpc_mode, new_apptainer_recorder, stub_pipeline_output,
# read_pipeline_log) are auto-loaded from helper-hpc-mocks.R.
# make_bc_data() is auto-loaded from helper-fixtures.R.
#
# Wave-1 findings captured here:
#   - export_master_list_qs takes (master_list, qs_nthreads, qs_compress_level)
#     and derives the output path from master_list$project_details$project_dir
#   - qcCheckR is pure R; enable_HPC has no effect on its logic
#   - batchCorrectR is pure R; enable_HPC has no effect on its logic
#   - msConvertR per-plate logs: Runtime: Apptainer
#   - PeakForgeR per-plate logs: Status: SUCCESS (NOT Runtime: Apptainer)

library(mockery)

# ---------------------------------------------------------------------------
# Test 1: Chained pipeline smoke -- fixtures -> qcCheckR -> batchCorrectR
#         -> qs2 export, all under HPC mode with mocked dispatch
# ---------------------------------------------------------------------------
test_that("end-to-end pipeline: fixture chain completes under HPC mode, qs2 round-trips", {
  suppressMessages({
    tmp <- withr::local_tempdir()
    local_hpc_mode()

    # ---- Stage 1: msConvertR -- drop mzML stubs for two plates ----
    # The wave-1 test-hpc-msConvertR.R covers the front-door dispatch;
    # here we focus on the chain so we drop fixtures directly.
    plate_ids <- c("plateA", "plateB")
    for (pid in plate_ids) {
      stub_pipeline_output("msConvertR", pid, tmp)
    }

    mzml_files <- list.files(tmp, pattern = "\\.mzML$", recursive = TRUE)
    expect_length(mzml_files, 2L)

    # ---- Stage 2: PeakForgeR -- drop PeakForgeR report stubs ----
    for (pid in plate_ids) {
      stub_pipeline_output("PeakForgeR", pid, tmp)
    }

    pf_files <- list.files(tmp, pattern = "_PeakForgeR_report\\.csv$",
                           recursive = TRUE)
    expect_length(pf_files, 2L)

    # ---- Stage 3: qcCheckR -- run the real entry point on the fixture tree ----
    # Stub the heavy downstream sub-steps so the test is fast, but let
    # qcCheckR_setup_project run for real to exercise file discovery + CSV
    # import (the same pattern used in test-hpc-qcCheckR.R test 3).
    stub(qcCheckR, "qcCheckR_transpose_data",                   function(ml) ml)
    stub(qcCheckR, "qcCheckR_sort_data",                        function(ml, ...) ml)
    stub(qcCheckR, "qcCheckR_impute_data",                      function(ml) ml)
    stub(qcCheckR, "qcCheckR_calculate_response_concentration",  function(ml) ml)
    stub(qcCheckR, "qcCheckR_statTarget_batch_correction",       function(ml) ml)
    stub(qcCheckR, "qcCheckR_set_qc",                           function(ml) ml)
    stub(qcCheckR, "qcCheckR_sample_filter",                    function(ml) ml)
    stub(qcCheckR, "qcCheckR_sil_IntStd_filter",                function(ml) ml)
    stub(qcCheckR, "qcCheckR_lipid_filter",                     function(ml) ml)
    stub(qcCheckR, "qcCheckR_RSD_filter",                       function(ml) ml)
    stub(qcCheckR, "qcCheckR_summary_report",                   function(ml) ml)
    stub(qcCheckR, "qcCheckR_plot_options",                     function(ml) ml)
    stub(qcCheckR, "qcCheckR_PCA",                              function(ml) ml)
    stub(qcCheckR, "qcCheckR_run_order_plots",                  function(ml) ml)
    stub(qcCheckR, "qcCheckR_target_control_charts",            function(ml) ml)
    stub(qcCheckR, "qcCheckR_export_all",                       function(ml, ...) ml)

    qc_result <- qcCheckR(
      user_name         = "ANPC",
      project_directory = tmp,
      write_rda         = FALSE
    )

    expect_type(qc_result, "list")
    # qcCheckR_setup_project should have imported both fixture plates
    expect_true(length(qc_result$data$PeakForgeRReport) >= 2L,
                label = "qcCheckR must import >= 2 PeakForgeR fixture reports")

    # ---- Stage 4: batchCorrectR -- use make_bc_data() as tabular input ----
    # The qcCheckR -> batchCorrectR handoff in the real pipeline passes
    # processed peak-area data; here we use make_bc_data() directly (the
    # same approach the wave-1 batchCorrectR test uses) to isolate the
    # correction stage from the qcCheckR output shape.
    df <- make_bc_data(n_samples = 20, n_batches = 2, n_qc_per_batch = 4)

    stub(batchCorrectR, "bc_plot_correction_results",
         function(...) list())
    stub(batchCorrectR, "bc_generate_correction_report",
         function(...) "stubbed_report")
    stub(batchCorrectR, "bc_export_html_report",
         function(...) NA_character_)

    bc_result <- suppressWarnings(
      batchCorrectR(
        data   = df,
        method = "ComBat",
        plot   = FALSE,
        report = FALSE
      )
    )

    expect_type(bc_result, "list")
    expect_false(is.null(bc_result$corrected_data),
                 label = "batchCorrectR must produce corrected_data")

    # ---- Stage 5: qs2 export round-trip ----
    # Build a master_list that carries both stage results so the qs2 file
    # is representative. project_dir must point to tmp so
    # export_master_list_qs can derive the output path.
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
      qcCheckR      = qc_result,
      batchCorrectR = bc_result[c("corrected_data", "correction_summary")]
    )

    stub(export_master_list_qs, "update_script_log", function(ml, ...) ml)

    export_result <- export_master_list_qs(
      master_list,
      qs_nthreads       = 1L,
      qs_compress_level = 1L
    )

    expect_type(export_result, "list")

    # The .qs2 file must exist and contain no companion .rda
    qs2_files <- list.files(tmp, pattern = "\\.qs2$", recursive = TRUE,
                            full.names = TRUE)
    expect_length(qs2_files, 1L)

    rda_files <- list.files(tmp, pattern = "\\.rda$", recursive = TRUE)
    expect_length(rda_files, 0L)

    # Round-trip: project_name and corrected_data must survive intact
    round <- qs2::qs_read(qs2_files[[1L]])
    expect_equal(round$project_details$project_name, "hpc_smoke")
    expect_equal(
      round$batchCorrectR$corrected_data,
      bc_result$corrected_data,
      label = "corrected_data must survive qs2 round-trip"
    )
  })
})

# ---------------------------------------------------------------------------
# Test 2: Apptainer recorder captures one call per plate via msConvertR front door
#
# Drives msConvertR_execute_command directly (the same layer test-hpc-msConvertR.R
# test 5 uses) with a new_apptainer_recorder() and asserts the recorder logged
# calls for both plates under the E2E HPC context.
# ---------------------------------------------------------------------------
test_that("E2E: Apptainer recorder logs one call per plate for all plates in chain", {
  suppressMessages({
    local_hpc_mode()

    tmp <- withr::local_tempdir()
    plate_ids <- c("e2e_plateA", "e2e_plateB")

    # Build a commands list that mirrors what msConvertR_mzml_conversion
    # would produce for these two plates
    commands <- lapply(plate_ids, function(pid) {
      host_in  <- normalizePath(tmp, winslash = "/", mustWork = TRUE)
      host_out <- normalizePath(tmp, winslash = "/", mustWork = TRUE)
      list(
        image_command     = c("wine", "msconvert",
                              paste0("/data/", pid, ".wiff"), "-o", "/output"),
        binds             = list(
          list(host = host_in,  container = "/data",   ro = TRUE),
          list(host = host_out, container = "/output", ro = FALSE)
        ),
        docker_extra_args = NULL,
        docker_args       = NULL,
        saneID            = pid,
        junctions         = list()
      )
    })
    attr(commands, "active_plateIDs") <- plate_ids

    # Pre-create mzML stubs so the post-run verification inside
    # msConvertR_execute_command finds output files and does not error
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

    expect_no_error(
      msConvertR_execute_command(commands, tmp, plate_ids, enable_HPC = TRUE)
    )

    calls <- recorder$calls()

    # One call per plate
    expect_length(calls, length(plate_ids))

    # Every call must have enable_HPC = TRUE
    for (call in calls) {
      expect_true(isTRUE(call$enable_HPC),
                  label = "Every recorder call must have enable_HPC = TRUE")
    }

    # Both plates must appear in the image_command arguments
    image_cmds <- vapply(calls, function(c)
      paste(c$image_command, collapse = " "), character(1))

    for (pid in plate_ids) {
      expect_true(
        any(grepl(pid, image_cmds, fixed = TRUE)),
        label = paste("plate", pid, "must appear in at least one recorded command")
      )
    }

    # Per-plate logs must exist (msConvertR writes Runtime: Apptainer)
    for (pid in plate_ids) {
      log_lines <- read_pipeline_log(tmp, pid)
      expect_false(is.null(log_lines),
                   label = paste("log must exist for", pid))
      expect_true(any(grepl("Runtime: Apptainer", log_lines)),
                  label = paste("log must contain 'Runtime: Apptainer' for", pid))
    }
  })
})
