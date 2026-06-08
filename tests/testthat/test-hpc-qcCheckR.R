library(mockery)

# ---------------------------------------------------------------------------
# test-hpc-qcCheckR.R
#
# Verifies that qcCheckR() is invariant to enable_HPC (it is pure R, no
# container calls) and that it can consume PeakForgeR outputs left by an
# upstream HPC-mode PeakForgeR run.
#
# Entry point under test: qcCheckR() in R/qcCheckR.R
#
# Shared helpers (local_hpc_mode, stub_pipeline_output, etc.) live in
# tests/testthat/helper-hpc-mocks.R and are auto-loaded by testthat.
#
# Note on mockery::stub() lifecycle: stubs are registered with on.exit() in
# the CALLING frame.  When applied inside a helper function they are revoked
# the moment that helper returns.  All stubs below are therefore applied
# directly inside each test_that block so they remain active for the
# qcCheckR() call that follows.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Internal helper: minimal master_list that looks like qcCheckR_setup_project
# returned successfully.  Used in tests 2 and 2b where the full pipeline
# sub-steps are stubbed out.
# ---------------------------------------------------------------------------
make_synthetic_ml <- function(project_dir) {
  list(
    environment      = list(r_version = "mock", base_packages = character(0),
                            user_packages = character(0)),
    templates        = list(mrm_guides = list()),
    project_details  = list(
      project_dir        = project_dir,
      user_name          = "ANPC",
      project_name       = basename(project_dir),
      plateIDs           = character(0),
      qc_type            = "ltr",
      sample_tags        = c("ltr", "sample"),
      mv_sample_threshold = 50,
      batch_method       = "QCRFSC",
      batch_ntree        = 500L,
      batch_coCV         = 100,
      batch_Frule        = 0,
      batch_imputeM      = "minHalf",
      combat_par.prior   = TRUE,
      combat_mean.only   = FALSE,
      combat_ref.batch   = NULL,
      qcrlsc_method      = "subtract",
      qcrlsc_intra       = FALSE,
      qcrlsc_opti        = TRUE,
      qcrlsc_log10       = TRUE,
      qcrlsc_outl        = TRUE,
      qcrlsc_shift       = TRUE,
      batch_column       = NULL,
      statTarget_qc_type = "ltr",
      script_log         = list(timestamps = list(start_time = Sys.time()))
    ),
    data             = list(PeakForgeRReport = list(), peakArea = list(),
                            concentration = list()),
    summary_tables   = list(),
    process_lists    = list()
  )
}

# ---------------------------------------------------------------------------
# Test 1: qcCheckR has no enable_HPC argument
# ---------------------------------------------------------------------------
test_that("qcCheckR has no enable_HPC argument", {
  param_names <- names(formals(qcCheckR))
  expect_false(
    "enable_HPC" %in% param_names,
    label = paste0(
      "qcCheckR() must not accept 'enable_HPC' (it is pure R, no container). ",
      "Found formals: ", paste(param_names, collapse = ", ")
    )
  )
})

# ---------------------------------------------------------------------------
# Test 2: qcCheckR return value is identical under enable_HPC FALSE vs TRUE
#
# Stubs are applied directly in the test_that block (not via a helper
# function) so the mockery on.exit cleanup fires only when the test exits,
# not earlier.
# ---------------------------------------------------------------------------
test_that("qcCheckR return value is identical under enable_HPC FALSE vs TRUE", {
  suppressMessages({
    tmp <- withr::local_tempdir()

    synthetic_ml <- make_synthetic_ml(tmp)

    # ---- Apply stubs for Run A + B (stubs persist for this test_that scope) -
    stub(qcCheckR, "qcCheckR_setup_project",          function(...) synthetic_ml)
    stub(qcCheckR, "qcCheckR_transpose_data",          function(ml) ml)
    stub(qcCheckR, "qcCheckR_sort_data",               function(ml, ...) ml)
    stub(qcCheckR, "qcCheckR_impute_data",             function(ml) ml)
    stub(qcCheckR, "qcCheckR_calculate_response_concentration",
         function(ml) ml)
    stub(qcCheckR, "qcCheckR_statTarget_batch_correction", function(ml) ml)
    stub(qcCheckR, "qcCheckR_set_qc",                 function(ml) ml)
    stub(qcCheckR, "qcCheckR_sample_filter",           function(ml) ml)
    stub(qcCheckR, "qcCheckR_sil_IntStd_filter",       function(ml) ml)
    stub(qcCheckR, "qcCheckR_lipid_filter",            function(ml) ml)
    stub(qcCheckR, "qcCheckR_RSD_filter",              function(ml) ml)
    stub(qcCheckR, "qcCheckR_summary_report",          function(ml) ml)
    stub(qcCheckR, "qcCheckR_plot_options",            function(ml) ml)
    stub(qcCheckR, "qcCheckR_PCA",                    function(ml) ml)
    stub(qcCheckR, "qcCheckR_run_order_plots",         function(ml) ml)
    stub(qcCheckR, "qcCheckR_target_control_charts",   function(ml) ml)
    stub(qcCheckR, "qcCheckR_export_all",              function(ml, ...) ml)

    # ---- Run A: enable_HPC = FALSE (default) --------------------------------
    withr::local_options(list(MStargetR.enable_HPC = FALSE))

    result_false <- qcCheckR(
      user_name         = "ANPC",
      project_directory = tmp,
      write_rda         = FALSE
    )

    # ---- Run B: enable_HPC = TRUE via local_hpc_mode() ----------------------
    # Stubs remain active from above; only the option changes.
    local_hpc_mode()

    result_true <- qcCheckR(
      user_name         = "ANPC",
      project_directory = tmp,
      write_rda         = FALSE
    )

    # Compare everything except timestamps (which encode wall-clock and
    # legitimately differ between the two qcCheckR() calls).
    strip_ts <- function(ml) {
      ml$project_details$script_log$timestamps <- NULL
      ml
    }

    expect_equal(
      strip_ts(result_false),
      strip_ts(result_true),
      info = paste0(
        "qcCheckR() must produce identical output regardless of ",
        "MStargetR.enable_HPC, because the function never branches on that option."
      )
    )
  })
})

# ---------------------------------------------------------------------------
# Test 3: qcCheckR consumes a PeakForgeR-HPC fixture cleanly
#
# stub_pipeline_output() places the example PeakForgeR CSV for two plates
# into the standard location inside the project tree.  local_hpc_mode()
# activates HPC mode.  We let qcCheckR_setup_project run for real so the
# file-discovery + CSV import code executes against the fixture, but stub all
# downstream computation to keep the test fast.
# ---------------------------------------------------------------------------
test_that("qcCheckR imports PeakForgeR-HPC fixtures without error in HPC mode", {
  suppressMessages({
    tmp <- withr::local_tempdir()

    # Place the example PeakForgeR report for two simulated plates.
    # Files land at:
    #   <tmp>/plate_A/data/PeakForgeR/plate_A_PeakForgeR_report.csv
    #   <tmp>/plate_B/data/PeakForgeR/plate_B_PeakForgeR_report.csv
    stub_pipeline_output("PeakForgeR", "plate_A", tmp)
    stub_pipeline_output("PeakForgeR", "plate_B", tmp)

    # Activate HPC mode (sets option + sequential future plan).
    local_hpc_mode()

    # Stub everything downstream of qcCheckR_setup_project so the test
    # completes without touching real batch correction, plots, or exports.
    # qcCheckR_setup_project is intentionally NOT stubbed: we want its real
    # file-discovery + CSV import logic to run against the fixture tree.
    stub(qcCheckR, "qcCheckR_transpose_data",          function(ml) ml)
    stub(qcCheckR, "qcCheckR_sort_data",               function(ml, ...) ml)
    stub(qcCheckR, "qcCheckR_impute_data",             function(ml) ml)
    stub(qcCheckR, "qcCheckR_calculate_response_concentration",
         function(ml) ml)
    stub(qcCheckR, "qcCheckR_statTarget_batch_correction", function(ml) ml)
    stub(qcCheckR, "qcCheckR_set_qc",                 function(ml) ml)
    stub(qcCheckR, "qcCheckR_sample_filter",           function(ml) ml)
    stub(qcCheckR, "qcCheckR_sil_IntStd_filter",       function(ml) ml)
    stub(qcCheckR, "qcCheckR_lipid_filter",            function(ml) ml)
    stub(qcCheckR, "qcCheckR_RSD_filter",              function(ml) ml)
    stub(qcCheckR, "qcCheckR_summary_report",          function(ml) ml)
    stub(qcCheckR, "qcCheckR_plot_options",            function(ml) ml)
    stub(qcCheckR, "qcCheckR_PCA",                    function(ml) ml)
    stub(qcCheckR, "qcCheckR_run_order_plots",         function(ml) ml)
    stub(qcCheckR, "qcCheckR_target_control_charts",   function(ml) ml)
    stub(qcCheckR, "qcCheckR_export_all",              function(ml, ...) ml)

    result <- expect_no_error(
      qcCheckR(
        user_name         = "ANPC",
        project_directory = tmp,
        write_rda         = FALSE
      )
    )

    # The setup should have discovered and imported both fixture plates.
    expect_true(is.list(result), label = "qcCheckR() must return a list")

    expect_true(
      length(result$data$PeakForgeRReport) >= 2L,
      label = paste0(
        "Must import at least 2 PeakForgeR reports from the HPC fixture tree. ",
        "Found: ", length(result$data$PeakForgeRReport)
      )
    )

    # Plate IDs are derived from filenames during import.
    plate_ids_found <- result$project_details$plateIDs
    expect_true(
      any(grepl("plate_A", plate_ids_found, fixed = TRUE)),
      label = paste0("plateIDs must include plate_A. Found: ",
                     paste(plate_ids_found, collapse = ", "))
    )
    expect_true(
      any(grepl("plate_B", plate_ids_found, fixed = TRUE)),
      label = paste0("plateIDs must include plate_B. Found: ",
                     paste(plate_ids_found, collapse = ", "))
    )

    # Sentinel: confirm the option was TRUE during execution.  If qcCheckR
    # ever adds enable_HPC branching that breaks invariance, this provides
    # a clear diagnostic anchor in the test log.
    expect_true(
      isTRUE(getOption("MStargetR.enable_HPC")),
      label = "MStargetR.enable_HPC must be TRUE inside the HPC fixture test"
    )
  })
})
