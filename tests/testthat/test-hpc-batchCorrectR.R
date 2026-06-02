# tests/testthat/test-hpc-batchCorrectR.R
#
# Verifies two orthogonal properties:
#   1. batchCorrectR is invariant to enable_HPC (it is pure R; the option has
#      no effect on this stage).
#   2. export_master_list_qs() produces a readable .qs2 file end-to-end
#      (qs2 migration path, not the legacy .rda path).
#
# Shared helpers (auto-sourced by testthat):
#   helper-hpc-mocks.R  -- local_hpc_mode(), new_apptainer_recorder(), etc.
#   helper-fixtures.R   -- make_bc_data()
#
# DO NOT edit R/ source. Pure test infrastructure only.

library(mockery)

# ---------------------------------------------------------------------------
# Utility: build the minimal master_list skeleton that export_master_list_qs
# requires.  The function reads:
#   master_list$project_details$project_dir   -- output root
#   master_list$project_details$project_name  -- used in filename
#   master_list$project_details$script_log    -- passed to update_script_log
# We stub update_script_log in every export test so the script_log content
# only needs to satisfy the is.list() check inside update_script_log itself.
# ---------------------------------------------------------------------------
make_minimal_master_list <- function(project_dir, project_name = "hpc_test") {
  list(
    project_details = list(
      project_dir  = project_dir,
      project_name = project_name,
      script_log   = list(
        timestamps = list(
          plot_generation = Sys.time()
        ),
        runtimes   = list(),
        messages   = list()
      )
    ),
    payload = list(a = 1L, b = "hello", c = TRUE)
  )
}

# ---------------------------------------------------------------------------
# Helper: run batchCorrectR on make_bc_data() with all chatty side-effects
# suppressed and no file I/O.  Uses ComBat so there are no statTarget temp
# files (deterministic, fast, no QC-sample bookending required).
#
# All rendering/IO helpers are stubbed:
#   bc_plot_correction_results -- would create ggplot objects
#   bc_generate_correction_report -- would generate text
#   bc_export_html_report -- would write HTML to disk
#
# Returns the raw batchCorrectR result list.
# ---------------------------------------------------------------------------
run_bc_stubbed <- function(df) {
  stub(batchCorrectR, "bc_plot_correction_results",
       function(...) list())
  stub(batchCorrectR, "bc_generate_correction_report",
       function(...) "stubbed_report")
  stub(batchCorrectR, "bc_export_html_report",
       function(...) NA_character_)

  suppressMessages(suppressWarnings(
    batchCorrectR(
      data    = df,
      method  = "ComBat",
      plot    = FALSE,
      report  = FALSE
    )
  ))
}

# ===========================================================================
# Test 1: batchCorrectR has NO enable_HPC argument
# ===========================================================================
test_that("batchCorrectR has no enable_HPC formal argument", {
  formals_names <- names(formals(batchCorrectR))

  expect_false(
    "enable_HPC" %in% formals_names,
    label = paste0(
      "batchCorrectR() MUST NOT declare enable_HPC as a formal argument. ",
      "HPC mode is controlled via options(MStargetR.enable_HPC) and must ",
      "not perturb pure-R stages. Found formals: ",
      paste(formals_names, collapse = ", ")
    )
  )
})

# ===========================================================================
# Test 2: batchCorrectR result is identical regardless of enable_HPC setting
# ===========================================================================
test_that("batchCorrectR output is identical under enable_HPC TRUE vs FALSE", {
  df <- make_bc_data(n_samples = 20, n_batches = 2, n_qc_per_batch = 4)

  # Run 1: HPC disabled (default)
  withr::with_options(list(MStargetR.enable_HPC = FALSE), {
    result_no_hpc <- run_bc_stubbed(df)
  })

  # Run 2: HPC enabled via local_hpc_mode()
  local({
    local_hpc_mode()
    result_hpc <- run_bc_stubbed(df)

    # Both must succeed and return a list
    expect_true(is.list(result_no_hpc))
    expect_true(is.list(result_hpc))

    # corrected_data should be identical (ComBat is deterministic for fixed
    # input; no timestamps or run-order-dependent columns in make_bc_data
    # default output)
    expect_equal(
      result_hpc$corrected_data,
      result_no_hpc$corrected_data,
      label = "corrected_data differs between HPC TRUE and HPC FALSE runs"
    )

    # correction_summary should also be identical
    expect_equal(
      result_hpc$correction_summary,
      result_no_hpc$correction_summary,
      label = "correction_summary differs between HPC TRUE and HPC FALSE runs"
    )

    # QC RSD vectors must match
    expect_equal(
      result_hpc$qc_rsd_before,
      result_no_hpc$qc_rsd_before,
      label = "qc_rsd_before differs between HPC TRUE and HPC FALSE runs"
    )
    expect_equal(
      result_hpc$qc_rsd_after,
      result_no_hpc$qc_rsd_after,
      label = "qc_rsd_after differs between HPC TRUE and HPC FALSE runs"
    )
  })
})

# ===========================================================================
# Test 3: export_master_list_qs round-trip via qs2
# The returned master_list must survive qs2::qs_save -> qs2::qs_read intact.
# ===========================================================================
test_that("export_master_list_qs round-trips a master_list via qs2", {
  tmp <- withr::local_tempdir()
  ml  <- make_minimal_master_list(project_dir = tmp, project_name = "roundtrip")

  # Stub update_script_log so we do not need a fully-realised script_log
  stub(export_master_list_qs, "update_script_log", function(ml, ...) ml)

  result <- suppressMessages(
    export_master_list_qs(ml, qs_nthreads = 1L, qs_compress_level = 1L)
  )

  # The function must return the (possibly updated) master_list
  expect_true(is.list(result))

  # Locate the written .qs2 file
  qs2_files <- list.files(tmp, pattern = "\\.qs2$", recursive = TRUE,
                          full.names = TRUE)
  expect_length(qs2_files, 1L)

  # Read it back and compare payload to the original
  read_back <- qs2::qs_read(qs2_files[[1L]])
  expect_true(is.list(read_back))

  # The payload slot must survive the round-trip intact
  expect_equal(
    read_back$payload,
    ml$payload,
    label = "payload differs after qs2 round-trip"
  )

  # project_details must also be preserved
  expect_equal(
    read_back$project_details$project_name,
    ml$project_details$project_name,
    label = "project_name differs after qs2 round-trip"
  )
})

# ===========================================================================
# Test 4: export_master_list_qs produces NO .rda sidecar
# A lingering base::save() call would indicate the qs2 migration is incomplete.
# ===========================================================================
test_that("export_master_list_qs does not write any .rda files", {
  tmp <- withr::local_tempdir()
  ml  <- make_minimal_master_list(project_dir = tmp, project_name = "no_rda")

  stub(export_master_list_qs, "update_script_log", function(ml, ...) ml)

  suppressMessages(
    export_master_list_qs(ml, qs_nthreads = 1L, qs_compress_level = 1L)
  )

  rda_files <- list.files(tmp, pattern = "\\.rda$", recursive = TRUE,
                          full.names = TRUE)
  expect_equal(
    length(rda_files), 0L,
    info = paste0(
      "Found unexpected .rda file(s) alongside the .qs2 output -- ",
      "the base::save() migration to qs2 is incomplete. Files: ",
      paste(rda_files, collapse = ", ")
    )
  )
})

# ===========================================================================
# Test 5: batchCorrectR + qs2 export works end-to-end under HPC mode
# Integration smoke: HPC mode must not disturb the post-container pure-R path.
# ===========================================================================
test_that("batchCorrectR and export_master_list_qs work together under HPC mode", {
  local({
    local_hpc_mode()

    tmp <- withr::local_tempdir()
    df  <- make_bc_data(n_samples = 20, n_batches = 2, n_qc_per_batch = 4)

    # Run batch correction under HPC mode
    bc_result <- run_bc_stubbed(df)

    expect_true(is.list(bc_result))
    expect_true(!is.null(bc_result$corrected_data))

    # Build a master_list whose payload is the corrected_data tibble so the
    # round-trip assertion is meaningful
    ml <- make_minimal_master_list(project_dir = tmp, project_name = "hpc_smoke")
    ml$batch_correction <- bc_result[c("corrected_data", "correction_summary")]

    stub(export_master_list_qs, "update_script_log", function(ml, ...) ml)

    result <- suppressMessages(
      export_master_list_qs(ml, qs_nthreads = 1L, qs_compress_level = 1L)
    )

    # .qs2 file must exist
    qs2_files <- list.files(tmp, pattern = "\\.qs2$", recursive = TRUE,
                            full.names = TRUE)
    expect_length(qs2_files, 1L)

    # Round-trip the file
    read_back <- qs2::qs_read(qs2_files[[1L]])
    expect_true(is.list(read_back))

    # batch_correction slot must survive
    expect_equal(
      read_back$batch_correction$corrected_data,
      bc_result$corrected_data,
      label = "corrected_data differs after HPC-mode qs2 round-trip"
    )

    # No .rda files
    rda_files <- list.files(tmp, pattern = "\\.rda$", recursive = TRUE,
                            full.names = TRUE)
    expect_length(rda_files, 0L)
  })
})
