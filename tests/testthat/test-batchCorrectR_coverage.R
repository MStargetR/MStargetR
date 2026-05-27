# Additional coverage tests for batchCorrectR.R and batchCorrectR_Utils.R ----
# Targets zero-coverage lines not exercised by existing test suites.
library(mockery)

# make_bc_data() is defined in helper-fixtures.R (auto-sourced by testthat)

# ============================================================================
# bc_calculate_rsd -- real-function coverage (no stubs)
# Replaces the previous stubbed-out assertions that were tautologies
# against the mock. These now exercise the real implementation.
# ============================================================================

test_that("bc_calculate_rsd returns named numeric for real QC data", {
  df <- make_bc_data()
  rsd <- bc_calculate_rsd(df, qc_label = "qc",
                          metabolite_cols = c("metab_A", "metab_B"))
  expect_type(rsd, "double")
  expect_length(rsd, 2)
  expect_equal(names(rsd), c("metab_A", "metab_B"))
  expect_true(all(is.finite(rsd)))
  expect_true(all(rsd >= 0))
})

test_that("bc_calculate_rsd returns NA vector and warns with no QC rows", {
  df <- make_bc_data()
  df$sample_type <- "sample"  # remove all QC
  expect_warning(
    rsd <- bc_calculate_rsd(df, qc_label = "qc",
                            metabolite_cols = c("metab_A", "metab_B")),
    "No QC samples"
  )
  expect_equal(names(rsd), c("metab_A", "metab_B"))
  expect_true(all(is.na(rsd)))
})

test_that("bc_calculate_rsd returns NA for single-QC or zero-mean columns", {
  df <- make_bc_data()
  # Force only one QC row
  qc_idx <- which(df$sample_type == "qc")
  df$sample_type[qc_idx[-1]] <- "sample"
  rsd <- bc_calculate_rsd(df, qc_label = "qc",
                          metabolite_cols = c("metab_A", "metab_B"))
  expect_true(all(is.na(rsd)))
})

# ============================================================================
# batchCorrectR.R -- output_dir validation (lines 158-160)
# ============================================================================

test_that("batchCorrectR rejects non-character output_dir", {
  df <- make_bc_data()
  expect_error(
    suppressMessages(batchCorrectR(data = df, output_dir = 123)),
    "output_dir.*must be a non-empty single character string"
  )
})

test_that("batchCorrectR rejects empty string output_dir", {
  df <- make_bc_data()
  expect_error(
    suppressMessages(batchCorrectR(data = df, output_dir = "")),
    "output_dir.*must be a non-empty single character string"
  )
})

test_that("batchCorrectR rejects vector output_dir", {
  df <- make_bc_data()
  expect_error(
    suppressMessages(batchCorrectR(data = df, output_dir = c("a", "b"))),
    "output_dir.*must be a non-empty single character string"
  )
})

# ============================================================================
# batchCorrectR.R -- project_dir validation (lines 164-170)
# ============================================================================

test_that("batchCorrectR rejects non-character project_dir", {
  df <- make_bc_data()
  expect_error(
    suppressMessages(batchCorrectR(data = df, project_dir = 42)),
    "project_dir.*must be a non-empty single character string or NULL"
  )
})

test_that("batchCorrectR rejects empty string project_dir", {
  df <- make_bc_data()
  expect_error(
    suppressMessages(batchCorrectR(data = df, project_dir = "")),
    "project_dir.*must be a non-empty single character string or NULL"
  )
})

test_that("batchCorrectR rejects non-existent project_dir", {
  df <- make_bc_data()
  fake_path <- file.path(tempdir(), "nonexistent_dir_coverage_test_99999")
  expect_error(
    suppressMessages(batchCorrectR(data = df, project_dir = fake_path)),
    "project_dir.*does not exist"
  )
})

# ============================================================================
# batchCorrectR.R -- plot parameter validation (lines 175-177)
# ============================================================================

test_that("batchCorrectR rejects non-logical plot parameter", {
  df <- make_bc_data()
  expect_error(
    suppressWarnings(suppressMessages(batchCorrectR(data = df, plot = "yes"))),
    "plot.*must be TRUE or FALSE"
  )
})

test_that("batchCorrectR rejects NA plot parameter", {
  df <- make_bc_data()
  expect_error(
    suppressWarnings(suppressMessages(batchCorrectR(data = df, plot = NA))),
    "plot.*must be TRUE or FALSE"
  )
})

# ============================================================================
# batchCorrectR.R -- report parameter validation (lines 181-183)
# ============================================================================

test_that("batchCorrectR rejects non-logical report parameter", {
  df <- make_bc_data()
  expect_error(
    suppressMessages(batchCorrectR(data = df, report = "yes")),
    "report.*must be TRUE or FALSE"
  )
})

test_that("batchCorrectR rejects NA report parameter", {
  df <- make_bc_data()
  expect_error(
    suppressMessages(batchCorrectR(data = df, report = NA)),
    "report.*must be TRUE or FALSE"
  )
})

# ============================================================================
# batchCorrectR.R -- ComBat path: plot generation branch (lines 258-266)
# ============================================================================

test_that("batchCorrectR ComBat path generates plots when plot=TRUE and QC present", {
  df <- make_bc_data()
  plot_called <- FALSE

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A", "metab_B"))
  # NOTE: bc_calculate_rsd runs for real against make_bc_data() (QC rows present).
  stub(batchCorrectR, "bc_run_combat", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = c("metab_A", "metab_B"),
               rsd_before = c(10, 15), rsd_after = c(5, 8),
               rsd_change = c(-5, -7), improved = c(TRUE, TRUE))
  })
  stub(batchCorrectR, "bc_plot_correction_results", function(...) {
    plot_called <<- TRUE
    list(rsd = "mock_plot")
  })
  stub(batchCorrectR, "bc_generate_correction_report", function(...) "report")

  result <- suppressMessages(suppressWarnings(
    batchCorrectR(data = df, method = "ComBat", plot = TRUE, report = FALSE)
  ))

  expect_true(plot_called)
  expect_true("plots" %in% names(result))
})

# ============================================================================
# batchCorrectR.R -- ComBat path: report generation branch (lines 272-279)
# ============================================================================

test_that("batchCorrectR ComBat path generates report when report=TRUE", {
  df <- make_bc_data()
  report_called <- FALSE

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A", "metab_B"))
  # NOTE: bc_calculate_rsd runs for real against make_bc_data() (QC rows present).
  stub(batchCorrectR, "bc_run_combat", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = c("metab_A", "metab_B"),
               rsd_before = c(10, 15), rsd_after = c(5, 8),
               rsd_change = c(-5, -7), improved = c(TRUE, TRUE))
  })
  stub(batchCorrectR, "bc_plot_correction_results", function(...) list())
  stub(batchCorrectR, "bc_generate_correction_report", function(...) {
    report_called <<- TRUE
    list(title = "report")
  })

  result <- suppressMessages(suppressWarnings(
    batchCorrectR(data = df, method = "ComBat", plot = FALSE, report = TRUE)
  ))

  expect_true(report_called)
  expect_true("report" %in% names(result))
})

# ============================================================================
# batchCorrectR.R -- ComBat path: bc_save_to_project call (line 300)
# ============================================================================

test_that("batchCorrectR ComBat path calls bc_save_to_project when project_dir given", {
  df <- make_bc_data()
  save_called <- FALSE
  temp_proj <- withr::local_tempdir()


  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A", "metab_B"))
  # NOTE: bc_calculate_rsd runs for real against make_bc_data() (QC rows present).
  stub(batchCorrectR, "bc_run_combat", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = c("metab_A", "metab_B"),
               rsd_before = c(10, 15), rsd_after = c(5, 8),
               rsd_change = c(-5, -7), improved = c(TRUE, TRUE))
  })
  stub(batchCorrectR, "bc_plot_correction_results", function(...) list())
  stub(batchCorrectR, "bc_generate_correction_report", function(...) "report")
  stub(batchCorrectR, "bc_save_to_project", function(result, project_dir) {
    save_called <<- TRUE
  })

  suppressMessages(suppressWarnings(
    batchCorrectR(data = df, method = "ComBat", plot = FALSE, report = FALSE,
                  project_dir = temp_proj)
  ))

  expect_true(save_called)
})

# ============================================================================
# batchCorrectR.R -- QCRFSC path: bc_save_to_project call (line 447)
# ============================================================================

test_that("batchCorrectR QCRFSC path calls bc_save_to_project when project_dir given", {
  df <- make_bc_data()
  save_called <- FALSE
  temp_proj <- withr::local_tempdir()

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A", "metab_B"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) list(data = df, failed_samples = character(0)))
  # NOTE: bc_calculate_rsd runs for real against make_bc_data() (QC rows present).
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) data.frame(sample = "S1"))
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(...) data.frame(X1 = rnorm(20)))
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = c("metab_A", "metab_B"),
               rsd_before = c(10, 15), rsd_after = c(5, 8))
  })
  stub(batchCorrectR, "bc_save_to_project", function(result, project_dir) {
    save_called <<- TRUE
  })

  suppressMessages(suppressWarnings(
    batchCorrectR(data = df, method = "QCRFSC", plot = FALSE, report = FALSE,
                  project_dir = temp_proj)
  ))

  expect_true(save_called)
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_validate_input: empty data (line 62)
# ============================================================================

test_that("bc_validate_input rejects zero-row data.frame", {
  df <- data.frame(
    sample_name = character(0), batch = character(0),
    sample_type = character(0), run_order = numeric(0),
    met = numeric(0), stringsAsFactors = FALSE
  )
  expect_error(
    bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, "minHalf"),
    "must not be empty"
  )
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_validate_input: bad qc_label (lines 75-76)
# ============================================================================

test_that("bc_validate_input rejects non-character qc_label", {
  df <- make_bc_data()
  expect_error(
    bc_validate_input(df, 123, "QCRFSC", 500, 10000, 0, "minHalf"),
    "qc_label.*must be a non-empty single character string"
  )
})

test_that("bc_validate_input rejects empty string qc_label", {
  df <- make_bc_data()
  expect_error(
    bc_validate_input(df, "", "QCRFSC", 500, 10000, 0, "minHalf"),
    "qc_label.*must be a non-empty single character string"
  )
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_validate_input: bad method type (lines 96-97)
# ============================================================================

test_that("bc_validate_input rejects non-character method", {
  df <- make_bc_data()
  expect_error(
    bc_validate_input(df, "qc", 999, 500, 10000, 0, "minHalf"),
    "method.*must be a single character string"
  )
})

test_that("bc_validate_input rejects non-character method", {
  df <- make_bc_data()
  expect_error(
    bc_validate_input(df, "qc", 42, 500, 10000, 0, "minHalf"),
    "method.*must be a single character string"
  )
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_validate_input: bad imputeM type (lines 111-112)
# ============================================================================

test_that("bc_validate_input rejects non-character imputeM", {
  df <- make_bc_data()
  expect_error(
    bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, 42),
    "imputeM.*must be a single character string"
  )
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_flag_failed_qc: batch with no QC (line 149)
# ============================================================================

test_that("bc_flag_failed_qc skips batch with no QC samples", {
  df <- data.frame(
    sample_name = paste0("S", 1:6),
    batch = rep(c("plate1", "plate2"), each = 3),
    sample_type = c("qc", "qc", "sample", "sample", "sample", "sample"),
    run_order = 1:6,
    met = c(100, 100, 100, 100, 100, 100),
    stringsAsFactors = FALSE
  )
  result <- bc_flag_failed_qc(df, "qc", "met")
  expect_equal(length(result$failed_samples), 0)
  expect_equal(nrow(result$data), 6)
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_plot_pca: NA imputation branch (line 489)
# ============================================================================

test_that("bc_plot_pca imputes NA values in metabolite columns", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  n <- 10
  original <- data.frame(
    sample_name = paste0("S", 1:n),
    batch = rep(c("b1", "b2"), each = n / 2),
    sample_type = rep(c("qc", "sample"), n / 2),
    run_order = 1:n,
    met_A = rnorm(n, 100, 10),
    met_B = rnorm(n, 200, 20),
    met_C = rnorm(n, 300, 30),
    stringsAsFactors = FALSE
  )
  # Introduce NAs
  original$met_A[3] <- NA
  original$met_B[7] <- NA

  corrected <- original
  for (m in c("met_A", "met_B", "met_C")) {
    corrected[[m]] <- corrected[[m]] * 1.05
  }
  corrected$met_A[3] <- NA
  corrected$met_B[7] <- NA

  result <- bc_plot_pca(original, corrected, c("met_A", "met_B", "met_C"))
  expect_s3_class(result, "ggplot")
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_run_combat: sva not installed (lines 526-527)
# ============================================================================

test_that("bc_run_combat errors when sva is not installed", {
  df <- make_bc_data()
  stub(bc_run_combat, "requireNamespace", function(...) FALSE)

  expect_error(
    bc_run_combat(df, c("metab_A", "metab_B")),
    "sva.*package is required"
  )
})

# ============================================================================
# batch_shared_utils.R -- bc_run_combat: ref.batch validation
# Catches user-supplied ref.batch values that don't exist in the data,
# replacing the previously cryptic sva::ComBat error with a clear listing
# of the available batches.
# ============================================================================

test_that("bc_run_combat errors when ref.batch is not present in data", {
  df <- make_bc_data()  # batches = plate1, plate2
  stub(bc_run_combat, "requireNamespace", function(...) TRUE)

  err <- expect_error(
    bc_run_combat(df, c("metab_A", "metab_B"),
                  ref.batch = "plate99"),
    "ref.batch"
  )
  # Error message must surface the bad value AND the available batches so the
  # user can correct their input without re-running the whole pipeline.
  expect_match(conditionMessage(err), "plate99", fixed = TRUE)
  expect_match(conditionMessage(err), "plate1", fixed = TRUE)
  expect_match(conditionMessage(err), "plate2", fixed = TRUE)
})

test_that("bc_run_combat accepts a valid ref.batch (no validation error)", {
  df <- make_bc_data()
  stub(bc_run_combat, "requireNamespace", function(...) TRUE)
  stub(bc_run_combat, "sva::ComBat", function(dat, ...) dat)

  # plate1 is a real batch -> no validation error, ComBat is reached.
  expect_no_error(
    suppressMessages(
      bc_run_combat(df, c("metab_A", "metab_B"), ref.batch = "plate1")
    )
  )
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_run_combat: NA imputation (lines 536-544)
# ============================================================================

test_that("bc_run_combat imputes NA values before ComBat", {
  df <- make_bc_data()
  df$metab_A[3] <- NA

  combat_input <- NULL
  stub(bc_run_combat, "requireNamespace", function(...) TRUE)
  stub(bc_run_combat, "sva::ComBat", function(dat, ...) {
    combat_input <<- dat
    dat
  })

  suppressMessages(
    bc_run_combat(df, c("metab_A", "metab_B"))
  )

  # The NA should have been imputed before passing to ComBat
  expect_false(any(is.na(combat_input)))
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_run_combat: zero-variance removal (lines 551-553)
# ============================================================================

test_that("bc_run_combat removes zero-variance features", {
  df <- make_bc_data()
  # Make metab_A constant (zero variance)
  df$metab_A <- 100

  combat_dat <- NULL
  stub(bc_run_combat, "requireNamespace", function(...) TRUE)
  stub(bc_run_combat, "sva::ComBat", function(dat, ...) {
    combat_dat <<- dat
    dat
  })

  suppressMessages(
    result <- bc_run_combat(df, c("metab_A", "metab_B"))
  )

  # metab_A should have been removed before ComBat
  expect_false("metab_A" %in% rownames(combat_dat))
  # But should still exist in output (unchanged original values)
  expect_true("metab_A" %in% names(result))
  expect_true(all(result$metab_A == 100))
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_save_to_project (lines 600-613)
# ============================================================================

test_that("bc_save_to_project creates batch_correction directory and writes CSVs", {
  temp_proj <- withr::local_tempdir()

  mock_result <- list(
    corrected_data = data.frame(
      sample_name = c("S1", "S2"),
      met_A = c(100, 200),
      stringsAsFactors = FALSE
    ),
    correction_summary = data.frame(
      metabolite = "met_A",
      rsd_before = 20,
      rsd_after = 10,
      stringsAsFactors = FALSE
    )
  )

  suppressMessages(
    bc_dir <- bc_save_to_project(mock_result, temp_proj)
  )

  expect_true(dir.exists(file.path(temp_proj, "batch_correction")))
  csv_files <- list.files(file.path(temp_proj, "batch_correction"), pattern = "\\.csv$")
  expect_true(length(csv_files) >= 2)
  expect_true(any(grepl("corrected_data", csv_files)))
  expect_true(any(grepl("correction_summary", csv_files)))
})

test_that("bc_save_to_project emits messages with file paths", {
  temp_proj <- withr::local_tempdir()

  mock_result <- list(
    corrected_data = data.frame(sample_name = "S1", met = 100),
    correction_summary = data.frame(metabolite = "met", rsd_before = 20, rsd_after = 10)
  )

  msgs <- capture.output(type = "message",
    bc_save_to_project(mock_result, temp_proj)
  )

  expect_true(any(grepl("Corrected data saved to", msgs)))
  expect_true(any(grepl("Correction summary saved to", msgs)))
})

test_that("bc_save_to_project returns invisible output directory path", {
  temp_proj <- withr::local_tempdir()

  mock_result <- list(
    corrected_data = data.frame(sample_name = "S1", met = 100),
    correction_summary = data.frame(metabolite = "met", rsd_before = 20, rsd_after = 10)
  )

  bc_dir <- suppressMessages(
    bc_save_to_project(mock_result, temp_proj)
  )

  expect_equal(bc_dir, file.path(temp_proj, "batch_correction"))
})

# ============================================================================
# batchCorrectR_Utils.R -- bc_generate_correction_report (lines 396-415)
# Exercised via the ComBat path above, but also test directly
# ============================================================================

test_that("bc_generate_correction_report populates top_improved and worst_affected", {
  mets <- paste0("m", 1:5)
  rsd_before <- stats::setNames(c(30, 25, 20, 15, 10), mets)
  rsd_after  <- stats::setNames(c(10, 20, 25, 18, 5), mets)
  summary_tbl <- bc_build_correction_summary(mets, rsd_before, rsd_after)

  result <- bc_generate_correction_report(
    correction_summary = summary_tbl,
    failed_qc = character(0),
    method = "ComBat",
    n_samples = 50,
    n_batches = 3,
    n_metabolites = 5
  )

  expect_true(nrow(result$top_improved) > 0)
  expect_true(nrow(result$worst_affected) > 0)
  expect_equal(result$parameters$method, "ComBat")
})
