# Tests for batchCorrectR main function ----
library(mockery)

# make_bc_data() is defined in helper-fixtures.R (auto-sourced by testthat)

# --- Input validation tests (delegated to bc_validate_input) ---

test_that("batchCorrectR rejects non-data.frame input", {
  expect_error(
    batchCorrectR(data = "not_a_df"),
    "must be a data.frame"
  )
})

test_that("batchCorrectR rejects data missing required columns", {
  bad_df <- data.frame(x = 1:5, y = 6:10)
  expect_error(
    batchCorrectR(data = bad_df),
    "Missing required column"
  )
})

test_that("batchCorrectR rejects invalid method parameter", {
  df <- make_bc_data()
  expect_error(
    batchCorrectR(data = df, method = "INVALID"),
    "Invalid 'method'"
  )
})

test_that("batchCorrectR rejects invalid ntree parameter", {
  df <- make_bc_data()
  expect_error(
    batchCorrectR(data = df, ntree = -5),
    "'ntree' must be a positive integer"
  )
  expect_error(
    batchCorrectR(data = df, ntree = "abc"),
    "'ntree' must be a positive integer"
  )
})

test_that("batchCorrectR rejects invalid coCV parameter", {
  df <- make_bc_data()
  expect_error(
    batchCorrectR(data = df, coCV = -1),
    "'coCV' must be a positive number"
  )
})

test_that("batchCorrectR rejects Frule outside 0-1", {
  df <- make_bc_data()
  expect_error(
    batchCorrectR(data = df, Frule = 2),
    "'Frule' must be between 0 and 1"
  )
  expect_error(
    batchCorrectR(data = df, Frule = -0.1),
    "'Frule' must be between 0 and 1"
  )
})

test_that("batchCorrectR rejects invalid imputeM parameter", {
  df <- make_bc_data()
  expect_error(
    batchCorrectR(data = df, imputeM = "rubbish"),
    "Invalid 'imputeM'"
  )
})

test_that("batchCorrectR rejects duplicate sample names", {
  df <- make_bc_data()
  df$sample_name[2] <- df$sample_name[1]
  expect_error(
    batchCorrectR(data = df),
    "sample_name.*must be unique"
  )
})

test_that("batchCorrectR rejects non-numeric run_order", {
  df <- make_bc_data()
  df$run_order <- as.character(df$run_order)
  expect_error(
    batchCorrectR(data = df),
    "run_order.*must be numeric"
  )
})

test_that("batchCorrectR rejects data with no QC samples", {
  df <- make_bc_data()
  df$sample_type <- "sample"
  expect_error(
    batchCorrectR(data = df),
    "No QC samples found"
  )
})

test_that("batchCorrectR rejects batch with fewer than 2 QC samples", {
  df <- make_bc_data(n_samples = 20, n_batches = 2, n_qc_per_batch = 4)
  # Remove all but 1 QC in plate1

  plate1_qc_idx <- which(df$batch == "plate1" & df$sample_type == "qc")
  df$sample_type[plate1_qc_idx[-1]] <- "sample"
  expect_error(
    batchCorrectR(data = df),
    "has < 2 QC samples"
  )
})

test_that("batchCorrectR rejects data with no numeric metabolite columns", {
  df <- data.frame(
    sample_name = paste0("S", 1:10),
    batch       = rep("plate1", 10),
    sample_type = rep(c("qc", "sample"), 5),
    run_order   = 1:10,
    text_col    = letters[1:10],
    stringsAsFactors = FALSE
  )
  expect_error(
    batchCorrectR(data = df),
    "No numeric metabolite columns"
  )
})

test_that("batchCorrectR accepts valid methods without error at validation", {

  # We only test that validation passes -- stub out actual correction
  df <- make_bc_data()
  for (m in c("QCRFSC", "ComBat")) {
    # bc_validate_input should pass for each valid method
    expect_invisible(
      bc_validate_input(df, "qc", m, 500, 10000, 0, "minHalf")
    )
  }
})

test_that("batchCorrectR accepts all valid imputeM values", {
  df <- make_bc_data()
  for (imp in c("minHalf", "median", "mean", "knn")) {
    expect_invisible(
      bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, imp)
    )
  }
})

# --- Edge-case: case-insensitive QC label ---

test_that("batchCorrectR validation is case-insensitive on qc_label", {
  df <- make_bc_data()
  df$sample_type <- toupper(df$sample_type)  # "QC" and "SAMPLE"
  # Should not error with qc_label = "qc" because tolower is applied
  expect_invisible(
    bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, "minHalf")
  )
})

# ============================================================================
# Workflow orchestration tests (mocked pipeline steps)
# ============================================================================

test_that("batchCorrectR calls pipeline steps in correct order", {
  df <- make_bc_data()
  call_log <- character(0)

  stub(batchCorrectR, "bc_validate_input", function(...) {
    call_log <<- c(call_log, "validate")
  })
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) {
    call_log <<- c(call_log, "detect_cols")
    c("metab_A", "metab_B")
  })
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) {
    call_log <<- c(call_log, "flag_qc")
    list(data = df, failed_samples = character(0))
  })
  stub(batchCorrectR, "bc_calculate_rsd", function(...) {
    call_log <<- c(call_log, "calc_rsd")
    c(metab_A = 10, metab_B = 15)
  })
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) {
    call_log <<- c(call_log, "pheno_file")
    data.frame(sample = "S1")
  })
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) {
    call_log <<- c(call_log, "profile_file")
    list(metabolite_map = c(X1 = "metab_A", X2 = "metab_B"))
  })
  stub(batchCorrectR, "bc_run_batch_correction", function(...) {
    call_log <<- c(call_log, "run_correction")
    data.frame(X1 = rnorm(20), X2 = rnorm(20))
  })
  stub(batchCorrectR, "bc_clean_correction_output", function(...) {
    call_log <<- c(call_log, "clean_output")
    df
  })
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) {
    call_log <<- c(call_log, "adjust_means")
    df
  })
  stub(batchCorrectR, "bc_reconstruct_output", function(...) {
    call_log <<- c(call_log, "reconstruct")
    df
  })
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    call_log <<- c(call_log, "build_summary")
    data.frame(metabolite = c("metab_A", "metab_B"), rsd_before = c(10, 15), rsd_after = c(5, 8))
  })
  stub(batchCorrectR, "bc_plot_correction_results", function(...) {
    call_log <<- c(call_log, "plot")
    list()
  })
  stub(batchCorrectR, "bc_generate_correction_report", function(...) {
    call_log <<- c(call_log, "report")
    "report"
  })

  suppressMessages(suppressWarnings(
    batchCorrectR(data = df, plot = TRUE, report = TRUE)
  ))

  expected <- c("validate", "detect_cols", "flag_qc", "calc_rsd",
                "pheno_file", "profile_file", "run_correction",
                "clean_output", "adjust_means", "reconstruct",
                "reconstruct", "calc_rsd", "build_summary", "plot", "report")
  expect_equal(call_log, expected)
})

test_that("batchCorrectR skips plot generation when plot = FALSE", {
  df <- make_bc_data()
  plot_called <- FALSE

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A", "metab_B"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) list(data = df, failed_samples = character(0)))
  stub(batchCorrectR, "bc_calculate_rsd", function(...) c(metab_A = 10, metab_B = 15))
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) data.frame(sample = "S1"))
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(...) data.frame(X1 = rnorm(20)))
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = "metab_A", rsd_before = 10, rsd_after = 5)
  })
  stub(batchCorrectR, "bc_plot_correction_results", function(...) {
    plot_called <<- TRUE
    list()
  })
  stub(batchCorrectR, "bc_generate_correction_report", function(...) "report")

  suppressMessages(suppressWarnings(
    batchCorrectR(data = df, plot = FALSE, report = TRUE)
  ))

  expect_false(plot_called)
})

test_that("batchCorrectR skips report generation when report = FALSE", {
  df <- make_bc_data()
  report_called <- FALSE

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A", "metab_B"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) list(data = df, failed_samples = character(0)))
  stub(batchCorrectR, "bc_calculate_rsd", function(...) c(metab_A = 10, metab_B = 15))
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) data.frame(sample = "S1"))
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(...) data.frame(X1 = rnorm(20)))
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = "metab_A", rsd_before = 10, rsd_after = 5)
  })
  stub(batchCorrectR, "bc_plot_correction_results", function(...) list())
  stub(batchCorrectR, "bc_generate_correction_report", function(...) {
    report_called <<- TRUE
    "report"
  })

  suppressMessages(suppressWarnings(
    batchCorrectR(data = df, plot = TRUE, report = FALSE)
  ))

  expect_false(report_called)
})

test_that("batchCorrectR returns correct structure with all expected elements", {
  df <- make_bc_data()

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A", "metab_B"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) list(data = df, failed_samples = c("S3")))
  stub(batchCorrectR, "bc_calculate_rsd", function(...) c(metab_A = 10, metab_B = 15))
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) data.frame(sample = "S1"))
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(...) data.frame(X1 = rnorm(20)))
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = c("metab_A", "metab_B"), rsd_before = c(10, 15), rsd_after = c(5, 8))
  })
  stub(batchCorrectR, "bc_plot_correction_results", function(...) list(p1 = "plot"))
  stub(batchCorrectR, "bc_generate_correction_report", function(...) "html_report")

  result <- suppressMessages(suppressWarnings(
    batchCorrectR(data = df, plot = TRUE, report = TRUE)
  ))

  expect_true(is.list(result))
  expect_true("corrected_data" %in% names(result))
  expect_true("correction_summary" %in% names(result))
  expect_true("qc_rsd_before" %in% names(result))
  expect_true("qc_rsd_after" %in% names(result))
  expect_true("failed_qc" %in% names(result))
  expect_true("plots" %in% names(result))
  expect_true("report" %in% names(result))
  expect_equal(result$failed_qc, c("S3"))
})

test_that("batchCorrectR result omits plots when plot = FALSE", {
  df <- make_bc_data()

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) list(data = df, failed_samples = character(0)))
  stub(batchCorrectR, "bc_calculate_rsd", function(...) c(metab_A = 10))
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) data.frame(sample = "S1"))
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(...) data.frame(X1 = rnorm(20)))
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = "metab_A", rsd_before = 10, rsd_after = 5)
  })
  stub(batchCorrectR, "bc_generate_correction_report", function(...) "report")

  result <- suppressMessages(suppressWarnings(
    batchCorrectR(data = df, plot = FALSE, report = TRUE)
  ))

  expect_false("plots" %in% names(result))
  expect_true("report" %in% names(result))
})

test_that("batchCorrectR result omits report when report = FALSE", {
  df <- make_bc_data()

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) list(data = df, failed_samples = character(0)))
  stub(batchCorrectR, "bc_calculate_rsd", function(...) c(metab_A = 10))
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) data.frame(sample = "S1"))
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(...) data.frame(X1 = rnorm(20)))
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = "metab_A", rsd_before = 10, rsd_after = 5)
  })
  stub(batchCorrectR, "bc_plot_correction_results", function(...) list(p1 = "plot"))

  result <- suppressMessages(suppressWarnings(
    batchCorrectR(data = df, plot = TRUE, report = FALSE)
  ))

  expect_false("report" %in% names(result))
  expect_true("plots" %in% names(result))
})

test_that("batchCorrectR warns when failed QC injections are found", {
  df <- make_bc_data()

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) {
    list(data = df, failed_samples = c("S1", "S5"))
  })
  stub(batchCorrectR, "bc_calculate_rsd", function(...) c(metab_A = 10))
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) data.frame(sample = "S1"))
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(...) data.frame(X1 = rnorm(20)))
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = "metab_A", rsd_before = 10, rsd_after = 5)
  })

  expect_warning(
    suppressMessages(
      batchCorrectR(data = df, plot = FALSE, report = FALSE)
    ),
    "Flagged 2 failed QC injection"
  )
})

test_that("batchCorrectR emits completion message with RSD improvement count", {
  df <- make_bc_data()

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A", "metab_B"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) list(data = df, failed_samples = character(0)))

  rsd_call <- 0
  stub(batchCorrectR, "bc_calculate_rsd", function(...) {
    rsd_call <<- rsd_call + 1
    if (rsd_call == 1) c(metab_A = 20, metab_B = 30)  # before
    else c(metab_A = 10, metab_B = 25)  # after (both improved)
  })
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) data.frame(sample = "S1"))
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(...) data.frame(X1 = rnorm(20)))
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = c("metab_A", "metab_B"), rsd_before = c(20, 30), rsd_after = c(10, 25))
  })

  msgs <- capture.output(type = "message",
    batchCorrectR(data = df, plot = FALSE, report = FALSE)
  )
  expect_true(any(grepl("2/2 metabolites showed RSD improvement", msgs)))
})

test_that("batchCorrectR passes method parameter to bc_run_batch_correction", {
  df <- make_bc_data()
  captured_method <- NULL

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) list(data = df, failed_samples = character(0)))
  stub(batchCorrectR, "bc_calculate_rsd", function(...) c(metab_A = 10))
  stub(batchCorrectR, "bc_prepare_pheno_file", function(...) data.frame(sample = "S1"))
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(st_dir, method, ...) {
    captured_method <<- method
    data.frame(X1 = rnorm(20))
  })
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = "metab_A", rsd_before = 10, rsd_after = 5)
  })

  suppressMessages(suppressWarnings(
    batchCorrectR(data = df, method = "QCRFSC", plot = FALSE, report = FALSE)
  ))

  expect_equal(captured_method, "QCRFSC")
})

test_that("batchCorrectR uses tempdir-based output directory by default", {
  df <- make_bc_data()
  captured_st_dir <- NULL

  stub(batchCorrectR, "bc_validate_input", function(...) NULL)
  stub(batchCorrectR, "bc_detect_metabolite_columns", function(...) c("metab_A"))
  stub(batchCorrectR, "bc_flag_failed_qc", function(...) list(data = df, failed_samples = character(0)))
  stub(batchCorrectR, "bc_calculate_rsd", function(...) c(metab_A = 10))
  stub(batchCorrectR, "bc_prepare_pheno_file", function(data, qc_label, st_dir) {
    captured_st_dir <<- st_dir
    data.frame(sample = "S1")
  })
  stub(batchCorrectR, "bc_prepare_profile_file", function(...) list(metabolite_map = c(X1 = "metab_A")))
  stub(batchCorrectR, "bc_run_batch_correction", function(...) data.frame(X1 = rnorm(20)))
  stub(batchCorrectR, "bc_clean_correction_output", function(...) df)
  stub(batchCorrectR, "bc_adjust_corrected_means", function(...) df)
  stub(batchCorrectR, "bc_reconstruct_output", function(...) df)
  stub(batchCorrectR, "bc_build_correction_summary", function(...) {
    data.frame(metabolite = "metab_A", rsd_before = 10, rsd_after = 5)
  })

  suppressMessages(suppressWarnings(
    batchCorrectR(data = df, plot = FALSE, report = FALSE)
  ))

  expect_true(grepl("batchCorrectR_", captured_st_dir))
  expect_true(grepl(normalizePath(tempdir(), winslash = "/"),
                    normalizePath(captured_st_dir, winslash = "/"), fixed = TRUE))
})

# ============================================================================
# ComBat method tests
# ============================================================================

test_that("batchCorrectR accepts method = 'ComBat'", {
  # ComBat should be accepted as a valid method string
  # We can't easily test full execution without sva installed,
  # but we can test that validation passes
  df <- data.frame(
    sample_name = paste0("S", 1:20),
    batch = rep(c("B1", "B2"), each = 10),
    sample_type = rep(c("qc", "sample", "sample", "sample", "qc"), 4),
    run_order = 1:20,
    met_A = rnorm(20, 100, 10),
    met_B = rnorm(20, 500, 50)
  )
  result <- tryCatch(
    batchCorrectR(data = df, method = "ComBat", plot = FALSE, report = FALSE),
    error = function(e) e
  )
  if (requireNamespace("sva", quietly = TRUE)) {
    # sva installed: should succeed and return corrected data
    expect_true(is.list(result))
    expect_true("corrected_data" %in% names(result))
  } else {
    # sva not installed: should fail with sva install message, not invalid method
    expect_true(grepl("sva", result$message, ignore.case = TRUE))
    expect_false(grepl("Invalid 'method'", result$message))
  }
})

test_that("batchCorrectR ComBat works without QC samples", {
  # ComBat should not require QC samples
  df <- data.frame(
    sample_name = paste0("S", 1:20),
    batch = rep(c("B1", "B2"), each = 10),
    sample_type = rep("sample", 20),
    run_order = 1:20,
    met_A = rnorm(20, 100, 10),
    met_B = rnorm(20, 500, 50)
  )
  result <- tryCatch(
    batchCorrectR(data = df, qc_label = "qc", method = "ComBat",
                  plot = FALSE, report = FALSE),
    error = function(e) e
  )
  if (requireNamespace("sva", quietly = TRUE)) {
    # sva installed: should succeed without QC samples
    expect_true(is.list(result))
    expect_true("corrected_data" %in% names(result))
  } else {
    # sva not installed: should fail asking for sva, not "No QC samples found"
    expect_true(grepl("sva", result$message, ignore.case = TRUE))
    expect_false(grepl("No QC samples found", result$message))
  }
})
