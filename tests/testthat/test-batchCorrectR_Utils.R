# Tests for batchCorrectR_Utils.R helper functions ----

# ============================================================================
# Helper: build a minimal valid data.frame for utility function tests
# ============================================================================
make_bc_test_data <- function(n_per_batch = 10, n_batches = 2,
                               n_qc_per_batch = 3) {
  rows <- list()
  counter <- 1
  for (b in seq_len(n_batches)) {
    batch_name <- paste0("plate", b)
    qc_pos <- round(seq(1, n_per_batch, length.out = n_qc_per_batch))
    for (i in seq_len(n_per_batch)) {
      stype <- if (i %in% qc_pos) "qc" else "sample"
      rows[[counter]] <- data.frame(
        sample_name = paste0("S", counter),
        batch       = batch_name,
        sample_type = stype,
        run_order   = counter,
        metab_A     = rnorm(1, 100, 10),
        metab_B     = rnorm(1, 500, 50),
        metab_C     = rnorm(1, 1000, 100),
        stringsAsFactors = FALSE
      )
      counter <- counter + 1
    }
  }
  do.call(rbind, rows)
}

# ============================================================================
# bc_validate_input ----
# ============================================================================

test_that("bc_validate_input returns invisible TRUE for valid data", {
  df <- make_bc_test_data()
  expect_invisible(
    bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, "minHalf")
  )
})

test_that("bc_validate_input errors on non-data.frame", {
  expect_error(bc_validate_input(list(), "qc", "QCRFSC", 500, 10000, 0, "minHalf"),
               "must be a data.frame")
  expect_error(bc_validate_input(matrix(1:4, 2), "qc", "QCRFSC", 500, 10000, 0, "minHalf"),
               "must be a data.frame")
})

test_that("bc_validate_input errors on missing columns", {
  df <- data.frame(sample_name = "x", batch = "a", run_order = 1, val = 1)
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, "minHalf"),
               "Missing required column.*sample_type")
})

test_that("bc_validate_input errors on duplicate sample names", {
  df <- make_bc_test_data()
  df$sample_name[2] <- df$sample_name[1]
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, "minHalf"),
               "must be unique")
})

test_that("bc_validate_input errors on non-numeric run_order", {
  df <- make_bc_test_data()
  df$run_order <- as.character(df$run_order)
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, "minHalf"),
               "run_order.*must be numeric")
})

test_that("bc_validate_input errors when no QC samples present", {
  df <- make_bc_test_data()
  df$sample_type <- "sample"
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, "minHalf"),
               "No QC samples found")
})

test_that("bc_validate_input errors when batch has fewer than 2 QCs", {
  df <- make_bc_test_data(n_per_batch = 5, n_batches = 1, n_qc_per_batch = 3)
  # Keep only 1 QC
  qc_idx <- which(df$sample_type == "qc")
  df$sample_type[qc_idx[-1]] <- "sample"
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, "minHalf"),
               "has < 2 QC samples")
})

test_that("bc_validate_input errors on invalid method", {
  df <- make_bc_test_data()
  expect_error(bc_validate_input(df, "qc", "WRONG", 500, 10000, 0, "minHalf"),
               "Invalid 'method'")
})

test_that("bc_validate_input errors on bad ntree values", {
  df <- make_bc_test_data()
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 0, 10000, 0, "minHalf"),
               "'ntree' must be a positive integer")
  expect_error(bc_validate_input(df, "qc", "QCRFSC", c(1, 2), 10000, 0, "minHalf"),
               "'ntree' must be a positive integer")
})

test_that("bc_validate_input errors on bad coCV values", {
  df <- make_bc_test_data()
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, 0, 0, "minHalf"),
               "'coCV' must be a positive number")
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, -1, 0, "minHalf"),
               "'coCV' must be a positive number")
})

test_that("bc_validate_input errors on Frule outside 0-1", {
  df <- make_bc_test_data()
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, 10000, -0.01, "minHalf"),
               "'Frule' must be between 0 and 1")
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 1.1, "minHalf"),
               "'Frule' must be between 0 and 1")
})

test_that("bc_validate_input errors on invalid imputeM", {
  df <- make_bc_test_data()
  expect_error(bc_validate_input(df, "qc", "QCRFSC", 500, 10000, 0, "badval"),
               "Invalid 'imputeM'")
})

# ============================================================================
# bc_detect_metabolite_columns ----
# ============================================================================

test_that("bc_detect_metabolite_columns finds correct columns", {
  df <- make_bc_test_data()
  result <- bc_detect_metabolite_columns(df)
  expect_equal(sort(result), sort(c("metab_A", "metab_B", "metab_C")))
})

test_that("bc_detect_metabolite_columns excludes run_order", {
  df <- make_bc_test_data()
  result <- bc_detect_metabolite_columns(df)
  expect_false("run_order" %in% result)
})

test_that("bc_detect_metabolite_columns errors when no metabolite columns", {
  df <- data.frame(
    sample_name = "S1", batch = "b1", sample_type = "qc",
    run_order = 1, text_col = "a", stringsAsFactors = FALSE
  )
  expect_error(bc_detect_metabolite_columns(df),
               "No numeric metabolite columns")
})

test_that("bc_detect_metabolite_columns works with extra character columns", {
  df <- make_bc_test_data()
  df$notes <- "some text"
  df$category <- "A"
  result <- bc_detect_metabolite_columns(df)
  expect_equal(sort(result), sort(c("metab_A", "metab_B", "metab_C")))
})

# ============================================================================
# bc_flag_failed_qc ----
# ============================================================================

test_that("bc_flag_failed_qc returns no failures for healthy QC data", {
  df <- make_bc_test_data()
  metabolite_cols <- c("metab_A", "metab_B", "metab_C")
  result <- bc_flag_failed_qc(df, "qc", metabolite_cols)
  expect_equal(length(result$failed_samples), 0)
  expect_equal(nrow(result$data), nrow(df))
})

test_that("bc_flag_failed_qc flags QC with near-zero signal", {
  df <- make_bc_test_data(n_per_batch = 10, n_batches = 1, n_qc_per_batch = 4)
  metabolite_cols <- c("metab_A", "metab_B", "metab_C")
  # Set first QC to near-zero signal
  qc_idx <- which(df$sample_type == "qc")
  df[qc_idx[1], metabolite_cols] <- 0.001
  result <- bc_flag_failed_qc(df, "qc", metabolite_cols)
  expect_true(df$sample_name[qc_idx[1]] %in% result$failed_samples)
  # Failed QC should be reclassified as "sample"
  expect_equal(result$data$sample_type[qc_idx[1]], "sample")
})

test_that("bc_flag_failed_qc preserves non-QC sample types", {
  df <- make_bc_test_data()
  metabolite_cols <- c("metab_A", "metab_B", "metab_C")
  result <- bc_flag_failed_qc(df, "qc", metabolite_cols)
  sample_idx <- which(df$sample_type == "sample")
  expect_equal(result$data$sample_type[sample_idx], df$sample_type[sample_idx])
})

test_that("bc_flag_failed_qc handles all-zero QC batch gracefully", {
  df <- make_bc_test_data(n_per_batch = 6, n_batches = 1, n_qc_per_batch = 3)
  metabolite_cols <- c("metab_A", "metab_B", "metab_C")
  # Set all QCs to zero -- median is 0, so 0 < 0*0.1 = 0 is FALSE
  qc_idx <- which(df$sample_type == "qc")
  df[qc_idx, metabolite_cols] <- 0
  result <- bc_flag_failed_qc(df, "qc", metabolite_cols)
  # With all zeros, median is 0 and low_signal check is < 0 which is FALSE for 0

  expect_equal(length(result$failed_samples), 0)
})

# ============================================================================
# bc_calculate_rsd ----
# ============================================================================

test_that("bc_calculate_rsd returns named numeric vector", {
  df <- make_bc_test_data()
  metabolite_cols <- c("metab_A", "metab_B", "metab_C")
  result <- bc_calculate_rsd(df, "qc", metabolite_cols)
  expect_type(result, "double")
  expect_named(result, metabolite_cols)
})

test_that("bc_calculate_rsd returns correct RSD for known data", {
  # Known data: SD=10, mean=100 => RSD = 10%
  df <- data.frame(
    sample_name = paste0("S", 1:4),
    batch = "b1", sample_type = "qc", run_order = 1:4,
    fixed_met = c(90, 100, 110, 100),
    stringsAsFactors = FALSE
  )
  result <- bc_calculate_rsd(df, "qc", "fixed_met")
  expected_rsd <- (sd(c(90, 100, 110, 100)) / mean(c(90, 100, 110, 100))) * 100
  expect_equal(result[["fixed_met"]], expected_rsd, tolerance = 0.001)
})

test_that("bc_calculate_rsd returns NA when no QC samples", {
  df <- make_bc_test_data()
  df$sample_type <- "sample"
  metabolite_cols <- c("metab_A", "metab_B", "metab_C")
  expect_warning(
    result <- bc_calculate_rsd(df, "qc", metabolite_cols),
    "No QC samples"
  )
  expect_true(all(is.na(result)))
})

test_that("bc_calculate_rsd returns NA for column with single QC value", {
  df <- data.frame(
    sample_name = c("S1", "S2"),
    batch = "b1", sample_type = c("qc", "sample"), run_order = 1:2,
    met = c(100, 200),
    stringsAsFactors = FALSE
  )
  result <- bc_calculate_rsd(df, "qc", "met")
  expect_true(is.na(result[["met"]]))
})

test_that("bc_calculate_rsd returns NA when mean is zero", {
  df <- data.frame(
    sample_name = paste0("S", 1:4),
    batch = "b1", sample_type = "qc", run_order = 1:4,
    met = c(-50, 50, -50, 50),
    stringsAsFactors = FALSE
  )
  result <- bc_calculate_rsd(df, "qc", "met")
  expect_true(is.na(result[["met"]]))
})

test_that("bc_calculate_rsd handles NA values in metabolite columns", {
  df <- data.frame(
    sample_name = paste0("S", 1:5),
    batch = "b1", sample_type = "qc", run_order = 1:5,
    met = c(100, NA, 100, 100, NA),
    stringsAsFactors = FALSE
  )
  result <- bc_calculate_rsd(df, "qc", "met")
  # Only 3 non-NA values: c(100, 100, 100) => SD=0, mean=100, RSD=0
  expect_equal(result[["met"]], 0)
})

# ============================================================================
# bc_build_correction_summary ----
# ============================================================================

test_that("bc_build_correction_summary returns correct structure", {
  mets <- c("metab_A", "metab_B")
  rsd_before <- c(metab_A = 20, metab_B = 30)
  rsd_after  <- c(metab_A = 10, metab_B = 35)
  result <- bc_build_correction_summary(mets, rsd_before, rsd_after)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(c("metabolite", "rsd_before", "rsd_after",
                     "rsd_change", "improved") %in% names(result)))
})

test_that("bc_build_correction_summary correctly identifies improved metabolites", {
  mets <- c("metab_A", "metab_B", "metab_C")
  rsd_before <- c(metab_A = 20, metab_B = 30, metab_C = 10)
  rsd_after  <- c(metab_A = 10, metab_B = 35, metab_C = 5)
  result <- bc_build_correction_summary(mets, rsd_before, rsd_after)
  expect_equal(unname(result$improved), c(TRUE, FALSE, TRUE))
})

test_that("bc_build_correction_summary computes rsd_change correctly", {
  mets <- c("m1")
  rsd_before <- c(m1 = 25)
  rsd_after  <- c(m1 = 15)
  result <- bc_build_correction_summary(mets, rsd_before, rsd_after)
  expect_equal(unname(result$rsd_change), -10)
})

test_that("bc_build_correction_summary handles NA RSDs", {
  mets <- c("m1", "m2")
  rsd_before <- c(m1 = NA_real_, m2 = 20)
  rsd_after  <- c(m1 = 10, m2 = NA_real_)
  result <- bc_build_correction_summary(mets, rsd_before, rsd_after)
  expect_true(is.na(result$rsd_change[1]))
  expect_true(is.na(result$rsd_change[2]))
  expect_true(is.na(result$improved[1]))
  expect_true(is.na(result$improved[2]))
})

# ============================================================================
# bc_reconstruct_output ----
# ============================================================================

test_that("bc_reconstruct_output preserves original column order", {
  original <- data.frame(
    sample_name = paste0("S", 1:3),
    batch = "b1", sample_type = "qc", run_order = 1:3,
    metab_A = c(100, 200, 300), metab_B = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  corrected_adj <- data.frame(
    sample_name = paste0("S", 1:3),
    metab_A = c(110, 210, 310), metab_B = c(11, 21, 31),
    stringsAsFactors = FALSE
  )
  result <- bc_reconstruct_output(corrected_adj, original, c("metab_A", "metab_B"))
  expect_equal(names(result), names(original))
})

test_that("bc_reconstruct_output uses corrected values for available metabolites", {
  original <- data.frame(
    sample_name = paste0("S", 1:2),
    batch = "b1", sample_type = "qc", run_order = 1:2,
    metab_A = c(100, 200),
    stringsAsFactors = FALSE
  )
  corrected_adj <- data.frame(
    sample_name = paste0("S", 1:2),
    metab_A = c(150, 250),
    stringsAsFactors = FALSE
  )
  result <- bc_reconstruct_output(corrected_adj, original, "metab_A")
  expect_equal(result$metab_A, c(150, 250))
})

test_that("bc_reconstruct_output falls back to original for missing metabolites", {
  original <- data.frame(
    sample_name = paste0("S", 1:2),
    batch = "b1", sample_type = "qc", run_order = 1:2,
    metab_A = c(100, 200), metab_B = c(10, 20),
    stringsAsFactors = FALSE
  )
  corrected_adj <- data.frame(
    sample_name = paste0("S", 1:2),
    metab_A = c(150, 250),
    stringsAsFactors = FALSE
  )
  expect_warning(
    result <- bc_reconstruct_output(corrected_adj, original,
                                     c("metab_A", "metab_B")),
    "UNCORRECTED"
  )
  expect_equal(result$metab_B, c(10, 20))
})

# ============================================================================
# bc_adjust_corrected_means ----
# ============================================================================

test_that("bc_adjust_corrected_means adjusts values proportionally", {
  data_flagged <- data.frame(
    sample_name = paste0("S", 1:4),
    batch = "b1",
    sample_type = c("qc", "qc", "sample", "sample"),
    run_order = 1:4,
    met = c(100, 100, 50, 50),
    stringsAsFactors = FALSE
  )
  # Corrected QC mean = 200, original QC mean = 100
  # So values should be divided by (200/100) = 2
  corrected_clean <- data.frame(
    sample_name = paste0("S", 1:4),
    met = c(200, 200, 100, 100),
    stringsAsFactors = FALSE
  )
  result <- bc_adjust_corrected_means(
    corrected_clean, data_flagged, "qc", "met"
  )
  expect_equal(result$met, c(100, 100, 50, 50))
})

test_that("bc_adjust_corrected_means warns on no matching metabolite columns", {
  data_flagged <- data.frame(
    sample_name = "S1", batch = "b1", sample_type = "qc",
    run_order = 1, met = 100, stringsAsFactors = FALSE
  )
  corrected_clean <- data.frame(
    sample_name = "S1", other_met = 200, stringsAsFactors = FALSE
  )
  expect_warning(
    result <- bc_adjust_corrected_means(corrected_clean, data_flagged, "qc", "met"),
    "No matching metabolite columns"
  )
  # Returns input unchanged
  expect_equal(result, corrected_clean)
})

test_that("bc_adjust_corrected_means skips metabolites with zero mean", {
  data_flagged <- data.frame(
    sample_name = paste0("S", 1:4),
    batch = "b1",
    sample_type = c("qc", "qc", "sample", "sample"),
    run_order = 1:4,
    met = c(0, 0, 50, 50),
    stringsAsFactors = FALSE
  )
  corrected_clean <- data.frame(
    sample_name = paste0("S", 1:4),
    met = c(200, 200, 100, 100),
    stringsAsFactors = FALSE
  )
  expect_warning(
    result <- bc_adjust_corrected_means(
      corrected_clean, data_flagged, "qc", "met"
    ),
    "non-positive corrected/original QC-mean ratio"
  )
  # Original QC mean is 0, so adjustment should be skipped => values unchanged
  expect_equal(result$met, c(200, 200, 100, 100))
})

# ============================================================================
# bc_generate_correction_report ----
# ============================================================================

test_that("bc_generate_correction_report returns correct structure", {
  mets <- c("m1", "m2", "m3")
  rsd_before <- c(m1 = 20, m2 = 30, m3 = 10)
  rsd_after  <- c(m1 = 10, m2 = 35, m3 = 5)
  summary_tbl <- bc_build_correction_summary(mets, rsd_before, rsd_after)

  result <- bc_generate_correction_report(
    correction_summary = summary_tbl,
    failed_qc = c("S5"),
    method = "QCRFSC",
    n_samples = 30,
    n_batches = 2,
    n_metabolites = 3
  )
  expect_type(result, "list")
  expect_equal(result$title, "batchCorrectR Interbatch Correction Report")
  expect_equal(result$parameters$method, "QCRFSC")
  expect_equal(result$parameters$n_failed_qc, 1)
  expect_equal(result$results$n_improved, 2)
  expect_equal(result$results$n_worsened, 1)
  expect_equal(result$failed_qc_samples, "S5")
})

test_that("bc_generate_correction_report handles no failures", {
  mets <- c("m1")
  summary_tbl <- bc_build_correction_summary(
    mets, c(m1 = 20), c(m1 = 10)
  )
  result <- bc_generate_correction_report(
    summary_tbl, character(0), "QCRFSC", 10, 1, 1
  )
  expect_equal(result$parameters$n_failed_qc, 0)
  expect_equal(length(result$failed_qc_samples), 0)
})

# ============================================================================
# bc_prepare_pheno_file ----
# ============================================================================

test_that("bc_prepare_pheno_file writes PhenoFile.csv and returns correct columns", {
  df <- make_bc_test_data(n_per_batch = 6, n_batches = 1, n_qc_per_batch = 3)
  st_dir <- file.path(tempdir(), paste0("test_pheno_", Sys.getpid()))
  dir.create(st_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(st_dir, recursive = TRUE), add = TRUE)

  result <- bc_prepare_pheno_file(df, "qc", st_dir)

  # File should be written

  expect_true(file.exists(file.path(st_dir, "PhenoFile.csv")))

  # Result columns
  expect_true(all(c("sample", "sample_name", "batch", "class", "order") %in%
                     names(result)))

  # QC samples should be labelled
  expect_true("qc" %in% result$class)
})

# ============================================================================
# bc_prepare_profile_file ----
# ============================================================================

test_that("bc_prepare_profile_file writes ProfileFile.csv and returns metabolite map", {
  df <- make_bc_test_data(n_per_batch = 6, n_batches = 1, n_qc_per_batch = 3)
  metabolite_cols <- c("metab_A", "metab_B", "metab_C")
  st_dir <- file.path(tempdir(), paste0("test_profile_", Sys.getpid()))
  dir.create(st_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(st_dir, recursive = TRUE), add = TRUE)

  pheno <- bc_prepare_pheno_file(df, "qc", st_dir)
  result <- bc_prepare_profile_file(df, metabolite_cols, pheno, st_dir)

  expect_true(file.exists(file.path(st_dir, "ProfileFile.csv")))
  expect_true("metabolite_map" %in% names(result))
  expect_equal(nrow(result$metabolite_map), length(metabolite_cols))
  expect_true(all(c("name", "metabolite_code") %in% names(result$metabolite_map)))
})

# ============================================================================
# bc_run_batch_correction (integration-safe with mockery) ----
# ============================================================================

test_that("bc_run_batch_correction constructs correct file paths and calls shiftCor", {
  skip_if_not_installed("mockery")
  st_dir <- file.path(tempdir(), paste0("test_bc_run_", Sys.getpid()))
  dir.create(st_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(st_dir, recursive = TRUE), add = TRUE)

  # Create dummy input files so the function can reference them
  writeLines("dummy", file.path(st_dir, "PhenoFile.csv"))
  writeLines("dummy", file.path(st_dir, "ProfileFile.csv"))

  # Create the expected output directory and file
  out_dir <- file.path(st_dir, "statTarget", "shiftCor", "After_shiftCor")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(
    data.frame(name = c("M1", "M2"), sample1 = c(100, 200), sample2 = c(300, 400)),
    file.path(out_dir, "shift_all_cor.csv")
  )

  # Track what arguments shiftCor was called with
  captured_args <- NULL
  mock_shiftCor <- function(...) {
    captured_args <<- list(...)
    invisible(NULL)
  }

  mockery::stub(bc_run_batch_correction, "statTarget::shiftCor", mock_shiftCor)
  result <- bc_run_batch_correction(st_dir, "QCRFSC", 500, 100, 0.8, "minHalf")

  # Verify shiftCor was called with correct arguments
  expect_equal(captured_args$samPeno, file.path(st_dir, "PhenoFile.csv"))
  expect_equal(captured_args$samFile, file.path(st_dir, "ProfileFile.csv"))
  expect_equal(captured_args$MLmethod, "QCRFSC")
  expect_equal(captured_args$ntree, 500)
  expect_equal(captured_args$coCV, 100)
  expect_equal(captured_args$Frule, 0.8)
  expect_equal(captured_args$imputeM, "minHalf")
  expect_false(captured_args$plot)

  # Verify return value is a tibble

  expect_s3_class(result, "tbl_df")
  expect_true("name" %in% names(result) || "M1" %in% names(result) || "sample1" %in% names(result))
})

test_that("bc_run_batch_correction errors when shiftCor fails", {
  skip_if_not_installed("mockery")
  st_dir <- file.path(tempdir(), paste0("test_bc_run_err_", Sys.getpid()))
  dir.create(st_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(st_dir, recursive = TRUE), add = TRUE)
  writeLines("dummy", file.path(st_dir, "PhenoFile.csv"))
  writeLines("dummy", file.path(st_dir, "ProfileFile.csv"))

  mock_shiftCor_fail <- function(...) {
    stop("shiftCor internal error")
  }
  mockery::stub(bc_run_batch_correction, "statTarget::shiftCor", mock_shiftCor_fail)
  expect_error(bc_run_batch_correction(st_dir, "QCRFSC", 500, 10000, 0.8, "minHalf"),
               "shiftCor internal error")
})

test_that("bc_run_batch_correction errors when output file not found", {
  skip_if_not_installed("mockery")
  st_dir <- file.path(tempdir(), paste0("test_bc_run_nofile_", Sys.getpid()))
  dir.create(st_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(st_dir, recursive = TRUE), add = TRUE)
  writeLines("dummy", file.path(st_dir, "PhenoFile.csv"))
  writeLines("dummy", file.path(st_dir, "ProfileFile.csv"))

  # shiftCor succeeds but does not create output
  mock_shiftCor_noop <- function(...) invisible(NULL)
  mockery::stub(bc_run_batch_correction, "statTarget::shiftCor", mock_shiftCor_noop)
  expect_error(bc_run_batch_correction(st_dir, "QCRFSC", 500, 10000, 0.8, "minHalf"),
               "statTarget output not found")
})

test_that("bc_run_batch_correction restores working directory on success", {
  skip_if_not_installed("mockery")
  st_dir <- file.path(tempdir(), paste0("test_bc_run_wd_", Sys.getpid()))
  dir.create(st_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(st_dir, recursive = TRUE), add = TRUE)
  writeLines("dummy", file.path(st_dir, "PhenoFile.csv"))
  writeLines("dummy", file.path(st_dir, "ProfileFile.csv"))
  out_dir <- file.path(st_dir, "statTarget", "shiftCor", "After_shiftCor")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data.frame(name = "M1", s1 = 1), file.path(out_dir, "shift_all_cor.csv"))

  mock_shiftCor <- function(...) invisible(NULL)
  mockery::stub(bc_run_batch_correction, "statTarget::shiftCor", mock_shiftCor)

  orig_wd <- getwd()
  bc_run_batch_correction(st_dir, "QCRFSC", 500, 10000, 0.8, "minHalf")
  expect_equal(getwd(), orig_wd)
})

test_that("bc_run_batch_correction restores working directory on error", {
  skip_if_not_installed("mockery")
  st_dir <- file.path(tempdir(), paste0("test_bc_run_wd_err_", Sys.getpid()))
  dir.create(st_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(st_dir, recursive = TRUE), add = TRUE)
  writeLines("dummy", file.path(st_dir, "PhenoFile.csv"))
  writeLines("dummy", file.path(st_dir, "ProfileFile.csv"))

  mock_shiftCor_fail <- function(...) stop("boom")
  mockery::stub(bc_run_batch_correction, "statTarget::shiftCor", mock_shiftCor_fail)

  orig_wd <- getwd()
  try(bc_run_batch_correction(st_dir, "QCRFSC", 500, 10000, 0.8, "minHalf"), silent = TRUE)
  expect_equal(getwd(), orig_wd)
})

# ============================================================================
# bc_plot_correction_results & bc_plot_pca (structural tests) ----
# ============================================================================

test_that("bc_plot_correction_results returns a named list of ggplot objects", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tidyr")

  set.seed(42)
  original <- make_bc_test_data(n_per_batch = 10, n_batches = 2, n_qc_per_batch = 3)
  corrected <- original
  metabolite_cols <- c("metab_A", "metab_B", "metab_C")
  # Slightly modify corrected data
  for (m in metabolite_cols) corrected[[m]] <- corrected[[m]] * runif(nrow(corrected), 0.9, 1.1)

  rsd_before <- bc_calculate_rsd(original, "qc", metabolite_cols)
  rsd_after  <- bc_calculate_rsd(corrected, "qc", metabolite_cols)

  result <- bc_plot_correction_results(original, corrected, "qc",
                                        metabolite_cols, rsd_before, rsd_after)

  expect_type(result, "list")
  expect_true("rsd_comparison" %in% names(result))
  expect_true("run_order" %in% names(result))
  expect_s3_class(result$rsd_comparison, "ggplot")
  expect_s3_class(result$run_order, "ggplot")
})

test_that("bc_plot_pca returns a ggplot for valid multi-column data", {
  skip_if_not_installed("ggplot2")

  set.seed(42)
  original <- make_bc_test_data(n_per_batch = 10, n_batches = 2, n_qc_per_batch = 3)
  corrected <- original
  metabolite_cols <- c("metab_A", "metab_B", "metab_C")
  for (m in metabolite_cols) corrected[[m]] <- corrected[[m]] * 1.05

  result <- bc_plot_pca(original, corrected, metabolite_cols)
  expect_s3_class(result, "ggplot")
})

test_that("bc_plot_pca returns NULL when fewer than 2 variable columns", {
  skip_if_not_installed("ggplot2")

  # Data with only 1 metabolite and zero variance
  df <- data.frame(
    sample_name = paste0("S", 1:5), batch = "b1",
    sample_type = c("qc", "qc", "sample", "sample", "sample"),
    run_order = 1:5, met_const = rep(100, 5),
    stringsAsFactors = FALSE
  )
  result <- bc_plot_pca(df, df, "met_const")
  expect_null(result)
})

test_that("bc_plot_correction_results handles single metabolite", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tidyr")

  set.seed(42)
  original <- make_bc_test_data(n_per_batch = 6, n_batches = 1, n_qc_per_batch = 3)
  corrected <- original
  metabolite_cols <- c("metab_A")
  corrected$metab_A <- corrected$metab_A * 1.1

  rsd_before <- bc_calculate_rsd(original, "qc", metabolite_cols)
  rsd_after  <- bc_calculate_rsd(corrected, "qc", metabolite_cols)

  # Should not error even with a single metabolite (PCA may return NULL)
  result <- bc_plot_correction_results(original, corrected, "qc",
                                        metabolite_cols, rsd_before, rsd_after)
  expect_type(result, "list")
  expect_s3_class(result$rsd_comparison, "ggplot")
})

# ============================================================================
# bc_clean_correction_output (3 code paths) ----
# ============================================================================

test_that("bc_clean_correction_output handles path 1: 'sample1' in colnames", {
  # Path 1: wide format with sample IDs as column names
  corrected_raw <- tibble::tibble(
    sample = c("class", "M1", "M2"),
    sample1 = c("qc", "100", "200"),
    sample2 = c("sample", "300", "400")
  )
  metabolite_map <- tibble::tibble(
    name = c("metab_A", "metab_B"),
    metabolite_code = c("M1", "M2")
  )
  pheno <- tibble::tibble(
    sample = c("sample1", "sample2"),
    sample_name = c("S1", "S2")
  )

  result <- bc_clean_correction_output(corrected_raw, metabolite_map, pheno)
  expect_s3_class(result, "tbl_df")
  expect_true("sample_name" %in% names(result))
  expect_true(all(c("metab_A", "metab_B") %in% names(result)))
  expect_equal(nrow(result), 2)
  # Check that metabolite codes were mapped back to names
  expect_equal(result$metab_A, c(100, 300))
  expect_equal(result$metab_B, c(200, 400))
})

test_that("bc_clean_correction_output handles path 2: 'M1' in colnames", {
  # Path 2: transposed format where metabolite codes are column names.
  # After t() + rownames_to_column + setNames(.[1,]), the first row becomes headers.
  # The "sample" column holds original row names: "sample", "M1", "M2".
  # Filter removes "class" and "sample" rows, leaving metabolite rows.
  corrected_raw <- tibble::tibble(
    sample = c("class", "sample1", "sample2"),
    M1 = c("NA", "100", "300"),
    M2 = c("NA", "200", "400")
  )
  metabolite_map <- tibble::tibble(
    name = c("metab_A", "metab_B"),
    metabolite_code = c("M1", "M2")
  )
  pheno <- tibble::tibble(
    sample = c("sample1", "sample2"),
    sample_name = c("S1", "S2")
  )

  # This path transposes the data, so exact output depends on internal reshape.
  # The key check is that it does not error and returns a tibble with sample_name.
  # Suppress expected "NAs introduced by coercion" from as.numeric on class row.
  result <- suppressWarnings(bc_clean_correction_output(corrected_raw, metabolite_map, pheno))
  expect_s3_class(result, "tbl_df")
  expect_true("sample_name" %in% names(result))
})

test_that("bc_clean_correction_output handles path 3: fallback/generic format", {
  # Path 3: generic format (neither 'sample1' nor 'M1' in colnames)
  corrected_raw <- tibble::tibble(
    name_col = c("class", "M1", "M2"),
    QC1 = c("qc", "150", "250"),
    sample1_x = c("sample", "350", "450")
  )
  metabolite_map <- tibble::tibble(
    name = c("metab_A", "metab_B"),
    metabolite_code = c("M1", "M2")
  )
  pheno <- tibble::tibble(
    sample = c("QC1", "sample1_x"),
    sample_name = c("S1", "S2")
  )

  result <- bc_clean_correction_output(corrected_raw, metabolite_map, pheno)
  expect_s3_class(result, "tbl_df")
  expect_true("sample_name" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_true(all(c("metab_A", "metab_B") %in% names(result)))
})

test_that("bc_clean_correction_output maps metabolite codes back to names", {
  corrected_raw <- tibble::tibble(
    sample = c("class", "M1", "M2", "M3"),
    sample1 = c("qc", "10", "20", "30"),
    sample2 = c("sample", "40", "50", "60")
  )
  metabolite_map <- tibble::tibble(
    name = c("lipid_A", "lipid_B", "lipid_C"),
    metabolite_code = c("M1", "M2", "M3")
  )
  pheno <- tibble::tibble(
    sample = c("sample1", "sample2"),
    sample_name = c("QC_1", "Samp_1")
  )

  result <- bc_clean_correction_output(corrected_raw, metabolite_map, pheno)
  expect_true(all(c("lipid_A", "lipid_B", "lipid_C") %in% names(result)))
  expect_equal(result$sample_name, c("QC_1", "Samp_1"))
})

# ============================================================================
# bc_preprocess_input tests
# ============================================================================

test_that("bc_preprocess_input combines a list of data.frames", {
  df1 <- data.frame(
    sample_name = c("S1", "S2"),
    sample_plate_id = "plate1",
    sample_type_factor = c("pqc", "sample"),
    sample_run_index = 1:2,
    metab_A = c(10, 20),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    sample_name = c("S3", "S4"),
    sample_plate_id = "plate2",
    sample_type_factor = c("pqc", "sample"),
    sample_run_index = 3:4,
    metab_A = c(30, 40),
    stringsAsFactors = FALSE
  )

  result <- bc_preprocess_input(list(df1, df2))
  expect_equal(nrow(result), 4)
  expect_true("batch" %in% colnames(result))
  expect_true("run_order" %in% colnames(result))
  expect_equal(result$batch, c("plate1", "plate1", "plate2", "plate2"))
  expect_equal(result$run_order, 1:4)
})

test_that("bc_preprocess_input maps sample_plate_id to batch", {
  df <- data.frame(
    sample_name = "S1",
    sample_plate_id = "plate1",
    sample_type = "qc",
    sample_run_index = 1,
    metab_A = 100,
    stringsAsFactors = FALSE
  )
  result <- bc_preprocess_input(df)
  expect_true("batch" %in% colnames(result))
  expect_equal(result$batch, "plate1")
})

test_that("bc_preprocess_input maps sample_run_index to run_order", {
  df <- data.frame(
    sample_name = "S1",
    batch = "plate1",
    sample_type = "qc",
    sample_run_index = 42,
    metab_A = 100,
    stringsAsFactors = FALSE
  )
  result <- bc_preprocess_input(df)
  expect_true("run_order" %in% colnames(result))
  expect_equal(result$run_order, 42)
})

test_that("bc_preprocess_input uses sample_type_factor for sample_type", {
  df <- data.frame(
    sample_name = c("S1", "S2"),
    batch = "plate1",
    sample_type = c("qc", "sample"),
    sample_type_factor = c("vltr", "pqc"),
    run_order = 1:2,
    metab_A = c(10, 20),
    stringsAsFactors = FALSE
  )
  result <- bc_preprocess_input(df)
  expect_equal(result$sample_type, c("vltr", "pqc"))
})

test_that("bc_preprocess_input does not rename if canonical columns already exist", {
  df <- data.frame(
    sample_name = "S1",
    batch = "plate1",
    sample_type = "qc",
    run_order = 1,
    sample_plate_id = "ignore_me",
    metab_A = 100,
    stringsAsFactors = FALSE
  )
  result <- bc_preprocess_input(df)
  expect_equal(result$batch, "plate1")
  expect_true("sample_plate_id" %in% colnames(result))
})

test_that("bc_preprocess_input passes through single data.frame unchanged when canonical", {
  df <- make_bc_test_data()
  result <- bc_preprocess_input(df)
  expect_equal(colnames(result), colnames(df))
  expect_equal(nrow(result), nrow(df))
})

test_that("bc_preprocess_input errors on empty list", {
  expect_error(bc_preprocess_input(list()), "list is empty")
})

test_that("bc_preprocess_input errors on list with non-dataframe elements", {
  expect_error(
    bc_preprocess_input(list("not_a_df", data.frame(x = 1))),
    "must be data.frames"
  )
})

test_that("bc_detect_metabolite_columns excludes sample_ prefixed columns", {
  df <- data.frame(
    sample_name = "S1",
    batch = "plate1",
    sample_type = "qc",
    run_order = 1,
    sample_plate_order = 1,
    sample_run_index = 1,
    metab_A = 100,
    metab_B = 200,
    stringsAsFactors = FALSE
  )
  result <- bc_detect_metabolite_columns(df)
  expect_equal(sort(result), c("metab_A", "metab_B"))
  expect_false("sample_plate_order" %in% result)
  expect_false("sample_run_index" %in% result)
})

# ============================================================================
# bc_export_html_report ----
# ============================================================================

test_that("bc_export_html_report errors when rmarkdown package missing", {
  # Stub requireNamespace to simulate missing rmarkdown
  mockery::stub(bc_export_html_report, "requireNamespace",
                function(pkg, ...) pkg != "rmarkdown")
  expect_error(
    bc_export_html_report(list(correction_summary = data.frame(x = 1))),
    "rmarkdown"
  )
})

test_that("bc_export_html_report errors when DT package missing", {
  mockery::stub(bc_export_html_report, "requireNamespace",
                function(pkg, ...) pkg != "DT")
  expect_error(
    bc_export_html_report(list(correction_summary = data.frame(x = 1))),
    "DT"
  )
})

test_that("bc_export_html_report errors when result lacks correction_summary", {
  # Input validation runs before the rmarkdown/DT dependency check, so this
  # test executes in CI environments that don't install the Suggests render
  # deps. Guards H4 in the bug-review plan.
  expect_error(
    bc_export_html_report(list()),
    "correction_summary"
  )
  expect_error(
    bc_export_html_report(list(correction_summary = NULL)),
    "correction_summary"
  )
})

test_that("bc_export_html_report renders HTML report to requested output path", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")
  skip_if_not(nzchar(Sys.which("pandoc")) ||
              (requireNamespace("rmarkdown", quietly = TRUE) &&
               isTRUE(try(rmarkdown::pandoc_available(), silent = TRUE))),
              "pandoc not available")

  tmp_out <- tempfile(fileext = ".html")
  on.exit(if (file.exists(tmp_out)) file.remove(tmp_out), add = TRUE)

  fake_result <- list(
    correction_summary = data.frame(metabolite = "A", improved = TRUE)
  )
  fake_rendered <- tmp_out

  # Stub rmarkdown::render so we don't need a real template / pandoc run
  mockery::stub(bc_export_html_report, "rmarkdown::render",
                function(...) {
                  file.create(fake_rendered)
                  fake_rendered
                })
  # Pretend the template file is found
  mockery::stub(bc_export_html_report, "system.file",
                function(...) fake_rendered)

  out <- suppressMessages(bc_export_html_report(
    fake_result,
    original_data = NULL,
    output_file = tmp_out,
    open = FALSE
  ))
  expect_equal(normalizePath(out, mustWork = FALSE),
               normalizePath(fake_rendered, mustWork = FALSE))
  expect_true(file.exists(tmp_out))
})

test_that("bc_export_html_report combines list of data.frames via bind_rows", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")

  tmp_out <- tempfile(fileext = ".html")
  on.exit(if (file.exists(tmp_out)) file.remove(tmp_out), add = TRUE)

  captured <- NULL
  mockery::stub(bc_export_html_report, "rmarkdown::render",
                function(..., params, envir, quiet) {
                  captured <<- params$original_data
                  file.create(tmp_out)
                  tmp_out
                })
  mockery::stub(bc_export_html_report, "system.file",
                function(...) tmp_out)

  lst <- list(
    data.frame(a = 1:2, b = 3:4),
    data.frame(a = 5:6, b = 7:8)
  )
  suppressMessages(bc_export_html_report(
    list(correction_summary = data.frame(x = 1)),
    original_data = lst,
    output_file = tmp_out,
    open = FALSE
  ))
  expect_s3_class(captured, "data.frame")
  expect_equal(nrow(captured), 4)
})

test_that("bc_export_html_report errors when template cannot be located", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("DT")

  mockery::stub(bc_export_html_report, "system.file",
                function(...) "")
  expect_error(
    bc_export_html_report(
      list(correction_summary = data.frame(x = 1)),
      open = FALSE
    ),
    "template not found"
  )
})

# ============================================================================
# bc_reconstruct_output: dropped-feature message tests ----
# ============================================================================

test_that("emits message listing features dropped by statTarget", {
  original <- data.frame(
    sample_name = paste0("S", 1:3),
    batch = "b1", sample_type = "qc", run_order = 1:3,
    metab_A = c(100, 200, 300),
    metab_B = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  # corrected_adjusted only has metab_A; metab_B was dropped by statTarget
  corrected_adj <- data.frame(
    sample_name = paste0("S", 1:3),
    metab_A = c(110, 210, 310),
    stringsAsFactors = FALSE
  )
  expect_message(
    suppressWarnings(
      bc_reconstruct_output(corrected_adj, original, c("metab_A", "metab_B"))
    ),
    "feature\\(s\\) were dropped"
  )
})

test_that("emits no message when no features are dropped", {
  original <- data.frame(
    sample_name = paste0("S", 1:3),
    batch = "b1", sample_type = "qc", run_order = 1:3,
    metab_A = c(100, 200, 300),
    metab_B = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  corrected_adj <- data.frame(
    sample_name = paste0("S", 1:3),
    metab_A = c(110, 210, 310),
    metab_B = c(11, 21, 31),
    stringsAsFactors = FALSE
  )
  messages_seen <- character(0)
  withCallingHandlers(
    bc_reconstruct_output(corrected_adj, original, c("metab_A", "metab_B")),
    message = function(m) {
      messages_seen <<- c(messages_seen, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_length(messages_seen, 0)
})
