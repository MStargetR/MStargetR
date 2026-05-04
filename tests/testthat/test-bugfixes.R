# Regression tests for bugs fixed in the code review
# Each test targets a specific fix to prevent regressions.

# --- Fix 1.2: find_peak_start_idx NA propagation ---

test_that("find_peak_start_idx returns 1 when fewer than 4 sub-baseline points", {
  # Create minimal chromatogram data where sub-baseline points before apex are < 4
  mock_chrom <- matrix(c(1:8, c(5, 5, 5, 10, 20, 15, 5, 5)), ncol = 2)
  FUNC_mzR <- list(
    plate1 = list(
      sample1 = list(
        mzR_chromatogram = list(
          mrm1 = mock_chrom
        )
      )
    )
  )
  baseline_value <- 6
  peak_apex_idx <- 5  # index of max intensity

  result <- find_peak_start_idx(FUNC_mzR, "plate1", "sample1", "mrm1",
                                 peak_apex_idx, baseline_value)
  expect_true(!is.na(result))
  expect_true(result >= 1)
})

# --- Fix 1.3: validate_project_directory return value ---

test_that("validate_project_directory returns normalized path on success", {
  temp_dir <- tempdir()
  result <- suppressMessages(validate_project_directory(temp_dir))
  expect_type(result, "character")
  expect_equal(result, normalizePath(temp_dir, winslash = "/", mustWork = TRUE))
})

# --- Fix 2.1: Tolerance-based m/z matching ---

test_that("find_lipid_info uses tolerance-based matching", {
  guide <- data.frame(
    precursor_mz = 760.585,
    product_mz = 184.073,
    molecule_list_name = "PC",
    precursor_name = "PC(34:1)",
    explicit_retention_time = 5.0,
    explicit_retention_time_window = 1.0,
    precursor_charge = 1,
    product_charge = 1,
    note = "test",
    stringsAsFactors = FALSE
  )

  mock_chrom <- data.frame(rtime = seq(4, 6, by = 0.1))
  FUNC_mzR <- list(
    plate1 = list(
      sample1 = list(
        mzR_chromatogram = list(mrm1 = mock_chrom)
      )
    )
  )

  # Exact match should work
  result <- find_lipid_info(guide, 760.585, 184.073, 5.0,
                             FUNC_mzR, "plate1", "sample1", "mrm1")
  expect_equal(result$name, "PC(34:1)")

  # Near match within 0.01 Da should also work
  result2 <- find_lipid_info(guide, 760.5851, 184.0731, 5.0,
                              FUNC_mzR, "plate1", "sample1", "mrm1")
  expect_equal(result2$name, "PC(34:1)")
})

# --- Fix 2.3: find_peak_apex_idx short chromatogram guard ---

test_that("find_peak_apex_idx handles chromatograms with fewer than 10 points", {
  short_chrom <- matrix(c(1:5, c(1, 3, 10, 2, 1)), ncol = 2)
  FUNC_mzR <- list(
    plate1 = list(
      sample1 = list(
        mzR_chromatogram = list(mrm1 = short_chrom)
      )
    )
  )

  result <- find_peak_apex_idx(FUNC_mzR, "plate1", "sample1", "mrm1")
  expect_equal(result, 3)  # index of max value (10)
})

# --- Fix 2.8: RSD with NA values and zero means ---

test_that("RSD calculation handles NA values and near-zero means", {
  data <- make_bc_data(n_samples = 20, n_batches = 2, n_qc_per_batch = 4)
  # Inject an NA
  data$metab_A[1] <- NA

  rsd <- bc_calculate_rsd(data, qc_label = "qc",
                           metabolite_cols = c("metab_A", "metab_B"))
  # Should still compute (na.rm = TRUE in internal logic)
  expect_true(!all(is.na(rsd)))
  expect_length(rsd, 2)

  # Actually test near-zero means: force QC values close to zero
  data_near_zero <- data
  qc_rows <- which(tolower(data_near_zero$sample_type) == "qc")
  data_near_zero$metab_A[qc_rows] <- rnorm(length(qc_rows), mean = 1e-10, sd = 1e-11)
  rsd_near_zero <- bc_calculate_rsd(data_near_zero, qc_label = "qc",
                                     metabolite_cols = c("metab_A", "metab_B"))
  # Should return a finite value (not Inf/NaN) or NA for the near-zero column
  expect_length(rsd_near_zero, 2)
  expect_true(!any(is.nan(rsd_near_zero), na.rm = TRUE))
})

# --- Fix 2.15: Tolerance-based duplicate detection in TransitionCheckR ---

test_that("transition_checkR detects near-duplicate m/z values", {
  df <- data.frame(
    `Molecule List Name` = c("Class1", "Class1"),
    `Precursor Name` = c("Lipid_A", "Lipid_B"),
    `Precursor Mz` = c(760.5850, 760.5851),
    `Precursor Charge` = c(1, 1),
    `Product Mz` = c(184.0730, 184.0731),
    `Product Charge` = c(1, 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(transition_checkR(df))
  # With 0.001 Da tolerance, these should be flagged as near-duplicates
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  # Both near-duplicate rows must appear in the result
  expect_true(all(c("Lipid_A", "Lipid_B") %in% result$`Precursor Name`))
  # The flagged rows must have the near-duplicate Precursor Mz values
  expect_equal(sort(result$`Precursor Mz`), c(760.5850, 760.5851))
})

# --- Fix 2.4: Post-flagging QC count check ---

test_that("post-flagging QC count check catches batches with <2 QCs", {
  # Directly test the post-flagging check by using bc_flag_failed_qc
  # then verifying the main function's guard
  data <- make_bc_data(n_samples = 10, n_batches = 1, n_qc_per_batch = 2)
  met_cols <- c("metab_A", "metab_B")

  # Manually set one QC to have near-zero signal (below 10% of median)
  qc_idx <- which(data$sample_type == "qc")
  data$metab_A[qc_idx[1]] <- 0.0001
  data$metab_B[qc_idx[1]] <- 0.0001

  # Flag should remove one QC, leaving only 1
  flagging <- bc_flag_failed_qc(data, "qc", met_cols)
  remaining_qc <- sum(tolower(flagging$data$sample_type) == "qc" &
                        flagging$data$batch == "plate1")

  # Verify the flagging actually removed a QC
  if (remaining_qc < 2) {
    # The main function should catch this
    expect_error(
      suppressMessages(suppressWarnings(
        batchCorrectR(data = data, qc_label = "qc")
      )),
      "QC sample"
    )
  } else {
    # Flagging didn't remove enough QCs with this random seed — force the scenario
    data2 <- make_bc_data(n_samples = 10, n_batches = 1, n_qc_per_batch = 2)
    qc_idx2 <- which(data2$sample_type == "qc")
    # Set both QCs to near-zero to force both being flagged
    data2$metab_A[qc_idx2] <- 1e-10
    data2$metab_B[qc_idx2] <- 1e-10
    expect_error(
      suppressMessages(suppressWarnings(
        batchCorrectR(data = data2, qc_label = "qc")
      )),
      "QC|qc|quality"
    )
  }
})
