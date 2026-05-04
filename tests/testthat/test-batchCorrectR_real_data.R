# Test batchCorrectR with real MStargetR-format data
# Uses a subset of lipid columns from actual lipidomics run

# --- Factory: returns a fresh list(plate1, plate2) each call ---
# Using a factory prevents file-scope mutation from leaking between tests.
make_bc_plates <- function() {
  plate1 <- data.frame(
    sample_run_index = 1:6,
    sample_name = c(
      "VLTR_PS_20", "VPQC_21", "PQC_22",
      "LTR_23", "COV19868_002_24", "COV19831_035_25"
    ),
    sample_ID = c(
      "VLTR_PS_20", "VPQC_21", "PQC_22",
      "LTR_23", "COV19868_002_24", "COV19831_035_25"
    ),
    sample_timestamp = rep("2024/12/13", 6),
    sample_plate_id = rep("plate_A", 6),
    sample_plate_order = 1:6,
    sample_matrix = rep("PLA", 6),
    sample_type = c("qc", "sample", "sample", "sample", "sample", "sample"),
    sample_type_factor = c("vltr", "pqc", "pqc", "ltr", "sample", "sample"),
    sample_type_factor_rev = c("vltr", "pqc", "pqc", "ltr", "sample", "sample"),
    sample_data_source = rep("concentration.imputed", 6),
    `CE(14:0)` = c(0.533, 0.952, 1.04, 0.657, 0.931, 0.46),
    `CE(16:0)` = c(8.76, 14.3, 20.93, 10.69, 22.05, 8.62),
    `CE(18:1)` = c(43.69, 53.04, 71.84, 54.86, 71.84, 44.49),
    `CE(18:2)` = c(413.04, 498.86, 864.41, 387.46, 643.92, 507.46),
    `CE(20:4)` = c(420.52, 549.63, 957.42, 637.26, 1070.71, 496.19),
    `LPC(16:0)` = c(208.69, 105.4, 99.48, 109.99, 123.81, 85.2),
    `LPC(18:0)` = c(1.65, 1.53, 1.47, 1.52, 1.66, 1.2),
    `LPC(18:1)` = c(111.01, 51.41, 60.54, 63.81, 49.46, 37.56),
    `SM(16:0)` = c(36.92, 40.26, 41.37, 44.29, 44.02, 46.27),
    `SM(18:0)` = c(11.74, 9.77, 10.13, 10.55, 14.0, 13.49),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  plate2 <- data.frame(
    sample_run_index = 7:12,
    sample_name = c(
      "VLTR_PS_40", "VPQC_41", "PQC_42",
      "LTR_43", "COV19999_003_44", "COV19900_036_45"
    ),
    sample_ID = c(
      "VLTR_PS_40", "VPQC_41", "PQC_42",
      "LTR_43", "COV19999_003_44", "COV19900_036_45"
    ),
    sample_timestamp = rep("2024/12/14", 6),
    sample_plate_id = rep("plate_B", 6),
    sample_plate_order = 1:6,
    sample_matrix = rep("PLA", 6),
    sample_type = c("qc", "sample", "sample", "sample", "sample", "sample"),
    sample_type_factor = c("vltr", "pqc", "pqc", "ltr", "sample", "sample"),
    sample_type_factor_rev = c("vltr", "pqc", "pqc", "ltr", "sample", "sample"),
    sample_data_source = rep("concentration.imputed", 6),
    # Slightly shifted values to simulate batch effect
    `CE(14:0)` = c(0.633, 1.052, 1.14, 0.757, 1.031, 0.56) * 1.1,
    `CE(16:0)` = c(9.76, 15.3, 21.93, 11.69, 23.05, 9.62) * 1.1,
    `CE(18:1)` = c(44.69, 54.04, 72.84, 55.86, 72.84, 45.49) * 1.1,
    `CE(18:2)` = c(423.04, 508.86, 874.41, 397.46, 653.92, 517.46) * 1.1,
    `CE(20:4)` = c(430.52, 559.63, 967.42, 647.26, 1080.71, 506.19) * 1.1,
    `LPC(16:0)` = c(218.69, 115.4, 109.48, 119.99, 133.81, 95.2) * 1.1,
    `LPC(18:0)` = c(1.75, 1.63, 1.57, 1.62, 1.76, 1.3) * 1.1,
    `LPC(18:1)` = c(121.01, 61.41, 70.54, 73.81, 59.46, 47.56) * 1.1,
    `SM(16:0)` = c(37.92, 41.26, 42.37, 45.29, 45.02, 47.27) * 1.1,
    `SM(18:0)` = c(12.74, 10.77, 11.13, 11.55, 15.0, 14.49) * 1.1,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  list(plate1 = plate1, plate2 = plate2)
}

# ============================================================================
# Test 1: Preprocessing works with list of data.frames
# ============================================================================
test_that("Preprocessing works with list of data.frames", {
  plates <- make_bc_plates()
  combined <- bc_preprocess_input(list(plates$plate1, plates$plate2))

  expect_equal(nrow(combined), 12)
  expect_true("batch" %in% colnames(combined))
  expect_true("run_order" %in% colnames(combined))
  expect_equal(combined$batch, c(rep("plate_A", 6), rep("plate_B", 6)))
  expect_equal(combined$run_order, 1:12)
  # sample_type should now be from sample_type_factor
  expect_equal(
    combined$sample_type,
    c("vltr", "pqc", "pqc", "ltr", "sample", "sample",
      "vltr", "pqc", "pqc", "ltr", "sample", "sample")
  )
})

# ============================================================================
# Test 2: Metabolite column detection excludes sample_* metadata
# ============================================================================
test_that("Metabolite column detection excludes sample_* metadata", {
  plates <- make_bc_plates()
  combined <- bc_preprocess_input(list(plates$plate1, plates$plate2))
  met_cols <- bc_detect_metabolite_columns(combined)

  expect_equal(length(met_cols), 10)
  expect_true(all(c("CE(14:0)", "CE(16:0)", "SM(16:0)") %in% met_cols))
  expect_false(any(grepl("^sample_", met_cols)))
  expect_false("run_order" %in% met_cols)
  expect_false("batch" %in% met_cols)
})

# ============================================================================
# Test 3: Validation passes with pqc as qc_label
# ============================================================================
test_that("Validation passes with qc_label = 'pqc'", {
  plates <- make_bc_plates()
  combined <- bc_preprocess_input(list(plates$plate1, plates$plate2))

  result <- bc_validate_input(combined, qc_label = "pqc", method = "QCRFSC",
                              ntree = 500, coCV = 100, Frule = 0,
                              imputeM = "minHalf")
  expect_true(is.null(result) || is.logical(result) || is.list(result))
})

# ============================================================================
# Test 4: QC flagging works with sample_type_factor values
# ============================================================================
test_that("QC flagging works with pqc label", {
  plates <- make_bc_plates()
  combined <- bc_preprocess_input(list(plates$plate1, plates$plate2))
  met_cols <- bc_detect_metabolite_columns(combined)

  flagging <- bc_flag_failed_qc(combined, qc_label = "pqc", metabolite_cols = met_cols)

  expect_type(flagging, "list")
  expect_true("data" %in% names(flagging))
  expect_true("failed_samples" %in% names(flagging))
})

# ============================================================================
# Test 5: RSD calculation works
# ============================================================================
test_that("QC RSD calculation works", {
  plates <- make_bc_plates()
  combined <- bc_preprocess_input(list(plates$plate1, plates$plate2))
  met_cols <- bc_detect_metabolite_columns(combined)

  rsd <- bc_calculate_rsd(combined, qc_label = "pqc", metabolite_cols = met_cols)

  expect_equal(length(rsd), length(met_cols))
  expect_false(any(is.na(rsd)))
})

# ============================================================================
# Test 6: Full pipeline with statTarget (if available)
# ============================================================================
test_that("Full batchCorrectR pipeline works with statTarget", {
  skip_if_not_installed("statTarget")
  plates <- make_bc_plates()

  result <- suppressWarnings(batchCorrectR(
    data = list(plates$plate1, plates$plate2),
    qc_label = "pqc",
    method = "QCRFSC",
    ntree = 100,
    plot = FALSE,
    report = FALSE
  ))

  expect_type(result, "list")
  expect_true("corrected_data" %in% names(result))
  expect_true("correction_summary" %in% names(result))
  # Output should have original column names
  expect_true("sample_plate_id" %in% colnames(result$corrected_data))
  expect_true("sample_type_factor" %in% colnames(result$corrected_data))
  expect_equal(nrow(result$corrected_data), 12)
  # sample_timestamp must be POSIXct so standalone output aligns with the
  # qcCheckR pipeline (which parses via extract_run_order).
  expect_s3_class(result$corrected_data$sample_timestamp, "POSIXct")
})

# ============================================================================
# Test: bc_preprocess_input strips .mzML and canonicalises run_order
# ============================================================================
test_that("bc_preprocess_input strips .mzML suffix and rebuilds run_order", {
  # Two plates where sample_run_index overlaps (1..3 on each) and sample_name
  # carries the .mzML suffix -- the combined output should have unique run_order
  # 1..6 in chronological order, bare sample_names, and POSIXct timestamps.
  a <- data.frame(
    sample_run_index = 1:3,
    sample_name = c("QC1.mzML", "S1.mzML", "QC2.mzML"),
    sample_timestamp = c("2024-01-01 10:00:00",
                          "2024-01-01 10:05:00",
                          "2024-01-01 10:10:00"),
    sample_plate_id = "A",
    sample_type = c("qc", "sample", "qc"),
    sample_type_factor = c("qc", "sample", "qc"),
    m1 = c(10, 20, 11),
    stringsAsFactors = FALSE
  )
  b <- data.frame(
    sample_run_index = 1:3,
    sample_name = c("QC3.mzML", "S2.mzML", "QC4.mzML"),
    sample_timestamp = c("2024-01-02 10:00:00",
                          "2024-01-02 10:05:00",
                          "2024-01-02 10:10:00"),
    sample_plate_id = "B",
    sample_type = c("qc", "sample", "qc"),
    sample_type_factor = c("qc", "sample", "qc"),
    m1 = c(12, 22, 13),
    stringsAsFactors = FALSE
  )
  combined <- bc_preprocess_input(list(a, b))

  expect_equal(combined$run_order, 1:6)
  expect_false(any(grepl("\\.mzML$", combined$sample_name, ignore.case = TRUE)))
  expect_s3_class(combined$sample_timestamp, "POSIXct")
  # Chronological ordering: plate A rows first, then plate B.
  expect_equal(combined$batch, c(rep("A", 3), rep("B", 3)))
})

# ============================================================================
# Test: QCRFSC output re-tags sample_data_source as .peakAreaCorrected
# ============================================================================
test_that("QCRFSC output tags sample_data_source with .peakAreaCorrected", {
  skip_if_not_installed("statTarget")
  plates <- make_bc_plates()

  result <- suppressWarnings(batchCorrectR(
    data = list(plates$plate1, plates$plate2),
    qc_label = "pqc",
    method = "QCRFSC",
    ntree = 100,
    plot = FALSE,
    report = FALSE
  ))

  expect_true("sample_data_source" %in% colnames(result$corrected_data))
  expect_true(all(result$corrected_data$sample_data_source == ".peakAreaCorrected"))
})

# ============================================================================
# Test: parse_sample_timestamp handles the formats qcCheckR accepts
# ============================================================================
test_that("parse_sample_timestamp normalises character input to POSIXct", {
  formats <- c(
    "2021-03-13T18:12:31Z",
    "2021-03-13 18:12:31",
    "2024/12/13",
    "09/27/2024 10:41:28",
    "13/03/2021 18:12:31"
  )
  parsed <- parse_sample_timestamp(formats)
  expect_s3_class(parsed, "POSIXct")
  expect_equal(length(parsed), length(formats))
  expect_false(any(is.na(parsed)))

  # POSIXct input is passed through unchanged.
  already <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  expect_identical(parse_sample_timestamp(already), already)
})
