# Tests for Medium-severity audit findings: BC-009 through BC-034

suppressPackageStartupMessages(library(dplyr))

# BC-009: parse_sample_timestamp initialises NA vector with tz="UTC"

test_that("parse_sample_timestamp NA vector initialised with UTC timezone (BC-009)", {
  result <- MStargetR:::parse_sample_timestamp(c("2021-03-13T18:12:31Z", NA))
  expect_true(inherits(result, "POSIXct"))
  expect_equal(attr(result, "tzone"), "UTC")
})

# BC-013: bc_validate_input warns on overlapping run_order ranges (no timestamp)

test_that("bc_validate_input warns when per-batch run_order ranges overlap (BC-013)", {
  df <- data.frame(
    sample_name = paste0("S", 1:8),
    batch = rep(c("b1", "b2"), each = 4),
    sample_type = rep(c("qc", "sample", "sample", "qc"), 2),
    run_order = c(1L, 2L, 3L, 4L, 2L, 3L, 4L, 5L),
    met1 = rnorm(8, 100, 10),
    stringsAsFactors = FALSE
  )
  expect_warning(
    MStargetR:::bc_validate_input(df, "qc", "QCRFSC", 500, 100, 0, "minHalf"),
    "overlaps"
  )
})

test_that("bc_validate_input does NOT warn when run_order ranges are disjoint (BC-013)", {
  df <- data.frame(
    sample_name = paste0("S", 1:8),
    batch = rep(c("b1", "b2"), each = 4),
    sample_type = rep(c("qc", "sample", "sample", "qc"), 2),
    run_order = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
    met1 = rnorm(8, 100, 10),
    stringsAsFactors = FALSE
  )
  expect_silent(
    MStargetR:::bc_validate_input(df, "qc", "QCRFSC", 500, 100, 0, "minHalf")
  )
})

# BC-015: bc_detect_stattarget_format uses all(grepl("^M\\d+$")) heuristic

test_that("bc_detect_stattarget_format uses strict M-number check not single 'M1' (BC-015)", {
  df_user_m1 <- data.frame(
    sample = c("class", "QC1", "sample1"),
    M1 = c("qc", "100", "200"),
    glucose = c("qc", "50", "60"),
    stringsAsFactors = FALSE
  )
  result <- MStargetR:::bc_detect_stattarget_format(df_user_m1)
  expect_true("name" %in% colnames(result))
})

test_that("bc_detect_stattarget_format takes transpose branch when all cols are M-numbers (BC-015)", {
  df_all_m <- data.frame(
    sample = c("class", "QC1", "sample1"),
    M1 = c("qc", "100", "200"),
    M2 = c("qc", "50", "60"),
    stringsAsFactors = FALSE
  )
  # The transpose path coerces character cells (the "qc" class label) to
  # numeric, which dplyr surfaces as "NAs introduced by coercion"; this is
  # expected for the synthetic header row.
  result <- suppressWarnings(MStargetR:::bc_detect_stattarget_format(df_all_m))
  expect_true("name" %in% colnames(result))
})

# BC-016: bc_apply_mean_ratios emits message for zero/NA ratios

test_that("bc_apply_mean_ratios emits message when ratios are zero or NA (BC-016)", {
  data <- data.frame(met1 = c(10, 20), met2 = c(30, 40))
  ratios <- c(met1 = 0, met2 = 2)
  expect_message(
    MStargetR:::bc_apply_mean_ratios(data, ratios),
    "left uncorrected"
  )
})

test_that("bc_apply_mean_ratios leaves zero-ratio column unchanged (BC-016)", {
  data <- data.frame(met1 = c(10, 20), met2 = c(30, 40))
  ratios <- c(met1 = 0, met2 = 2)
  suppressMessages(result <- MStargetR:::bc_apply_mean_ratios(data, ratios))
  expect_equal(result$met1, c(10, 20))
  expect_equal(result$met2, c(15, 20))
})

# BC-017: bc_prepare_combat_matrix drops all-NA rows instead of zero-filling

test_that("bc_prepare_combat_matrix drops all-NA feature rows (BC-017)", {
  data <- data.frame(
    met1 = c(1, 2, 3, 4),
    met2 = c(NA_real_, NA_real_, NA_real_, NA_real_),
    met3 = c(5, 6, 7, 8),
    batch = c("b1", "b1", "b2", "b2"),
    stringsAsFactors = FALSE
  )
  result <- MStargetR:::bc_prepare_combat_matrix(data, c("met1", "met2", "met3"))
  expect_false("met2" %in% result$kept_features)
  expect_true("met1" %in% result$kept_features)
})

# BC-018: bc_prepare_combat_matrix row-median imputation (vectorised path)

test_that("bc_prepare_combat_matrix imputes partial-NA rows with row median (BC-018)", {
  data <- data.frame(
    met1 = c(10, NA, 30, 40),
    met2 = c(5, 15, 25, 35),
    batch = c("b1", "b1", "b2", "b2"),
    stringsAsFactors = FALSE
  )
  result <- MStargetR:::bc_prepare_combat_matrix(data, c("met1", "met2"))
  expect_false(any(is.na(result$dat_combat)))
})

# BC-019: bc_reconstruct_combat_output uses vectorised assignment

test_that("bc_reconstruct_combat_output returns correct values for all kept features (BC-019)", {
  data <- data.frame(met1 = c(1, 2), met2 = c(3, 4), batch = c("b1", "b2"))
  # corrected_matrix is features x samples: met1=[10,30], met2=[20,40]
  # t() gives samples x features: sample1=[10,20], sample2=[30,40]
  corrected_matrix <- matrix(c(10, 20, 30, 40), nrow = 2,
                              dimnames = list(c("met1", "met2"), NULL))
  result <- MStargetR:::bc_reconstruct_combat_output(data, corrected_matrix,
                                                      c("met1", "met2"))
  # After t(): corrected_values[1,] = c(10,20), corrected_values[2,] = c(30,40)
  expect_equal(result$met1, c(10, 30))
  expect_equal(result$met2, c(20, 40))
})

# BC-020: unique() applied after tolower() in keep_tags

test_that("batchCorrectR deduplicates keep_tags after tolower (BC-020)", {
  df <- data.frame(
    sample_name = paste0("S", 1:8),
    batch = rep(c("b1", "b2"), each = 4),
    sample_type = rep(c("qc", "sample", "sample", "qc"), 2),
    run_order = 1:8,
    met1 = rnorm(8, 100, 10),
    met2 = rnorm(8, 200, 20),
    stringsAsFactors = FALSE
  )
  expect_error(
    suppressWarnings(
      batchCorrectR(df, qc_label = "qc", sample_tags = c("Sample", "sample"),
                    method = "QCRFSC", plot = FALSE, report = FALSE)
    ),
    NA
  )
})

# BC-021: original_data_raw is ungrouped after filter

test_that("batchCorrectR ungroups original_data_raw after sample_tags filter (BC-021)", {
  df <- data.frame(
    sample_name = paste0("S", 1:8),
    batch = rep(c("b1", "b2"), each = 4),
    sample_type = rep(c("qc", "sample", "blank", "qc"), 2),
    run_order = 1:8,
    met1 = rnorm(8, 100, 10),
    met2 = rnorm(8, 200, 20),
    stringsAsFactors = FALSE
  )
  result <- suppressWarnings(
    batchCorrectR(df, qc_label = "qc", sample_tags = c("sample"),
                  method = "QCRFSC", plot = FALSE, report = FALSE)
  )
  expect_false(dplyr::is_grouped_df(result$corrected_data))
})

# BC-022: signif loop uses pre-filtered intersect

test_that("batchCorrectR signif rounding applies to all metabolite columns (BC-022)", {
  df <- data.frame(
    sample_name = paste0("S", 1:8),
    batch = rep(c("b1", "b2"), each = 4),
    sample_type = rep(c("qc", "sample", "sample", "qc"), 2),
    run_order = 1:8,
    met1 = rnorm(8, 100.12345, 10),
    met2 = rnorm(8, 200.98765, 20),
    stringsAsFactors = FALSE
  )
  result <- suppressWarnings(
    batchCorrectR(df, qc_label = "qc",
                  method = "QCRFSC", plot = FALSE, report = FALSE)
  )
  vals <- result$corrected_data$met1
  expect_true(all(vals == signif(vals, 3) | is.na(vals)))
})

# BC-024: duplicate (batch, sample_name) warning is added to bc_validate_input.
# The global unique-name stop fires before the batch+name check can be reached for
# the same-name-same-batch case, so we verify the global stop still fires correctly.

test_that("bc_validate_input stops on globally duplicate sample_name (BC-024)", {
  df <- data.frame(
    sample_name = c("S1", "S1", "S2", "S3", "S4", "S5"),
    batch = c("b1", "b1", "b1", "b1", "b2", "b2"),
    sample_type = c("qc", "sample", "sample", "qc", "qc", "sample"),
    run_order = 1:6,
    met1 = rnorm(6),
    stringsAsFactors = FALSE
  )
  expect_error(
    MStargetR:::bc_validate_input(df, "qc", "QCRFSC", 500, 100, 0, "minHalf"),
    "sample_name.*unique|unique.*sample_name"
  )
})

# BC-025: bc_detect_metabolite_columns logs detected columns

test_that("bc_detect_metabolite_columns emits message listing detected columns (BC-025)", {
  df <- data.frame(sample_name = "S1", batch = "b1", sample_type = "qc",
                   run_order = 1L, met1 = 1.0, met2 = 2.0)
  expect_message(
    MStargetR:::bc_detect_metabolite_columns(df),
    "Detected metabolite"
  )
})

test_that("bc_detect_metabolite_columns warns on metadata-like column names (BC-025)", {
  df <- data.frame(sample_name = "S1", batch = "b1", sample_type = "qc",
                   run_order = 1L, met1 = 1.0, injection_volume = 2.0)
  expect_warning(
    suppressMessages(MStargetR:::bc_detect_metabolite_columns(df)),
    "metadata"
  )
})

# BC-026: sample_type is always character after bc_preprocess_input

test_that("bc_preprocess_input coerces sample_type to character (BC-026)", {
  df <- data.frame(
    sample_name = paste0("S", 1:3),
    batch = "b1",
    sample_type = factor(c("qc", "sample", "blank")),
    run_order = 1:3,
    met1 = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  result <- MStargetR:::bc_preprocess_input(df)
  expect_type(result$sample_type, "character")
})

# BC-028: explicit indexing in case_when for statTarget sample labels

test_that("bc_prepare_pheno_file assigns contiguous QC/sample labels (BC-028)", {
  df <- data.frame(
    sample_name = paste0("S", 1:8),
    batch = rep(c("b1", "b2"), each = 4),
    sample_type = rep(c("qc", "sample", "sample", "qc"), 2),
    run_order = 1:8,
    met1 = rnorm(8, 100, 10),
    stringsAsFactors = FALSE
  )
  td <- tempdir()
  pheno <- MStargetR:::bc_prepare_pheno_file(df, "qc", td)
  qc_rows <- pheno[pheno$class == "qc", ]
  sample_rows <- pheno[pheno$class == "sample", ]
  expect_true(all(grepl("^QC\\d+$", qc_rows$sample)))
  expect_true(all(grepl("^sample\\d+$", sample_rows$sample)))
  expect_equal(sort(qc_rows$sample),
               paste0("QC", seq_len(nrow(qc_rows))))
})

# BC-029: bc_prepare_profile_file uses numeric-only transpose

test_that("bc_prepare_profile_file writes numeric values without locale coercion (BC-029)", {
  df <- data.frame(
    sample_name = paste0("S", 1:6),
    batch = rep(c("b1", "b2"), each = 3),
    sample_type = rep(c("qc", "sample", "qc"), 2),
    run_order = 1:6,
    met1 = c(1.23456, 2.34567, 3.45678, 4.56789, 5.67890, 6.78901),
    stringsAsFactors = FALSE
  )
  td <- tempfile()
  dir.create(td)
  pheno <- MStargetR:::bc_prepare_pheno_file(df, "qc", td)
  result <- MStargetR:::bc_prepare_profile_file(df, "met1", pheno, td)
  csv_data <- readr::read_csv(file.path(td, "ProfileFile.csv"),
                               show_col_types = FALSE)
  numeric_cols <- setdiff(names(csv_data), "name")
  vals <- unlist(csv_data[numeric_cols])
  expect_true(all(is.numeric(vals) | is.na(vals)))
})

# BC-032: bc_build_correction_summary warns on name mismatch

test_that("bc_build_correction_summary warns when names do not match metabolite_cols (BC-032)", {
  mets <- c("met1", "met2")
  rsd_before <- c(met1 = 10, met3 = 20)
  rsd_after <- c(met1 = 5, met2 = 8)
  expect_warning(
    MStargetR:::bc_build_correction_summary(mets, rsd_before, rsd_after),
    "do not match"
  )
})

# BC-033: bc_plot_pca vectorised imputation (no for loop for NA)
# BC-034: bc_plot_pca emits message with imputation fraction

test_that("bc_plot_pca emits imputation message when NAs present (BC-033/BC-034)", {
  df_orig <- data.frame(
    sample_name = paste0("S", 1:6),
    batch = rep(c("b1", "b2"), each = 3),
    sample_type = rep(c("qc", "sample", "sample"), 2),
    run_order = 1:6,
    met1 = c(1, NA, 3, 4, 5, 6),
    met2 = c(10, 20, 30, 40, 50, 60),
    met3 = c(5, 6, 7, 8, 9, 10),
    stringsAsFactors = FALSE
  )
  df_corr <- df_orig
  df_corr$met1 <- df_corr$met1 + 0.1
  expect_message(
    MStargetR:::bc_plot_pca(df_orig, df_corr, c("met1", "met2", "met3")),
    "imputing"
  )
})
