# Extended tests for qcCheckR_Utils.R ----
# Covers: QC setup/classification, filtering, statTarget pipeline, exports, summaries
library(mockery)
library(dplyr)
library(tibble)

# ============================================================================
# QC Setup & Classification ----
# ============================================================================

# --- determine_qc_type ---
test_that("determine_qc_type: returns single passed QC type", {
  master_list <- list(
    project_details = list(
      global_qc_pass = c(pqc = "pass"),
      qc_type = "pqc"
    )
  )

  result <- suppressMessages(determine_qc_type(master_list))
  expect_equal(result, "pqc")
})

test_that("determine_qc_type: returns 'unknown' when no QC passes", {
  master_list <- list(
    project_details = list(
      global_qc_pass = c(pqc = "fail", ltr = "fail"),
      qc_type = "pqc"
    )
  )

  result <- suppressMessages(determine_qc_type(master_list))
  expect_equal(result, "unknown")
})

test_that("determine_qc_type: returns 'unknown' with NULL passed_qc", {
  master_list <- list(
    project_details = list(
      global_qc_pass = c(pqc = "fail"),
      qc_type = "pqc"
    )
  )

  result <- suppressMessages(determine_qc_type(master_list))
  expect_equal(result, "unknown")
})

test_that("determine_qc_type: reverts to user choice when multiple pass and user_qc is among them", {
  master_list <- list(
    project_details = list(
      global_qc_pass = c(pqc = "pass", ltr = "pass"),
      qc_type = "ltr"
    )
  )

  result <- suppressMessages(determine_qc_type(master_list))
  expect_equal(result, "ltr")
})

test_that("determine_qc_type: reverts to user choice case-insensitively", {
  # Regression: uppercase user QC ("LTR") vs lowercase tag keys ("ltr")
  # must still match rather than falling through to 'unknown'.
  master_list <- list(
    project_details = list(
      global_qc_pass = c(vltr = "pass", ltr = "pass"),
      qc_type = "LTR"
    )
  )

  result <- suppressMessages(determine_qc_type(master_list))
  expect_equal(result, "ltr")
})

test_that("determine_qc_type: returns 'unknown' when multiple pass but user_qc not among them", {
  master_list <- list(
    project_details = list(
      global_qc_pass = c(pqc = "pass", ltr = "pass"),
      qc_type = "bqc"
    )
  )

  result <- suppressMessages(determine_qc_type(master_list))
  expect_equal(result, "unknown")
})

# --- qcCheckR_set_qc ---
test_that("qcCheckR_set_qc: sets qc_type and initialises filters when QC is valid", {
  master_list <- list(
    project_details = list(
      global_qc_pass = c(pqc = "pass"),
      qc_type = "pqc",
      project_name = "test_project",
      qc_passed = list(plate1 = list(pqc = "pass"))
    )
  )

  stub(qcCheckR_set_qc, "determine_qc_type", "pqc")
  stub(qcCheckR_set_qc, "notify_qc_type", NULL)

  result <- suppressMessages(qcCheckR_set_qc(master_list))

  expect_equal(result$project_details$qc_type, "pqc")
  expect_type(result$filters, "list")
  expect_length(result$filters, 0)
})

test_that("qcCheckR_set_qc: stops with error when qc_type is unknown", {
  master_list <- list(
    project_details = list(
      global_qc_pass = c(pqc = "fail"),
      qc_type = "pqc",
      project_name = "test_project",
      qc_passed = list(plate1 = list(pqc = "fail"))
    )
  )

  stub(qcCheckR_set_qc, "determine_qc_type", "unknown")

  expect_error(
    suppressMessages(qcCheckR_set_qc(master_list)),
    "STOPPING SCRIPT"
  )
})

# --- stop_with_qc_error ---
test_that("stop_with_qc_error: produces error message with project name", {
  expect_error(
    stop_with_qc_error(
      project_name = "MyProject",
      global_qc_pass = list(pqc = "fail"),
      plate_qc_passed = list(plate1 = list(pqc = "fail"))
    ),
    "MyProject"
  )
})

test_that("stop_with_qc_error: includes 'STOPPING SCRIPT' in message", {
  expect_error(
    stop_with_qc_error(
      project_name = "test",
      global_qc_pass = list(),
      plate_qc_passed = list()
    ),
    "STOPPING SCRIPT"
  )
})

test_that("stop_with_qc_error: includes Global QC Assessment in message", {
  expect_error(
    stop_with_qc_error(
      project_name = "test",
      global_qc_pass = list(pqc = "fail"),
      plate_qc_passed = list()
    ),
    "Global QC Assessment"
  )
})


# ============================================================================
# Filtering functions ----
# ============================================================================

# --- qcCheckR_sample_filter ---
test_that("qcCheckR_sample_filter: flags sample with excessive missing values", {
  # Build mock sorted data with one good sample and one bad sample
  good_lipids <- rep(10000, 5)
  bad_lipids <- rep(0, 5)  # all below 5000 threshold
  good_sil <- rep(50000, 3)

  sorted_data <- tibble(
    sample_run_index = 1:4,
    sample_name = c("good1", "good2", "bad1", "good3"),
    sample_plate_id = rep("plate1", 4),
    sample_type_factor = c("sample", "qc", "sample", "qc"),
    lipid_A = c(10000, 12000, 0, 11000),
    lipid_B = c(10000, 13000, 0, 12000),
    lipid_C = c(10000, 11000, 0, 10000),
    lipid_D = c(10000, 14000, 0, 13000),
    lipid_E = c(10000, 15000, 0, 14000),
    SIL_A = c(50000, 55000, 50000, 52000),
    SIL_B = c(50000, 54000, 50000, 53000),
    SIL_C = c(50000, 56000, 50000, 51000)
  )

  master_list <- list(
    project_details = list(mv_sample_threshold = 50),
    data = list(peakArea = list(sorted = list(plate1 = sorted_data))),
    filters = list()
  )

  result <- qcCheckR_sample_filter(master_list)

  expect_true("bad1" %in% result$filters$failed_samples)
  expect_false("good1" %in% result$filters$failed_samples)
})

test_that("qcCheckR_sample_filter: no failures with clean data", {
  sorted_data <- tibble(
    sample_run_index = 1:3,
    sample_name = c("S1", "S2", "S3"),
    sample_plate_id = rep("plate1", 3),
    sample_type_factor = c("qc", "sample", "sample"),
    lipid_A = c(10000, 12000, 11000),
    lipid_B = c(10000, 13000, 12000),
    SIL_A = c(50000, 55000, 52000),
    SIL_B = c(50000, 54000, 53000)
  )

  master_list <- list(
    project_details = list(mv_sample_threshold = 50),
    data = list(peakArea = list(sorted = list(plate1 = sorted_data))),
    filters = list()
  )

  result <- qcCheckR_sample_filter(master_list)

  expect_length(result$filters$failed_samples, 0)
})

test_that("qcCheckR_sample_filter: handles multiple plates", {
  make_plate <- function(plate_id) {
    tibble(
      sample_run_index = 1:3,
      sample_name = paste0(plate_id, "_S", 1:3),
      sample_plate_id = rep(plate_id, 3),
      sample_type_factor = c("qc", "sample", "sample"),
      lipid_A = c(10000, 12000, 11000),
      SIL_A = c(50000, 55000, 52000)
    )
  }

  master_list <- list(
    project_details = list(mv_sample_threshold = 50),
    data = list(peakArea = list(sorted = list(
      plate1 = make_plate("plate1"),
      plate2 = make_plate("plate2")
    ))),
    filters = list()
  )

  result <- qcCheckR_sample_filter(master_list)

  # Expect rows from both plates in the missingValues summary
  expect_equal(nrow(result$filters$samples.missingValues), 6)
})

# --- calculate_rsd ---
test_that("calculate_rsd: returns correct RSD for known values", {
  # c(10, 10, 10) -> RSD = 0%
  # c(5, 10, 15) -> sd=5, mean=10 -> RSD = 50%
  batch_data <- tibble(
    sample_name = c("QC1", "QC2", "QC3"),
    sample_type = c("qc", "qc", "qc"),
    feature_zero_rsd = c(10, 10, 10),
    feature_fifty_rsd = c(5, 10, 15)
  )

  master_list <- list(
    filters = list(failed_samples = character())
  )

  result <- calculate_rsd(master_list, "peakArea", list(batch1 = batch_data))

  expect_equal(nrow(result), 1)
  # The V1 column is source name, V2 is batch name
  expect_true("feature_zero_rsd" %in% names(result))
  expect_true("feature_fifty_rsd" %in% names(result))
  expect_equal(result$feature_zero_rsd, 0)
  expect_equal(result$feature_fifty_rsd, 50, tolerance = 0.1)
})

test_that("calculate_rsd: batches with no QC samples yield placeholder (QC-H6)", {
  batch_data <- tibble(
    sample_name = c("S1", "S2"),
    sample_type = c("sample", "sample"),
    feature_A = c(100, 200)
  )

  master_list <- list(
    filters = list(failed_samples = character())
  )

  result <- calculate_rsd(master_list, "peakArea", list(batch1 = batch_data))
  # QC-H6: emits a labelled NA placeholder row rather than silently producing 0.
  # QC-H8: the placeholder carries the batch name it stands in for, not a
  # hard-coded "allBatches" (which collided with the real allBatches row).
  expect_equal(nrow(result), 1)
  expect_equal(result$V1, "peakArea")
  expect_equal(result$V2, "batch1")
})

test_that("calculate_rsd: excludes failed samples (QC-C3 requires >= 3 non-NA)", {
  # QC-C3: RSD now requires >= 3 non-NA QC points per metabolite. Use
  # four QCs, flag one as failed, and expect the remaining three to
  # produce a valid (zero) RSD.
  batch_data <- tibble(
    sample_name = c("QC1", "QC2", "QC3", "QC_bad"),
    sample_type = c("qc", "qc", "qc", "qc"),
    feature_A   = c(100, 100, 100, 999999)
  )

  master_list <- list(
    filters = list(failed_samples = "QC_bad")
  )

  result <- calculate_rsd(master_list, "peakArea", list(batch1 = batch_data))
  # With QC_bad excluded, remaining values are c(100, 100, 100) -> RSD = 0
  expect_equal(result$feature_A, 0)
})

test_that("calculate_rsd: NULL or empty batches yield placeholder (QC-H6)", {
  master_list <- list(
    filters = list(failed_samples = character())
  )

  result <- calculate_rsd(master_list, "peakArea", list(
    batch1 = NULL,
    batch2 = tibble()
  ))

  # QC-H6/H8: one labelled NA placeholder row per supplied batch name.
  expect_equal(nrow(result), 2)
  expect_equal(result$V1, c("peakArea", "peakArea"))
  expect_equal(result$V2, c("batch1", "batch2"))
})

test_that("calculate_rsd: unnamed batch list still falls back to allBatches (QC-H8)", {
  master_list <- list(filters = list(failed_samples = character()))

  result <- calculate_rsd(master_list, "peakArea", list())
  expect_equal(nrow(result), 1)
  expect_equal(result$V2, "allBatches")
})

test_that("calculate_rsd: placeholder is batch-labelled when all QCs failed (QC-H8)", {
  # Reproduces the v3_serum condition: every QC injection on the plate is in
  # failed_samples, so no QC rows survive and the batch yields a placeholder.
  batch_data <- tibble(
    sample_name = c("QC1", "QC2", "QC3", "S1"),
    sample_class = c("qc", "qc", "qc", "sample"),
    sample_type = c("qc", "qc", "qc", "sample"),
    feature_A = c(100, 110, 105, 200)
  )

  master_list <- list(
    filters = list(failed_samples = c("QC1", "QC2", "QC3"))
  )

  expect_warning(
    result <- calculate_rsd(master_list, "peakArea", list(P1 = batch_data)),
    "no QC injections remain"
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$V2, "P1")
  expect_false("feature_A" %in% names(result))
})

test_that("calculate_rsd: per-batch and allBatches calls cannot collide (QC-H8)", {
  # The exact shape qcCheckR_RSD_filter() produces: one call over the plate
  # list, one over an explicit list(allBatches = ...). Both used to return
  # "allBatches" when no QCs survived, producing duplicate keys.
  batch_data <- tibble(
    sample_name = c("QC1", "S1"),
    sample_class = c("qc", "sample"),
    sample_type = c("qc", "sample"),
    feature_A = c(100, 200)
  )
  master_list <- list(filters = list(failed_samples = "QC1"))

  per_plate <- suppressWarnings(
    calculate_rsd(master_list, "peakArea", list(P1 = batch_data))
  )
  all_batches <- suppressWarnings(
    calculate_rsd(master_list, "peakArea", list(allBatches = batch_data))
  )

  keys <- paste0(c(per_plate$V1, all_batches$V1), ".",
                 c(per_plate$V2, all_batches$V2))
  expect_equal(anyDuplicated(keys), 0L)
})

# --- QC-H8: dedupe_rsd_keys ---
test_that("dedupe_rsd_keys: collapses duplicate keys, keeping the richest row", {
  rsd <- tibble(
    dataSource = c("peakArea", "peakArea"),
    dataBatch = c("allBatches", "allBatches"),
    feature_A = c(NA_real_, 12.5)
  )

  expect_warning(out <- dedupe_rsd_keys(rsd), "duplicate RSD key")
  expect_equal(nrow(out), 1)
  expect_equal(out$feature_A, 12.5)
})

test_that("dedupe_rsd_keys: leaves unique keys untouched", {
  rsd <- tibble(
    dataSource = c("peakArea", "peakArea"),
    dataBatch = c("P1", "allBatches"),
    feature_A = c(1, 2)
  )

  expect_silent(out <- dedupe_rsd_keys(rsd))
  expect_identical(out, rsd)
})

# --- QC-H8: format_rsd_table ---
test_that("format_rsd_table: metabolites become rows, keys become columns", {
  master_list <- list(filters = list(rsd = tibble(
    dataSource = c("peakArea", "peakArea"),
    dataBatch = c("P1", "allBatches"),
    feature_A = c(12.345, 15.678),
    feature_B = c(1.111, 2.222)
  )))

  out <- format_rsd_table(master_list)
  expect_equal(names(out), c("data", "peakArea.P1", "peakArea.allBatches"))
  expect_equal(out$data, c("feature_A", "feature_B"))
  expect_equal(out$peakArea.P1, c(12.35, 1.11))
  expect_equal(out$peakArea.allBatches, c(15.68, 2.22))
})

test_that("format_rsd_table: duplicate keys no longer abort the export (QC-H8)", {
  # The v3_serum crash: dplyr::filter() rejected the duplicate-named frame with
  # "Can't transform a data frame with duplicate names".
  master_list <- list(filters = list(rsd = tibble(
    dataSource = c("peakArea", "peakArea"),
    dataBatch = c("allBatches", "allBatches"),
    feature_A = c(10, 20)
  )))

  out <- format_rsd_table(master_list)
  expect_equal(nrow(out), 1)
  expect_equal(anyDuplicated(names(out)), 0L)
})

test_that("format_rsd_table: placeholder-only table returns an empty tibble", {
  master_list <- list(filters = list(rsd = tibble(
    dataSource = c("peakArea", "concentration"),
    dataBatch = c("P1", "allBatches")
  )))

  out <- format_rsd_table(master_list)
  expect_equal(nrow(out), 0)
  expect_equal(ncol(out), 0)
})

test_that("format_rsd_table: NULL / empty RSD table returns an empty tibble", {
  expect_equal(nrow(format_rsd_table(list(filters = list(rsd = NULL)))), 0)
  expect_equal(nrow(format_rsd_table(list(filters = list(rsd = tibble())))), 0)
})

test_that("calculate_rsd: excludes SIL columns from RSD computation", {
  batch_data <- tibble(
    sample_name = c("QC1", "QC2", "QC3"),
    sample_type = c("qc", "qc", "qc"),
    feature_A = c(10, 10, 10),
    SIL_feature = c(100, 200, 150)
  )

  master_list <- list(
    filters = list(failed_samples = character())
  )

  result <- calculate_rsd(master_list, "peakArea", list(batch1 = batch_data))
  expect_false("SIL_feature" %in% names(result))
  expect_true("feature_A" %in% names(result))
})

# --- QC-C3: RSD requires >= 3 non-NA QC points ---
test_that("calculate_rsd: returns NA when fewer than 3 non-NA QC points (QC-C3)", {
  # Only 2 QCs with non-NA values for feature_sparse -> must be NA_real_.
  batch_data <- tibble(
    sample_name = c("QC1", "QC2", "QC3"),
    sample_type = c("qc", "qc", "qc"),
    feature_sparse = c(100, 110, NA),
    feature_dense  = c(100, 110, 105)
  )

  master_list <- list(
    filters = list(failed_samples = character())
  )

  result <- calculate_rsd(master_list, "peakArea", list(batch1 = batch_data))
  expect_true(is.na(result$feature_sparse))
  expect_false(is.na(result$feature_dense))
})

# --- QC-C1: calculate_rsd must honour sample_class over sample_type ---
test_that("calculate_rsd: uses sample_class 'qc' split when present (QC-C1)", {
  # 'other' rows (blanks / SIL / non-chosen QCs) must NOT be treated as QCs
  # even if their legacy sample_type is "qc".
  batch_data <- tibble(
    sample_name = c("QC1", "QC2", "QC3", "BLANK", "SAM1"),
    sample_type  = c("qc", "qc", "qc", "qc", "sample"),
    sample_class = c("qc", "qc", "qc", "other", "sample"),
    feature_A = c(100, 100, 100, 1e9, 500)  # blank value would ruin RSD
  )
  master_list <- list(filters = list(failed_samples = character()))
  result <- calculate_rsd(master_list, "peakArea", list(batch1 = batch_data))
  expect_equal(result$feature_A, 0)
})

# --- QC-H6: empty statTargetProcessed returns labelled NA row ---
test_that("calculate_rsd: empty input returns labelled placeholder (QC-H6)", {
  master_list <- list(filters = list(failed_samples = character()))
  # Empty list (statTargetProcessed never populated).
  res_empty <- calculate_rsd(master_list, "concentration[statTarget]", list())
  expect_true("V1" %in% names(res_empty))
  expect_equal(res_empty$V1, "concentration[statTarget]")
  expect_equal(res_empty$V2, "allBatches")
  # List of NULL/empty batches must still return placeholders, not zeros.
  # QC-H8: one row per supplied batch name so the rows cannot collide with the
  # allBatches row qcCheckR_RSD_filter() requests separately.
  res_null <- calculate_rsd(
    master_list, "concentration[statTarget]",
    list(batch1 = NULL, batch2 = tibble())
  )
  expect_equal(res_null$V1, rep("concentration[statTarget]", 2))
  expect_equal(res_null$V2, c("batch1", "batch2"))
})

# --- qcCheckR_RSD_filter ---
test_that("qcCheckR_RSD_filter: processes multiple data sources and renames columns", {
  qc_data <- tibble(
    sample_name = c("QC1", "QC2", "QC3"),
    sample_type = c("qc", "qc", "qc"),
    feature_A = c(100, 110, 105)
  )

  master_list <- list(
    filters = list(failed_samples = character()),
    project_details = list(
      script_log = list(timestamps = list(
        start_time = Sys.time(), data_preparation = Sys.time()))
    ),
    data = list(
      peakArea = list(
        sorted = list(batch1 = qc_data),
        imputed = list(batch1 = qc_data)
      ),
      concentration = list(
        imputed = list(batch1 = qc_data),
        statTargetProcessed = list(batch1 = qc_data)
      )
    )
  )

  result <- qcCheckR_RSD_filter(master_list)

  expect_true("dataSource" %in% names(result$filters$rsd))
  expect_true("dataBatch" %in% names(result$filters$rsd))
  # Should have rows from multiple sources: peakArea(per-batch + allBatches),
  # concentration(per-batch + allBatches), concentration[statTarget](per-batch + allBatches)
  expect_true(nrow(result$filters$rsd) > 0)
  # QC-H8: the per-batch and allBatches rows must stay distinguishable.
  keys <- paste0(result$filters$rsd$dataSource, ".", result$filters$rsd$dataBatch)
  expect_equal(anyDuplicated(keys), 0L)
})


# ============================================================================
# StatTarget batch correction pipeline ----
# ============================================================================

# --- initialise_statTarget_environment ---
test_that("initialise_statTarget_environment: sets up FUNC_list with correct structure", {
  conc_data <- tibble(
    sample_name = c("QC1", "S1", "S2"),
    sample_type_factor = c("pqc", "sample", "sample"),
    sample_plate_id = c("plate1", "plate1", "plate1"),
    sample_run_index = 1:3,
    feature_A = c(100, 200, 300),
    SIL_B = c(50, 60, 70)
  )

  master_list <- list(
    project_details = list(
      project_dir = "/tmp/test_project",
      qc_type = "pqc"
    ),
    data = list(
      concentration = list(
        imputed = list(plate1 = conc_data)
      )
    )
  )

  stub(initialise_statTarget_environment, "check_dir_exists", TRUE)
  stub(initialise_statTarget_environment, "create_dir", NULL)
  stub(initialise_statTarget_environment, "flag_failed_qc_injections", function(x) x)

  result <- initialise_statTarget_environment(master_list)

  expect_true("project_dir" %in% names(result))
  expect_true("master_data" %in% names(result))
  expect_true("metabolite_list" %in% names(result))
  expect_true("sample_type" %in% names(result$master_data))
  # QC should be mapped correctly
  expect_equal(result$master_data$sample_type[1], "qc")
  expect_equal(result$master_data$sample_type[2], "sample")
})

test_that("initialise_statTarget_environment: creates directories when missing", {
  conc_data <- tibble(
    sample_name = "QC1",
    sample_type_factor = "pqc",
    sample_plate_id = "plate1",
    sample_run_index = 1,
    feature_A = 100
  )

  master_list <- list(
    project_details = list(
      project_dir = "/tmp/test_project",
      qc_type = "pqc"
    ),
    data = list(
      concentration = list(
        imputed = list(plate1 = conc_data)
      )
    )
  )

  check_mock <- mock(FALSE, FALSE, FALSE, FALSE, FALSE, cycle = TRUE)
  create_mock <- mock(TRUE, cycle = TRUE)

  stub(initialise_statTarget_environment, "check_dir_exists", check_mock)
  stub(initialise_statTarget_environment, "create_dir", create_mock)
  stub(initialise_statTarget_environment, "flag_failed_qc_injections", function(x) x)

  result <- initialise_statTarget_environment(master_list)

  # create_dir should be called for each missing directory
  expect_called(create_mock, 5)
})

# --- prepare_statTarget_files ---
test_that("prepare_statTarget_files: calls pheno and profile creation", {
  FUNC_list <- list(some_data = "test")

  pheno_mock <- mock(list(some_data = "test", PhenoFile = "created"))
  profile_mock <- mock(list(some_data = "test", PhenoFile = "created", ProfileFile = "created"))

  stub(prepare_statTarget_files, "create_pheno_file", pheno_mock)
  stub(prepare_statTarget_files, "create_profile_file", profile_mock)

  result <- prepare_statTarget_files(FUNC_list)

  expect_called(pheno_mock, 1)
  expect_called(profile_mock, 1)
})

# --- create_pheno_file ---
test_that("create_pheno_file: creates pheno with QC first and last in each batch", {
  FUNC_list <- list(
    project_dir = withr::local_tempdir(),
    master_data = tibble(
      sample_name = c("S1", "QC1", "S2", "S3", "QC2"),
      sample_plate_id = rep("plate1", 5),
      sample_type = c("sample", "qc", "sample", "sample", "qc"),
      sample_run_index = 1:5
    )
  )

  # Stub write_csv to avoid actual file I/O
  stub(create_pheno_file, "readr::write_csv", NULL)
  stub(create_pheno_file, "dir.create", TRUE)

  result <- create_pheno_file(FUNC_list)

  expect_true("PhenoFile" %in% names(result))
  expect_true("template_qc_order" %in% names(result$PhenoFile))

  pheno_order <- result$PhenoFile$template_qc_order
  plate1_data <- pheno_order %>% filter(batch == "plate1")

  # First row should be QC
  expect_equal(plate1_data$class[1], "qc")
  # Last row should be QC
  expect_equal(plate1_data$class[nrow(plate1_data)], "qc")
})

test_that("create_pheno_file: assigns sequential sample IDs grouped by class", {
  FUNC_list <- list(
    project_dir = withr::local_tempdir(),
    master_data = tibble(
      sample_name = c("QC1", "S1", "S2", "QC2"),
      sample_plate_id = rep("plate1", 4),
      sample_type = c("qc", "sample", "sample", "qc"),
      sample_run_index = 1:4
    )
  )

  stub(create_pheno_file, "readr::write_csv", NULL)
  stub(create_pheno_file, "dir.create", TRUE)

  result <- create_pheno_file(FUNC_list)

  pheno <- result$PhenoFile$template_qc_order
  qc_samples <- pheno %>% filter(class == "qc") %>% pull(sample)
  sample_samples <- pheno %>% filter(class == "sample") %>% pull(sample)

  expect_true(all(grepl("^QC", qc_samples)))
  expect_true(all(grepl("^sample", sample_samples)))
})

test_that("create_pheno_file: sets QC class to NA in template_sample_id", {
  FUNC_list <- list(
    project_dir = withr::local_tempdir(),
    master_data = tibble(
      sample_name = c("QC1", "S1", "QC2"),
      sample_plate_id = rep("plate1", 3),
      sample_type = c("qc", "sample", "qc"),
      sample_run_index = 1:3
    )
  )

  stub(create_pheno_file, "readr::write_csv", NULL)
  stub(create_pheno_file, "dir.create", TRUE)

  result <- create_pheno_file(FUNC_list)

  template <- result$PhenoFile$template_sample_id
  qc_classes <- template$class[template$sample %in% c("QC1", "QC2")]
  expect_true(all(is.na(qc_classes)))
})

# --- create_profile_file ---
test_that("create_profile_file: stops when metabolite_list is empty", {
  FUNC_list <- list(
    metabolite_list = character(0),
    master_data = tibble(sample_name = "S1"),
    PhenoFile = list(template_sample_id = tibble(sample = "sample1", sample_name = "S1")),
    project_dir = withr::local_tempdir()
  )

  expect_error(
    create_profile_file(FUNC_list),
    "metabolite_list cannot be empty"
  )
})

test_that("create_profile_file: creates ProfileFile with metabolite map", {
  FUNC_list <- list(
    project_dir = withr::local_tempdir(),
    metabolite_list = c("feature_A", "feature_B"),
    master_data = tibble(
      sample_name = c("QC1", "S1"),
      sample_type = c("qc", "sample"),
      feature_A = c(100, 200),
      feature_B = c(300, 400)
    ),
    PhenoFile = list(
      template_sample_id = tibble(
        sample = c("QC1", "sample1"),
        sample_name = c("QC1", "S1"),
        batch = c(1, 1),
        class = c(NA, "sample"),
        order = 1:2
      )
    )
  )

  stub(create_profile_file, "readr::write_csv", NULL)
  stub(create_profile_file, "dir.create", TRUE)

  result <- create_profile_file(FUNC_list)

  expect_true("ProfileFile" %in% names(result))
  expect_true("metabolite_list" %in% names(result$ProfileFile))
  expect_true("name" %in% names(result$ProfileFile$ProfileFile))
})


# ============================================================================
# Export functions ----
# ============================================================================

# --- export_xlsx_file ---
test_that("export_xlsx_file: calls write.xlsx with correct output path", {
  master_list <- list(
    project_details = list(
      project_dir = "/tmp/test_project",
      user_name = "testuser",
      project_name = "testproj"
    ),
    summary_tables = list(
      projectOverview = tibble(metric = "test"),
      odsAreaOverview = NULL
    ),
    filters = list(
      samples.missingValues = tibble(sample = "S1"),
      lipid.missingValues = list(allPlates = list(tibble(lipid = "L1"))),
      rsd = tibble(dataSource = "peakArea", dataBatch = "batch1", feat = 10),
      failed_lipids = character()
    ),
    data = list(
      peakArea = list(sorted = list(plate1 = tibble(
        sample_name = "S1", sample_type_factor = "sample"
      ))),
      concentration = list(
        sorted = list(),
        imputed = list(),
        statTargetProcessed = list()
      )
    )
  )

  write_mock <- mock(NULL)
  stub(export_xlsx_file, "openxlsx::write.xlsx", write_mock)
  stub(export_xlsx_file, "create_user_guide", tibble(key = "test", value = "val"))
  stub(export_xlsx_file, "format_rsd_table", tibble(data = "test"))
  stub(export_xlsx_file, "filter_concentration", tibble(sample_name = "S1"))

  result <- suppressWarnings(export_xlsx_file(master_list))

  expect_called(write_mock, 1)
  # Verify the file argument contains expected path components
  call_args <- mock_args(write_mock)[[1]]
  expect_true(grepl("qcCheckeR\\.xlsx$", call_args$file))
  expect_true(grepl("testuser", call_args$file))
})

# --- export_master_list_qs ---
test_that("export_master_list_qs: creates output directory and saves synchronously", {
  master_list <- list(
    project_details = list(
      project_dir = "/tmp/test_project",
      project_name = "testproj",
      script_log = list(timestamps = list(plot_generation = Sys.time()))
    )
  )

  dir_create_mock <- mock(TRUE)
  qs_save_mock <- mock(invisible(NULL))

  stub(export_master_list_qs, "dir.create", dir_create_mock)
  stub(export_master_list_qs, "qs2::qs_save", qs_save_mock)
  stub(export_master_list_qs, "update_script_log", function(ml, ...) ml)

  result <- suppressMessages(export_master_list_qs(master_list))

  expect_called(dir_create_mock, 1)
  expect_called(qs_save_mock, 1)
  # Path should land under all/data/qs2 with .qs2 extension.
  call_args <- mock_args(qs_save_mock)[[1]]
  expect_true(grepl("all/data/qs2/", call_args$file, fixed = TRUE) ||
                grepl("all\\\\data\\\\qs2\\\\", call_args$file))
  expect_true(grepl("_qcCheckR\\.qs2$", call_args$file))
})

test_that("export_master_list_qs: forwards qs_nthreads and qs_compress_level to qs2::qs_save", {
  master_list <- list(
    project_details = list(
      project_dir = "/tmp/test_project",
      project_name = "testproj",
      script_log = list(timestamps = list(plot_generation = Sys.time()))
    )
  )

  qs_save_mock <- mock(invisible(NULL))
  stub(export_master_list_qs, "dir.create", TRUE)
  stub(export_master_list_qs, "qs2::qs_save", qs_save_mock)
  stub(export_master_list_qs, "update_script_log", function(ml, ...) ml)

  suppressMessages(
    export_master_list_qs(master_list,
                          qs_nthreads = 4L,
                          qs_compress_level = 7L)
  )
  call_args <- mock_args(qs_save_mock)[[1]]
  expect_equal(call_args$nthreads, 4L)
  expect_equal(call_args$compress_level, 7L)
})

test_that("export_master_list_qs: updates script log with completion message", {
  master_list <- list(
    project_details = list(
      project_dir = "/tmp/test_project",
      project_name = "testproj",
      script_log = list(timestamps = list(plot_generation = Sys.time()))
    )
  )

  stub(export_master_list_qs, "dir.create", TRUE)
  stub(export_master_list_qs, "qs2::qs_save", function(...) invisible(NULL))

  log_mock <- mock(master_list)
  stub(export_master_list_qs, "update_script_log", log_mock)

  result <- suppressWarnings(export_master_list_qs(master_list))

  expect_called(log_mock, 1)
  log_call_args <- mock_args(log_mock)[[1]]
  expect_equal(log_call_args[[2]], "data_exports")
})


# ============================================================================
# Summary / reporting ----
# ============================================================================

# --- format_rsd_table ---
test_that("format_rsd_table: transposes and formats RSD data", {
  rsd_data <- tibble(
    dataSource = c("peakArea", "concentration"),
    dataBatch = c("plate1", "plate1"),
    feature_A = c(12.345, 25.678),
    feature_B = c(5.111, 8.222)
  )

  master_list <- list(
    filters = list(rsd = rsd_data)
  )

  result <- suppressWarnings(format_rsd_table(master_list))

  # Should be transposed: features as rows, data sources as columns
  expect_true("data" %in% names(result))
  # feature_A and feature_B should appear as row values in the 'data' column
  expect_true(nrow(result) > 0)
})

test_that("format_rsd_table: rounds values to 2 decimal places", {
  rsd_data <- tibble(
    dataSource = c("peakArea"),
    dataBatch = c("plate1"),
    feature_A = c(12.3456789)
  )

  master_list <- list(
    filters = list(rsd = rsd_data)
  )

  result <- suppressWarnings(format_rsd_table(master_list))

  # Extract the numeric column(s) - the source.batch column
  numeric_cols <- result %>% select(where(is.numeric))
  if (ncol(numeric_cols) > 0) {
    # Check that values are rounded to 2 decimal places
    vals <- unlist(numeric_cols)
    expect_true(all(round(vals, 2) == vals, na.rm = TRUE))
  }
})

test_that("format_rsd_table: creates combined data column from source and batch", {
  rsd_data <- tibble(
    dataSource = c("peakArea"),
    dataBatch = c("batch1"),
    feature_X = c(15.5)
  )

  master_list <- list(
    filters = list(rsd = rsd_data)
  )

  result <- suppressWarnings(format_rsd_table(master_list))

  expect_true("data" %in% names(result))
})

# --- generate_plate_summary ---
test_that("generate_plate_summary: returns tibble with expected metric count", {
  sorted_data <- tibble(
    sample_name = c("S1", "QC1", "S2"),
    sample_type_factor = factor(c("sample", "qc", "sample"),
                                 levels = c("sample", "qc")),
    sample_matrix = c("plasma", "plasma", "plasma"),
    lipid_A = c(100, 110, 105),
    lipid_B = c(200, 210, 205),
    SIL_X = c(50, 55, 52)
  )

  rsd_row <- tibble(
    dataSource = c("peakArea", "concentration", "concentration[statTarget]"),
    dataBatch = rep("plate1", 3),
    lipid_A = c(5, 8, 12),
    lipid_B = c(15, 22, 35)
  )

  master_list <- list(
    data = list(
      peakArea = list(sorted = list(plate1 = sorted_data))
    ),
    filters = list(
      rsd = rsd_row,
      failed_lipids = character(),
      samples.missingValues = tibble(
        sample_plate_id = c("plate1", "plate1"),
        sample.flag = c(0, 1)
      ),
      sil.intStd.missingValues = list(
        plate1 = tibble(flag_SIL_intStd_Plate = c(0, 1))
      ),
      lipid.missingValues = list(
        plate1 = tibble(flag.Lipid.Plate = c(0, 0, 1))
      )
    ),
    templates = list(`Plate SIL version` = list(plate1 = "v1"))
  )

  metrics <- c(
    "MatrixType", "totalSamples", "studySamples", "qcSamples",
    "lipidTargets", "matchedLipidTargets", "SIL.version", "SIL.IntStds",
    "missingValueFilterFlags[samples]",
    "missingValueFilterFlags[SIL.IS]",
    "missingValueFilterFlags[lipidTargets]",
    "rsd<30%[peakArea]", "rsd<20%[peakArea]", "rsd<10%[peakArea]",
    "rsd<30%[concentration]", "rsd<20%[concentration]", "rsd<10%[concentration]",
    "rsd<30%[concentration.statTarget]", "rsd<20%[concentration.statTarget]",
    "rsd<10%[concentration.statTarget]"
  )
  sample_tags <- "qc"

  result <- generate_plate_summary(master_list, "plate1", metrics, sample_tags)

  expect_s3_class(result, "tbl_df")
  expect_true("metric" %in% names(result))
  expect_true("plate1" %in% names(result))
  expect_equal(nrow(result), length(metrics))
})

# --- notify_qc_type ---
test_that("notify_qc_type: emits message with qc type", {
  expect_message(
    notify_qc_type("pqc"),
    "qcCheckeR has set filtering QC to: pqc"
  )
})

test_that("notify_qc_type: works with different QC labels", {
  expect_message(notify_qc_type("ltr"), "ltr")
  expect_message(notify_qc_type("bqc"), "bqc")
})


# ============================================================================
# Flag failed QC injections ----
# ============================================================================

test_that("flag_failed_qc_injections: reclassifies low-signal QCs as sample", {
  FUNC_list <- list(
    master_data = tibble(
      sample_name = c("QC1", "QC2", "QC3", "S1"),
      sample_type = c("qc", "qc", "qc", "sample"),
      sample_plate_id = rep("plate1", 4),
      feature_A = c(1, 1000, 1100, 500),
      feature_B = c(1, 900, 950, 600)
    ),
    metabolite_list = c("feature_A", "feature_B")
  )

  result <- flag_failed_qc_injections(FUNC_list)

  # QC1 has very low signal relative to median, should be reclassified
  expect_equal(result$master_data$sample_type[1], "sample")
  # QC2 and QC3 should remain QC
  expect_equal(result$master_data$sample_type[2], "qc")
  expect_equal(result$master_data$sample_type[3], "qc")
})

test_that("flag_failed_qc_injections: does not flag QCs with normal signal", {
  FUNC_list <- list(
    master_data = tibble(
      sample_name = c("QC1", "QC2", "QC3"),
      sample_type = c("qc", "qc", "qc"),
      sample_plate_id = rep("plate1", 3),
      feature_A = c(1000, 1050, 980),
      feature_B = c(500, 520, 490)
    ),
    metabolite_list = c("feature_A", "feature_B")
  )

  result <- flag_failed_qc_injections(FUNC_list)

  expect_true(all(result$master_data$sample_type == "qc"))
})
