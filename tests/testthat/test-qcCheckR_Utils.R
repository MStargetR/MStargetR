library(mockery)
library(dplyr)
library(tidyr)

#Tests for setup_project function ----
test_that("qcCheckR_setup_project returns a master_list with expected structure", {
  # Create dummy inputs
  user_name <- "test_user"
  project_directory <- "dummy/path"
  mrm_template_list <- list(list(guide = "dummy"))
  QC_sample_label <- "QC"
  sample_tags <- c("SampleA", "SampleB")
  mv_threshold <- 50

  # Create dummy master_list
  dummy_master_list <- list(initialised = TRUE)

  # Mock all dependencies
  stub(qcCheckR_setup_project, "validate_project_directory", NULL)
  stub(qcCheckR_setup_project, "initialise_master_list", dummy_master_list)
  stub(qcCheckR_setup_project, "store_environment_details", function(x) x)
  stub(qcCheckR_setup_project, "qcCheckR_set_project_details", function(x, ...) x)
  stub(qcCheckR_setup_project, "qcCheckR_read_mrm_guides", function(x, ...) x)
  stub(qcCheckR_setup_project, "qcCheckR_setup_project_directories", function(x) NULL)
  stub(qcCheckR_setup_project, "qcCheckR_import_PeakForgeR_reports", function(x) x)
  stub(qcCheckR_setup_project, "find_method_version", function(x) x)
  stub(qcCheckR_setup_project, "update_script_log", function(x, ...) x)

  # Run the function
  result <- qcCheckR_setup_project(user_name, project_directory, mrm_template_list,
                                   QC_sample_label, sample_tags, mv_threshold)

  # Expectations
  expect_type(result, "list")
  expect_true(result$initialised)
})

test_that("qcCheckR_set_project_details correctly updates master_list", {
  # Dummy master_list structure
  master_list <- list(project_details = list(script_log = list(timestamps = list())))

  # Inputs
  user_name <- "test_user"
  project_directory <- "some/path/projectX"
  QC_sample_label <- "QC"
  sample_tags <- c("sample1", "sample2")
  mv_threshold <- 40

  # Run function
  result <- qcCheckR_set_project_details(master_list, user_name, project_directory,
                                         QC_sample_label, sample_tags, mv_threshold)

  # Expectations
  expect_equal(result$project_details$project_dir, project_directory)
  expect_equal(result$project_details$user_name, user_name)
  expect_equal(result$project_details$project_name, "projectX")
  expect_equal(result$project_details$qc_type, QC_sample_label)
  expect_equal(result$project_details$mv_sample_threshold, mv_threshold)
  expect_equal(result$project_details$sample_tags, sample_tags)
  expect_type(result$project_details$script_log$timestamps$start_time, "double") # Sys.time returns POSIXct (double)
})

test_that("qcCheckR_set_project_details sets default sample_tags for ANPC", {
  master_list <- list(project_details = list(user_name = "ANPC", script_log = list(timestamps = list())))

  result <- qcCheckR_set_project_details(master_list, "ANPC", "dir", "QC", NULL, 50)

  expect_equal(result$project_details$sample_tags,
               c("pqc", "qc", "vltr", "sltr", "ltr", "blank", "istds", "cond", "sample"))
})


test_that("qcCheckR_read_mrm_guides calls validation and loads user-supplied guides", {
  master_list <- list(project_details = list(user_name = "test_user"))

  mrm_template_list <- list(
    v1 = list(SIL_guide = "path/to/sil.tsv", conc_guide = "path/to/conc.tsv")
  )

  # Mock read_tsv to return dummy tibble
  stub(qcCheckR_read_mrm_guides, "readr::read_tsv", function(path, ...) tibble::tibble(file = path))

  # Mock replace_precursor_symbols to return input unchanged
  stub(qcCheckR_read_mrm_guides, "replace_precursor_symbols", function(guide, ...) guide)

  # Mock validation function
  validation_mock <- mockery::mock(master_list)
  stub(qcCheckR_read_mrm_guides, "validate_qcCheckR_mrm_template_list", validation_mock)

  result <- qcCheckR_read_mrm_guides(master_list, mrm_template_list)

  # Check guides loaded
  expect_equal(result$templates$mrm_guides$v1$SIL_guide$file, "path/to/sil.tsv")
  expect_equal(result$templates$mrm_guides$v1$conc_guide$file, "path/to/conc.tsv")

  # Check validation was called
  mockery::expect_called(validation_mock, 1)
})

test_that("qcCheckR_setup_project_directories does nothing if directories already exist", {
  master_list <- list(project_details = list(project_dir = "dummy/project"))

  # Mock dir.exists to always return TRUE
  stub(qcCheckR_setup_project_directories, "dir.exists", function(...) TRUE)
  stub(qcCheckR_setup_project_directories, "dir.create", function(...) stop("Should not be called"))

  expect_message(qcCheckR_setup_project_directories(master_list),
                 "qcCheckR directory set up at: dummy/project/all")
})

test_that("qcCheckR_setup_project_directories creates directories if they do not exist", {
  master_list <- list(project_details = list(project_dir = "dummy/project"))

  # Simulate dir.exists returning FALSE initially, then TRUE after creation
  exists_calls <- c(FALSE, FALSE, FALSE, FALSE, TRUE)
  exists_mock <- mockery::mock(FALSE, FALSE, FALSE, FALSE, TRUE)
  create_mock <- mockery::mock(TRUE, cycle = TRUE)

  stub(qcCheckR_setup_project_directories, "dir.exists", exists_mock)
  stub(qcCheckR_setup_project_directories, "dir.create", create_mock)

  expect_message(qcCheckR_setup_project_directories(master_list),
                 "qcCheckR directory set up at: dummy/project/all")

  # Check that dir.create was called 4 times (main + 3 subdirs)
  mockery::expect_called(create_mock, 4)
})

test_that("qcCheckR_setup_project_directories throws error if directory creation fails", {
  master_list <- list(project_details = list(project_dir = "dummy/project"))

  # Simulate dir.exists always returning FALSE
  stub(qcCheckR_setup_project_directories, "dir.exists", function(...) FALSE)
  stub(qcCheckR_setup_project_directories, "dir.create", function(...) TRUE)

  expect_error(qcCheckR_setup_project_directories(master_list),
               "Failed to create qcCheckR directory at: dummy/project/all")
})

test_that("qcCheckR_import_PeakForgeR_reports stops if no files found", {
  master_list <- list(project_details = list(project_dir = "dummy/project"))
  stub(qcCheckR_import_PeakForgeR_reports, "list.files", function(...) character(0))

  expect_error(qcCheckR_import_PeakForgeR_reports(master_list),
               "No report files found in the specified project directory")
})

test_that("qcCheckR_import_PeakForgeR_reports reads and stores CSV and TSV files", {
  master_list <- list(
    project_details = list(project_dir = "dummy/project", plateIDs = character()),
    data = list(PeakForgeRReport = list())
  )

  files <- c("dummy/project/plate1_PeakForgeR_1_report.csv", "dummy/project/plate2_PeakForgeR_1_report.tsv")
  stub(qcCheckR_import_PeakForgeR_reports, "list.files", function(...) files)
  stub(qcCheckR_import_PeakForgeR_reports, "readr::read_csv", function(...) data.frame(FileName = "sample1.mzML", MoleculeName = "Mol1", Area = 100, Height = 100))
  stub(qcCheckR_import_PeakForgeR_reports, "readr::read_tsv", function(...) data.frame(FileName = "sample2.mzML", MoleculeName = "Mol2", Area = 200, Height = 200))

  result <- qcCheckR_import_PeakForgeR_reports(master_list)

  expect_true(length(result$data$PeakForgeRReport) == 2)
  expect_true(length(result$project_details$plateIDs) == 2)
})

test_that("qcCheckR_import_PeakForgeR_reports reads and stores multiple files", {
  master_list <- list(
    project_details = list(project_dir = "dummy/project", plateIDs = character()),
    data = list(PeakForgeRReport = list())
  )

  files <- c("dummy/project/plate1_PeakForgeR_1_report.csv", "dummy/project/plate2_PeakForgeR_1_report.tsv")
  stub(qcCheckR_import_PeakForgeR_reports, "list.files", function(...) files)
  stub(qcCheckR_import_PeakForgeR_reports, "readr::read_csv", function(...) data.frame(FileName = "file1.mzML", MoleculeName = "Mol1", Area = 100, Height = 100))
  stub(qcCheckR_import_PeakForgeR_reports, "readr::read_tsv", function(...) data.frame(FileName = "file2.mzML", MoleculeName = "Mol2", Area = 200, Height = 200))

  result <- qcCheckR_import_PeakForgeR_reports(master_list)

  expect_true(length(result$data$PeakForgeRReport) == 2)
  expect_true(length(result$project_details$plateIDs) == 2)
})

test_that("find_method_version assigns correct version when all SILs match", {
  suppressMessages({
  master_list <- list(
    data = list(PeakForgeRReport = list(
      plate1 = data.frame(MoleculeName = c("SIL_A", "SIL_B"),check.names = FALSE)
    )),
    templates = list(mrm_guides = list(
      v1 = list(SIL_guide = data.frame(`Precursor Name` = c("SIL_A", "SIL_B", "Other"), check.names = FALSE))
    )),
    project_details = list(plateIDs = "plate1", plate_method_versions = list())
  )

  result <- find_method_version(master_list)

  expect_equal(result$project_details$plate_method_versions[["plate1"]], "v1")
 })
})

test_that("find_method_version stops when some plates are unmatched", {
  master_list <- list(
    data = list(PeakForgeRReport = list(
      plate1 = data.frame(MoleculeName = c("SIL_A", "SIL_B"), check.names = FALSE),
      plate2 = data.frame(MoleculeName = c("SIL_X"), check.names = FALSE)
    )),
    templates = list(mrm_guides = list(
      v1 = list(SIL_guide = data.frame(`Precursor Name` = c("SIL_A", "SIL_B"), check.names = FALSE))
    )),
    project_details = list(plateIDs = c("plate1", "plate2"), plate_method_versions = list())
  )

  expect_error(find_method_version(master_list), "Could not match SIL guide version for plate")
})

test_that("find_method_version stops when no plate matches any guide", {
  master_list <- list(
    data = list(PeakForgeRReport = list(
      plate1 = data.frame(MoleculeName = c("SIL_X", "SIL_Y"), check.names = FALSE)
    )),
    templates = list(mrm_guides = list(
      v1 = list(SIL_guide = data.frame(`Precursor Name` = c("SIL_A", "SIL_B"), check.names = FALSE))
    )),
    project_details = list(plateIDs = "plate1", plate_method_versions = list())
  )

  expect_error(find_method_version(master_list), "Could not match SIL guide version for plate")
})

#Tests for phase 1 data prep----

##Transpose tests----
test_that("qcCheckR_transpose_data successfully transposes a matching plate", {
  suppressMessages({
  master_list <- list(
    project_details = list(plateIDs = "plate1"),
    data = list(PeakForgeRReport = list(plate1 = data.frame()),
                peakArea = list(transposed = list()))
  )

  stub(qcCheckR_transpose_data, "validate_master_list", function(x) NULL)
  stub(qcCheckR_transpose_data, "find_matching_report", function(x, y) "plate1")
  stub(qcCheckR_transpose_data, "transpose_plate_data", function(x) data.frame(transposed = TRUE))

  result <- qcCheckR_transpose_data(master_list)

  expect_true("plate1" %in% names(result$data$peakArea$transposed))
  expect_equal(result$data$peakArea$transposed[["plate1"]]$transposed, TRUE)
  })
})

test_that("qcCheckR_transpose_data handles no matching report", {
  suppressMessages({
  master_list <- list(
    project_details = list(plateIDs = "plate1"),
    data = list(PeakForgeRReport = list(plate1 = data.frame()),
                peakArea = list(transposed = list()))
  )

  stub(qcCheckR_transpose_data, "validate_master_list", function(x) NULL)
  stub(qcCheckR_transpose_data, "find_matching_report", function(x, y) character(0))

  expect_message(qcCheckR_transpose_data(master_list), "No matching PeakForgeR report found for plate: plate1")
  })
})

test_that("qcCheckR_transpose_data handles multiple matching reports", {
  suppressMessages({
  master_list <- list(
    project_details = list(plateIDs = "plate1"),
    data = list(PeakForgeRReport = list(plate1 = data.frame()),
                peakArea = list(transposed = list()))
  )

  stub(qcCheckR_transpose_data, "validate_master_list", function(x) NULL)
  stub(qcCheckR_transpose_data, "find_matching_report", function(x, y) c("plate1", "plate1_dup"))

  expect_message(qcCheckR_transpose_data(master_list), "Multiple matching PeakForgeR reports found for plate: plate1")
  })
})

test_that("qcCheckR_transpose_data catches error during transposition", {
  suppressMessages({
  master_list <- list(
    project_details = list(plateIDs = "plate1"),
    data = list(PeakForgeRReport = list(plate1 = data.frame()),
                peakArea = list(transposed = list()))
  )

  stub(qcCheckR_transpose_data, "validate_master_list", function(x) NULL)
  stub(qcCheckR_transpose_data, "find_matching_report", function(x, y) "plate1")
  stub(qcCheckR_transpose_data, "transpose_plate_data", function(x) stop("Simulated error"))

  expect_message(qcCheckR_transpose_data(master_list), "Error transposing plate: plate1")
  expect_message(qcCheckR_transpose_data(master_list), "Error message: Simulated error")
  })
})

test_that("transpose_plate_data returns a tibble with correct structure", {
  input <- tibble(
    FileName = c("sample1.mzML", "sample2.mzML"),
    MoleculeName = c("MolA", "MolA"),
    Area = c("100", "200")
  )

  result <- transpose_plate_data(input)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("sample_name", "MolA"))
  expect_equal(nrow(result), 2)
  expect_equal(result$sample_name, c("sample1", "sample2"))
  expect_type(result$MolA, "double")
})

test_that("transpose_plate_data handles multiple molecules correctly", {
  input <- tibble(
    FileName = rep(c("sample1.mzML", "sample2.mzML"), each = 2),
    MoleculeName = rep(c("MolA", "MolB"), times = 2),
    Area = c("100", "300", "200", "400")
  )

  result <- transpose_plate_data(input)

  expect_named(result, c("sample_name", "MolA", "MolB"))
  expect_equal(result$MolA, c(100, 200))
  expect_equal(result$MolB, c(300, 400))
})

test_that("transpose_plate_data removes .mzML extension from sample names", {
  input <- tibble(
    FileName = c("sample1.mzML", "sample2.mzML"),
    MoleculeName = c("MolA", "MolA"),
    Area = c("100", "200")
  )

  result <- transpose_plate_data(input)

  expect_false(any(grepl("\\.mzML$", result$sample_name)))
})

test_that("transpose_plate_data converts Area to numeric", {
  input <- tibble(
    FileName = c("sample1.mzML", "sample2.mzML"),
    MoleculeName = c("MolA", "MolA"),
    Area = c("100", "not_a_number")
  )

  result <- suppressWarnings(transpose_plate_data(input))

  expect_true(is.na(result$MolA[2]))
  expect_type(result$MolA, "double")
})



##Sort data tests----
test_that("qcCheckR_sort_data populates run_orders and sorted data", {
  # Minimal mock master_list
  master_list <- list(
    project_details = list(
      sample_tags = tibble(),
      run_orders = NULL
    ),
    data = list(
      skylineReport = list(plate1 = tibble()),
      peakArea = list(transposed = list(plate1 = tibble(sample_name = "S1")))
    )
  )

  # Mock dependencies
  local_mocked_bindings(
    extract_run_order = function(report, plate_id) tibble(sample_name = "S1"),
    assign_sample_type = function(tags, run_order) run_order,
    validate_qc_types = function(run_order, tags) NULL,
    sort_and_filter_data = function(run_order, data, tags) tibble(sample_name = "S1"),
    extract_sample_id = function(name) "ID1",
    assess_qc_coverage = function(ml) { ml$qc_checked <- TRUE; ml },
    set_project_qc_type = function(ml) { ml$qc_type <- "TypeA"; ml },
    finalise_sorted_data = function(ml) { ml$finalised <- TRUE; ml }
  )

  result <- qcCheckR_sort_data(master_list)

  expect_true("plate1" %in% names(result$project_details$run_orders))
  expect_true("plate1" %in% names(result$data$peakArea$sorted))
  expect_equal(result$data$peakArea$sorted$plate1$sample_ID, "ID1")
  expect_true(result$qc_checked)
  expect_equal(result$qc_type, "TypeA")
  expect_true(result$finalised)
})

test_that("extract_run_order returns expected columns and structure", {
  input <- tibble(
    FileName = c("sample_SER_01.mzML", "sample_PLA_02.mzML"),
    AcquiredTime = c("2023-01-01 10:00:00", "2023-01-01 09:00:00")
  )

  result <- extract_run_order(input, plate_id = "PlateA")

  expect_named(result, c("sample_name", "sample_timestamp", "sample_plate_id",
                         "sample_plate_order", "sample_matrix"))
  expect_equal(result$sample_name, c("sample_PLA_02", "sample_SER_01"))  # Sorted by time
  expect_equal(result$sample_plate_id, rep("PlateA", 2))
  expect_equal(result$sample_plate_order, 1:2)
  expect_equal(result$sample_matrix, c("PLA", "SER"))
})

test_that("extract_run_order filters out missing timestamps", {
  input <- tibble(
    FileName = c("sample1.mzML", "sample2.mzML"),
    AcquiredTime = c("2023-01-01 10:00:00", NA)
  )

  result <- extract_run_order(input, plate_id = "PlateB")

  expect_equal(nrow(result), 1)
  expect_equal(result$sample_name, "sample1")
})

test_that("extract_run_order handles unknown matrix types", {
  input <- tibble(
    FileName = c("sample_XYZ_01.mzML"),
    AcquiredTime = c("2023-01-01 10:00:00")
  )

  result <- extract_run_order(input, plate_id = "PlateC")

  expect_true(is.na(result$sample_matrix))
})

test_that("assign_sample_type assigns correct types based on sample_tags", {
  sample_tags <- c("QC", "BLANK")
  run_order <- tibble(sample_name = c("QC_01", "BLANK_02", "SAMPLE_03"))

  result <- assign_sample_type(sample_tags, run_order)

  expect_equal(result$sample_type, c("QC", "BLANK", "sample"))
})

test_that("assign_sample_type is case-insensitive", {
  sample_tags <- c("qc")
  run_order <- tibble(sample_name = c("QC_01", "qc_02", "Qc_03", "sample_04"))

  result <- assign_sample_type(sample_tags, run_order)

  expect_equal(result$sample_type, c("qc", "qc", "qc", "sample"))
})

test_that("assign_sample_type throws error with NULL or empty sample_tags", {
  run_order <- tibble(sample_name = c("QC_01"))

  expect_error(assign_sample_type(NULL, run_order),
               "sample_tags must be a character vector with at least one QC type.")
  expect_error(assign_sample_type(character(0), run_order),
               "sample_tags must be a character vector with at least one QC type.")
})

test_that("assign_sample_type matches tags delimited by space, dot or hyphen", {
  sample_tags <- c("LTR")
  # Real-world ANPC file naming uses a space before the QC tag, e.g.
  # ROCIT20_..._PLATE_3-PLASMA LTR_19.mzML
  run_order <- tibble(sample_name = c(
    "ROCIT20_C1_URI_MS-LIPIDS_PLIP01_PLATE_3-PLASMA LTR_19",
    "ROCIT20_C1_URI_MS-LIPIDS_PLIP01_PLATE_3-PLASMA-LTR-20",
    "PLATE_3.LTR.21",
    "PLATE_3-ROCIT10001_ROC0.01_12"
  ))
  result <- assign_sample_type(sample_tags, run_order)
  expect_equal(result$sample_type, c("LTR", "LTR", "LTR", "sample"))
})

test_that("assign_sample_type does not match tag inside another word", {
  sample_tags <- c("LTR")
  # 'VLTR' must not be classified as LTR; 'LTRX' must not match either.
  run_order <- tibble(sample_name = c("PLATE_VLTR_01", "PLATE_LTRX_02", "PLATE_LTR_03"))
  result <- assign_sample_type(sample_tags, run_order)
  expect_equal(result$sample_type, c("sample", "sample", "LTR"))
})

test_that("validate_qc_types passes with valid QC types", {
  run_order <- tibble(sample_type = c("QC", "sample", "BLANK"))
  sample_tags <- c("QC", "BLANK")

  expect_no_error(validate_qc_types(run_order, sample_tags))
})

test_that("validate_qc_types throws error for invalid QC type", {
  run_order <- tibble(sample_type = c("QC", "INVALID", "sample"))
  sample_tags <- c("QC")

  expect_error(suppressMessages(validate_qc_types(run_order, sample_tags)),
               "Invalid QC sample_type detected.")
})

test_that("validate_qc_types throws error when no QC types are present", {
  run_order <- tibble(
    sample_type = c("sample", "sample"),
    sample_plate_id = c("PLATE_1", "PLATE_1"),
    sample_name = c("study_001", "study_002")
  )
  sample_tags <- c("QC")

  err <- expect_error(validate_qc_types(run_order, sample_tags),
               "No QC types were identified for plate")
  # New error message must include the plate label, the tags used, and a
  # filename preview so users can diagnose mis-tagged plates from the log.
  expect_match(conditionMessage(err), "PLATE_1")
  expect_match(conditionMessage(err), "QC")
  expect_match(conditionMessage(err), "study_001")
})

test_that("sort_and_filter_data joins and sorts by sample_timestamp", {
  run_order <- tibble(
    sample_name = c("S1", "S2"),
    sample_timestamp = as.POSIXct(c("2023-01-02", "2023-01-01"))
  )
  transposed_data <- tibble(
    sample_name = c("S1", "S2"),
    MolA = c(100, 200)
  )
  sample_tags <- c("QC")

  result <- sort_and_filter_data(run_order, transposed_data, sample_tags)

  expect_equal(result$sample_name, c("S2", "S1"))  # Sorted by timestamp
  expect_equal(result$sample_run_index, 1:2)
})

test_that("sort_and_filter_data handles unmatched samples in transposed_data", {
  run_order <- tibble(
    sample_name = c("S1", "S2"),
    sample_timestamp = as.POSIXct(c("2023-01-01", "2023-01-02"))
  )
  transposed_data <- tibble(
    sample_name = c("S1"),
    MolA = c(100)
  )
  sample_tags <- c("QC")

  result <- sort_and_filter_data(run_order, transposed_data, sample_tags)

  expect_true("MolA" %in% names(result))
  expect_true(is.na(result$MolA[2]))  # S2 has no match
})

test_that("sort_and_filter_data assigns correct sample_run_index", {
  run_order <- tibble(
    sample_name = c("S1", "S2", "S3"),
    sample_timestamp = as.POSIXct(c("2023-01-03", "2023-01-01", "2023-01-02"))
  )
  transposed_data <- tibble(
    sample_name = c("S1", "S2", "S3"),
    MolA = c(100, 200, 300)
  )
  sample_tags <- c("QC")

  result <- sort_and_filter_data(run_order, transposed_data, sample_tags)

  expect_equal(result$sample_run_index, c(1:3))
})

test_that("extract_sample_id removes common tokens and retains unique identifiers", {
  filenames <- c("plate1_SER_001.mzML", "plate1_SER_002.mzML", "plate1_SER_003.mzML")
  result <- extract_sample_id(filenames)
  expect_equal(result, c("001", "002", "003"))
})

test_that("extract_sample_id handles mixed delimiters", {
  filenames <- c("plate1-SER.001.mzML", "plate1-SER.002.mzML")
  result <- extract_sample_id(filenames)
  expect_equal(result, c("001", "002"))
})

test_that("extract_sample_id returns full identifier when no common tokens", {
  filenames <- c("A_1.mzML", "B_2.mzML")
  result <- extract_sample_id(filenames)
  expect_equal(result, c("A_1", "B_2"))
})

test_that("extract_sample_id works with a single filename", {
  filenames <- c("plate1_SER_001.mzML")
  result <- extract_sample_id(filenames)
  expect_equal(unlist(result), "plate1_SER_001.mzML")
})

test_that("assess_qc_coverage marks QC as pass when ratio and count are sufficient", {
  master_list <- list(
    project_details = list(),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_type = c(rep("QC", 10), rep("sample", 94))
          )))))

  result <- assess_qc_coverage(master_list)

  expect_equal(result$project_details$qc_passed$plate1$QC, "pass")
  expect_equal(result$project_details$global_qc_pass$QC, "pass")
})

test_that("assess_qc_coverage marks QC as fail when ratio or count is insufficient", {
  master_list <- list(
    project_details = list(),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_type = c("QC", "sample", "sample")
          )))))

  result <- assess_qc_coverage(master_list)

  expect_equal(result$project_details$qc_passed$plate1$QC, "fail")
  expect_equal(result$project_details$global_qc_pass$QC, "fail")
})

test_that("assess_qc_coverage handles multiple QC types correctly", {
  master_list <- list(
    project_details = list(),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_type = c(rep("QC1", 8), rep("QC2", 1), rep("sample", 95))
          )))))

  result <- assess_qc_coverage(master_list)

  expect_equal(result$project_details$qc_passed$plate1$QC1, "pass")
  expect_equal(result$project_details$qc_passed$plate1$QC2, "fail")
  expect_equal(result$project_details$global_qc_pass$QC1, "pass")
  expect_equal(result$project_details$global_qc_pass$QC2, "fail")
})

test_that("set_project_qc_type uses user-specified QC type when it passes", {
  suppressMessages({
  master_list <- list(
    project_details = list(
      qc_type = "qc1",
      global_qc_pass = list(qc1 = "pass"),
      qc_passed = list(plate1 = list(qc1 = "pass"))
    )
  )

  result <- set_project_qc_type(master_list)
  expect_equal(result$project_details$qc_type, "qc1")
  })
})

test_that("set_project_qc_type selects alternative QC type when user-specified fails", {
  suppressMessages({
  master_list <- list(
    project_details = list(
      qc_type = "qc1",
      global_qc_pass = list(qc1 = "fail", qc2 = "pass"),
      qc_passed = list(plate1 = list(qc2 = "pass"), plate2 = list(qc2 = "pass"))
    )
  )

  result <- suppressWarnings(set_project_qc_type(master_list))
  warns <- testthat::capture_warnings(set_project_qc_type(master_list))
  expect_true(any(grepl(
    "User-specified QC type qc1 did not pass QC checks.", warns
  )))

  expect_equal(result$project_details$qc_type, "qc2")
  })
})

test_that("set_project_qc_type stops when no QC types pass", {
  master_list <- list(
    project_details = list(
      qc_type = "qc1",
      global_qc_pass = list(qc1 = "fail"),
      qc_passed = list(plate1 = list(qc1 = "fail"))
    )
  )

  expect_error(
    suppressWarnings(set_project_qc_type(master_list)),
    "No QC types passed. Stopping script")
})

test_that("finalise_sorted_data adds factor columns and standardises sample_type", {
  master_list <- list(
    project_details = list(
      qc_type = "qc1",
      sample_tags = c("sample", "qc1", "ltr", "pqc", "vltr", "sltr")
    ),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_name = c("S1", "S2"),
            sample_type = c("qc1", "sample"),
            sample_timestamp = as.POSIXct(c("2023-01-01", "2023-01-02")),
            sample_plate_id = "plate1"
          )))))

  result <- finalise_sorted_data(master_list)
  sorted <- result$data$peakArea$sorted$plate1

  expect_s3_class(sorted$sample_type_factor, "factor")
  expect_equal(levels(sorted$sample_type_factor), c("sample", "qc1", "ltr", "pqc", "vltr", "sltr"))
  expect_equal(as.character(sorted$sample_type), c("qc", "sample"))
  expect_equal(sorted$sample_data_source, rep(".peakArea", 2))
  # QC-C1: sample_class is the new 3-way column.
  expect_true("sample_class" %in% colnames(sorted))
  expect_equal(sorted$sample_class, c("qc", "sample"))
})

test_that("finalise_sorted_data populates sample_class with qc/sample/other (QC-C1)", {
  # Fixture covers the four tag categories: chosen QC, biological sample,
  # non-chosen QC, SIL-only injection, and blank. Only the chosen qc_type
  # becomes "qc" in sample_class; everything other than biological samples
  # must land in "other".
  master_list <- list(
    project_details = list(
      qc_type = "pqc",
      sample_tags = c("sample", "pqc", "ltr", "SIL", "blank")
    ),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_name = paste0("S", 1:5),
            sample_type = c("pqc", "sample", "ltr", "SIL", "blank"),
            sample_timestamp = as.POSIXct(paste0("2023-01-0", 1:5)),
            sample_plate_id = "plate1"
          )))))

  result <- finalise_sorted_data(master_list)
  sorted <- result$data$peakArea$sorted$plate1

  expect_true("sample_class" %in% colnames(sorted))
  # All three expected values should appear.
  expect_setequal(unique(sorted$sample_class), c("qc", "sample", "other"))
  # Row-by-row 3-way assignment.
  expect_equal(sorted$sample_class,
               c("qc", "sample", "other", "other", "other"))
  # Legacy sample_type remains the 2-way collapse, for backwards compatibility.
  expect_equal(sorted$sample_type,
               c("qc", "sample", "sample", "sample", "sample"))
})

test_that("finalise_sorted_data creates reversed factor levels", {
  master_list <- list(
    project_details = list(
      qc_type = "qc1",
      sample_tags = c("sample", "qc1", "ltr", "pqc", "vltr", "sltr")
    ),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_name = c("S1", "S2"),
            sample_type = c("qc1", "sample"),
            sample_timestamp = as.POSIXct(c("2023-01-01", "2023-01-02")),
            sample_plate_id = "plate1"
          )))))

  result <- finalise_sorted_data(master_list)
  sorted <- result$data$peakArea$sorted$plate1

  expect_s3_class(sorted$sample_type_factor_rev, "factor")
  expect_equal(levels(sorted$sample_type_factor_rev), rev(levels(sorted$sample_type_factor)))
})

test_that("finalise_sorted_data assigns sample_run_index and splits by plate", {
  master_list <- list(
    project_details = list(
      qc_type = "qc1",
      sample_tags = c("sample", "qc1")
    ),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(
            sample_name = "S1",
            sample_type = "qc1",
            sample_timestamp = as.POSIXct("2023-01-01"),
            sample_plate_id = "plate1"
          ),
          plate2 = tibble(
            sample_name = "S2",
            sample_type = "sample",
            sample_timestamp = as.POSIXct("2023-01-02"),
            sample_plate_id = "plate2"
          )))))

  result <- finalise_sorted_data(master_list)

  expect_equal(result$data$peakArea$sorted$plate1$sample_run_index, 1)
  expect_equal(result$data$peakArea$sorted$plate2$sample_run_index, 2)
})


##Impute tests----
test_that("qcCheckR_impute_data populates imputed data for each plate", {
  master_list <- list(
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = tibble(sample_name = "S1"),
          plate2 = tibble(sample_name = "S2")
        )
      )
    )
  )

  local_mocked_bindings(
    prepare_imputation_matrix = function(data) matrix(1, nrow = 1, ncol = 1),
    apply_lgw_imputation = function(mat) tibble(MolA = 0.5),
    merge_metadata = function(meta, imputed) bind_cols(meta, imputed)
  )

  result <- qcCheckR_impute_data(master_list)

  expect_true("plate1" %in% names(result$data$peakArea$imputed))
  expect_true("plate2" %in% names(result$data$peakArea$imputed))
  expect_named(result$data$peakArea$imputed$plate1, c("sample_name", "MolA"))
})

test_that("prepare_imputation_matrix sets rownames and removes sample columns", {
  input <- tibble(
    sample_name = c("S1", "S2"),
    MolA = c(1, 2),
    sample_type = c("qc", "sample"),
    sample_data_source = ".peakArea"
  )

  mock_replace <- function(df) df

  local_mocked_bindings(replace_problematic_values = mock_replace)

  result <- prepare_imputation_matrix(input)

  expect_equal(rownames(result), c("S1", "S2"))
  expect_named(result, "MolA")
})

test_that("prepare_imputation_matrix replaces problematic values", {
  input <- tibble(
    sample_name = c("S1", "S2"),
    MolA = c(0, Inf),
    sample_type = c("qc", "sample")
  )

  mock_replace <- function(df) {
    df$MolA <- c(0.5, 0.5)
    df
  }

  local_mocked_bindings(replace_problematic_values = mock_replace)

  result <- prepare_imputation_matrix(input)

  expect_equal(result$MolA, c(0.5, 0.5))
})

test_that("prepare_imputation_matrix excludes numeric sample_* metadata", {
  # Regression: sample_run_index and sample_plate_order are numeric. Without
  # an explicit drop they were carried into the imputation matrix and then
  # collided with the metadata in merge_metadata()'s left_join, producing
  # sample_run_index.x / .y suffixes and breaking statTarget pheno build
  # ("Column `sample_run_index` doesn't exist").
  input <- tibble(
    sample_name = c("S1", "S2"),
    sample_run_index = c(1L, 2L),
    sample_plate_order = c(1L, 2L),
    MolA = c(10, 20)
  )
  mock_replace <- function(df) df
  local_mocked_bindings(replace_problematic_values = mock_replace)
  result <- prepare_imputation_matrix(input)
  expect_named(result, "MolA")
  expect_false(any(grepl("^sample", colnames(result))))
})

test_that("lgw_impute replaces zeros with half the minimum non-zero value", {
  input <- tibble(a = c(0, 2, 4), b = c(0, 0, 6))
  result <- lgw_impute(input)
  expect_equal(result$a[1], 1)  # half of 2
  expect_equal(result$b[1], 3)  # half of 6
  expect_equal(result$b[2], 3)
})

test_that("lgw_impute drops all-zero / all-NA columns (QC-H8)", {
  # QC-H8: columns with no usable values can't be imputed with min/2, so
  # they are dropped entirely (with an informational message) rather than
  # left as all-NA -- otherwise downstream RSD mixes NA with min/2.
  input <- tibble(a = c(0, 0, 0), b = c(2, 4, 6))
  result <- suppressMessages(lgw_impute(input))
  expect_false("a" %in% colnames(result))
  expect_true("b" %in% colnames(result))
})

test_that("lgw_impute handles mixed zero and NA values", {
  input <- tibble(a = c(0, NA, 4))
  result <- lgw_impute(input)
  expect_equal(result$a[1], 2)
  expect_equal(result$a[2], 2)
})

test_that("lgw_impute returns a data.frame", {
  input <- tibble(a = c(0, 2, 4))
  result <- lgw_impute(input)
  expect_s3_class(result, "data.frame")
})

test_that("lgw_impute coerces non-numeric columns", {
  input <- tibble(a = c("0", "2", "4"))
  result <- lgw_impute(input)
  expect_type(result$a, "double")
  expect_equal(result$a[1], 1)  # half of 2

})

test_that("apply_lgw_imputation returns a tibble with sample_name column", {
  input <- tibble(row.names = c("sample1", "sample2"),
                  a = c(0, 2),
                  b = c(4, 0))
  result <- apply_lgw_imputation(input)
  expect_s3_class(result, "tbl_df")
  expect_true("sample_name" %in% colnames(result))
})

test_that("apply_lgw_imputation replaces zeros with half the minimum non-zero value", {
  input <- tibble(row.names = c("s1", "s2"),
                  a = c(0, 2),
                  b = c(4, 0))
  result <- apply_lgw_imputation(input)
  expect_equal(result$a[1], 1)  # half of 2
  expect_equal(result$b[2], 2)  # half of 4
})

test_that("apply_lgw_imputation replaces Inf values with 1", {
  input <- tibble(row.names = c("s1", "s2"),
                  a = c(Inf, 2),
                  b = c(4, Inf))
  result <- apply_lgw_imputation(input)
  expect_equal(result$a[1], 1)
  expect_equal(result$b[2], 1)
})


test_that("matrix conversion preserves column structure", {
  input <- matrix(c(0, 2, 4, 0), nrow = 2, byrow = TRUE)
  colnames(input) <- c("a", "b")
  df <- as.data.frame(input)
  expect_equal(colnames(df), c("a", "b"))
})

test_that("merge_metadata correctly merges by sample_name", {
  original <- tibble(sample_name = c("s1", "s2"), sample_meta = c("A", "B"), Lipx = c("1","2"))
  imputed <- tibble(sample_name = c("s1", "s2"), a = c(1, 2))
  result <- merge_metadata(original, imputed)
  expect_equal(result$sample_meta, c("A", "B"))
  expect_equal(result$a, c(1, 2))
  expect_equal(result$sample_name, c("s1","s2"))
})

test_that("merge_metadata adds sample_data_source column", {
  original <- tibble(sample_name = c("s1"), meta = "A")
  imputed <- tibble(sample_name = c("s1"), a = 1)
  result <- merge_metadata(original, imputed)
  expect_true("sample_data_source" %in% colnames(result))
  expect_equal(result$sample_data_source, ".peakAreaImputed")
})

test_that("merge_metadata handles unmatched sample_name values", {
  original <- tibble(sample_name = c("s1"), meta = "A")
  imputed <- tibble(sample_name = c("s2"), a = 2)
  result <- merge_metadata(original, imputed)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$a))
})


##Calculate response and concentration tests----
test_that("qcCheckR_calculate_response_concentration returns updated master_list", {
  local_mocked_bindings(
    calculate_plate_response_concentration = function(ml, pid, dt, tv) {
      ml$data$response[[dt]] <- paste("response", pid, dt)
      ml$data$concentration[[dt]] <- paste("concentration", pid, dt)
      return(ml)
    },
    harmonise_lipid_columns = function(ml) {
      ml$harmonised <- TRUE
      return(ml)
    }
  )

  # Minimal mock master_list
  master_list <- list(
    data = list(
      peakArea = list(
        imputed = list("plate1" = data.frame())
      )
    ),
    project_details = list(
      plate_method_versions = list("plate1" = "v1")
    ),
    templates = list("Plate SIL version" = list())
  )

  result <- qcCheckR_calculate_response_concentration(master_list)

  expect_type(result, "list")
  expect_true("response" %in% names(result$data))
  expect_true("concentration" %in% names(result$data))
  expect_equal(result$data$response$sorted, "response plate1 sorted")
  expect_equal(result$data$concentration$imputed, "concentration plate1 imputed")
  expect_equal(result$project_details$is_ver, "v1")
  expect_equal(result$templates[["Plate SIL version"]][["plate1"]], "v1")
  expect_true(result$harmonised)
})

test_that("calculate_plate_response_concentration processes SIL columns and updates master_list", {
  local_mocked_bindings(
    process_sil_target = function(ml, plate_id, data_type, template_version, sil) {
      ml$processed_targets <- c(ml$processed_targets, sil)
      return(ml)
    }
  )

  # Create mock data
  mock_data <- data.frame(sample_1 = 1:3,sample_2 = 4:6,SIL_A = 7:9,SIL_B = 10:12)

  master_list <- list(
    data = list(
      peakArea = list(
        imputed = list(
          plate1 = mock_data
        ))),
    templates = list(
      mrm_guides = list(
        v1 = list(
          SIL_guide = list(
            Note = c("SIL_A", "SIL_B", "SIL_C")  # SIL_C not in data
          )))))

  result <- calculate_plate_response_concentration(master_list, "plate1", "imputed", "v1")

  # Check that SIL columns were reordered
  reordered_cols <- colnames(result$data$peakArea$imputed$plate1)
  expect_true(all(c("SIL_A", "SIL_B") %in% reordered_cols))
  expect_true(!"SIL_C" %in% reordered_cols)

  # Check that response and concentration were created
  expect_true("response" %in% names(result$data))
  expect_true("concentration" %in% names(result$data))
  expect_true("plate1" %in% names(result$data$response$imputed))
  expect_true("plate1" %in% names(result$data$concentration$imputed))

  # Check sample_data_source tags
  expect_equal(
    unique(result$data$response$imputed$plate1$sample_data_source),
    ".response.imputed")

  expect_equal(
    unique(result$data$concentration$imputed$plate1$sample_data_source),
    "concentration.imputed")

  # Check that process_sil_target was called for SIL_A and SIL_B only
  expect_equal(result$processed_targets, c("SIL_A", "SIL_B"))
})

test_that("harmonise_lipid_columns takes the union of columns across plates (QC-H9)", {
  # QC-H9: metabolites missing from any plate must be filled with NA on
  # plates that lack them, not silently dropped project-wide.
  df1 <- data.frame(A = 1:3, B = 4:6, C = 7:9)
  df2 <- data.frame(B = 10:12, C = 13:15, D = 16:18)

  master_list <- list(
    data = list(
      response = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        )
      ),
      concentration = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        )
      )
    )
  )

  result <- harmonise_lipid_columns(master_list)

  # Union of A, B, C (plate1) and B, C, D (plate2) should be retained.
  expect_setequal(colnames(result$data$response$subtype1$plate1), c("A", "B", "C", "D"))
  expect_setequal(colnames(result$data$response$subtype1$plate2), c("A", "B", "C", "D"))
  # Plate1 never had D -> filled with NA; plate2 never had A -> filled with NA.
  expect_true(all(is.na(result$data$response$subtype1$plate1$D)))
  expect_true(all(is.na(result$data$response$subtype1$plate2$A)))
  # Original values are preserved.
  expect_equal(result$data$response$subtype1$plate1$A, 1:3)
  expect_equal(result$data$response$subtype1$plate2$D, 16:18)
})

test_that("harmonise_lipid_columns retains all columns when only one plate is present", {
  df <- data.frame(X = 1:3, Y = 4:6)

  master_list <- list(
    data = list(
      response = list(
        subtype1 = list(
          plate1 = df
        )
      ),
      concentration = list(
        subtype1 = list(
          plate1 = df
        )
      )
    )
  )

  result <- harmonise_lipid_columns(master_list)

  expect_equal(colnames(result$data$response$subtype1$plate1), c("X", "Y"))
  expect_equal(colnames(result$data$concentration$subtype1$plate1), c("X", "Y"))
})

test_that("harmonise_lipid_columns unions disjoint columns across plates (QC-H9)", {
  # QC-H9: disjoint plates should union to A + B; missing values filled NA.
  df1 <- data.frame(A = 1:3)
  df2 <- data.frame(B = 4:6)

  master_list <- list(
    data = list(
      response = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        )
      ),
      concentration = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        ))))

  result <- harmonise_lipid_columns(master_list)

  expect_setequal(colnames(result$data$response$subtype1$plate1), c("A", "B"))
  expect_setequal(colnames(result$data$response$subtype1$plate2), c("A", "B"))
  expect_true(all(is.na(result$data$response$subtype1$plate1$B)))
  expect_true(all(is.na(result$data$response$subtype1$plate2$A)))
})

test_that("harmonise_lipid_columns handles empty or NULL data frames gracefully (QC-H9)", {
  df1 <- data.frame(A = 1:3, B = 4:6)
  df2 <- data.frame()  # empty

  master_list <- list(
    data = list(
      response = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        )
      ),
      concentration = list(
        subtype1 = list(
          plate1 = df1,
          plate2 = df2
        ))))

  result <- harmonise_lipid_columns(master_list)

  # Union of {A,B} and {} -> {A,B}. plate1 keeps its values; plate2 is
  # empty (0 rows) but now has A, B columns available (each of length 0).
  expect_setequal(colnames(result$data$response$subtype1$plate1), c("A", "B"))
  expect_equal(result$data$response$subtype1$plate1$A, 1:3)
})


## Stattarget batch correction tests----
test_that("qcCheckR_statTarget_batch_correction runs statTarget workflow correctly", {
  # Mock each helper function to simulate expected behavior
  local_mocked_bindings(
    initialise_statTarget_environment = function(master_list) {
      list(env = "initialised", log = list())
    },
    prepare_statTarget_files = function(FUNC_list) {
      FUNC_list$files_prepared <- TRUE
      return(FUNC_list)
    },
    run_statTarget_shiftCor = function(FUNC_list, master_list) {
      FUNC_list$correction_done <- TRUE
      return(FUNC_list)
    },
    integrate_corrected_data = function(master_list, FUNC_list) {
      master_list$corrected_data <- TRUE
      return(master_list)
    },
    update_script_log = function(master_list, ...) {
      master_list$log <- list(steps = c(...))
      return(master_list)
    }
  )

  # Minimal mock master_list
  master_list <- list()

  # Run the function
  result <- qcCheckR_statTarget_batch_correction(master_list)

  # Assertions
  expect_type(result, "list")
  expect_true(result$corrected_data)
  expect_equal(result$log$steps, c("data_preparation", "project_setup", "data_filtering"))
})

test_that("initialise_statTarget_environment sets up environment and returns correct FUNC_list", {
  local_mocked_bindings(
    check_dir_exists = function(path) FALSE,
    create_dir = function(path) TRUE,
    flag_failed_qc_injections = function(FUNC_list) {
      FUNC_list$flagged <- TRUE
      return(FUNC_list)
    }
  )

  master_list <- list(
    project_details = list(
      project_dir = "mock_project",
      qc_type = "QC"
    ),
    data = list(
      concentration = list(
        imputed = list(
          plate1 = data.frame(
            sample_type_factor = c("Sample", "QC"),
            lipid1 = c(1, 2),
            lipid2 = c(3, 4)
          )))))

  FUNC_list <- initialise_statTarget_environment(master_list)

  expect_equal(FUNC_list$project_dir, file.path("mock_project", "all", "data", "batch_correction"))
  expect_s3_class(FUNC_list$master_data, "data.frame")
  expect_equal(nrow(FUNC_list$master_data), 2)
  expect_equal(FUNC_list$master_data$sample_type, c("sample", "qc"))
  expect_false(any(grepl("sample", FUNC_list$metabolite_list, ignore.case = TRUE)))
  expect_true(FUNC_list$flagged)
})


test_that("create_pheno_file works with typical input", {
  master_data <- tibble(
    sample_name = c("S1", "S2", "S3", "S4"),
    sample_plate_id = c("A", "A", "B", "B"),
    sample_type = c("qc", "sample", "sample", "qc"),
    sample_run_index = c(2, 1, 3, 4)
  )

  FUNC_list <- list(
    master_data = master_data,
    project_dir = tempdir(),
    PhenoFile = list()
  )

  d <- Sys.Date()
  result <- create_pheno_file(FUNC_list)

  expect_true("PhenoFile" %in% names(result))
  expect_true("template_qc_order" %in% names(result$PhenoFile))
  expect_true("template_sample_id" %in% names(result$PhenoFile))
  expect_equal(nrow(result$PhenoFile$template_sample_id), 6)
  expect_true(file.exists(file.path(result$project_dir, paste0(d, "_signal_correction_results"), "PhenoFile.csv")))
})

test_that("create_pheno_file handles edge case: no QC samples", {
  master_data <- tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("A", "A"),
    sample_type = c("sample", "sample"),
    sample_run_index = c(1, 2)
  )

  FUNC_list <- list(
    master_data = master_data,
    project_dir = tempdir(),
    PhenoFile = list()
  )

  result <- create_pheno_file(FUNC_list)

  expect_equal(sum(is.na(result$PhenoFile$template_sample_id$class)), 0)
  expect_equal(nrow(result$PhenoFile$template_sample_id), 2)
})

test_that("create_pheno_file handles edge case: all QC samples", {
  master_data <- tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("A", "A"),
    sample_type = c("qc", "qc"),
    sample_run_index = c(1, 2)
  )

  FUNC_list <- list(
    master_data = master_data,
    project_dir = tempdir(),
    PhenoFile = list()
  )

  result <- create_pheno_file(FUNC_list)

  expect_equal(sum(is.na(result$PhenoFile$template_sample_id$class)), 2)
  expect_equal(nrow(result$PhenoFile$template_sample_id), 2)
})

test_that("create_pheno_file throws error with missing required columns", {
  master_data <- tibble(
    sample_name = c("S1", "S2"),
    sample_plate_id = c("A", "A"),
    sample_run_index = c(1, 2)
    # Missing sample_type
  )

  FUNC_list <- list(
    master_data = master_data,
    project_dir = tempdir(),
    PhenoFile = list()
  )

  expect_error(create_pheno_file(FUNC_list), "Column `sample_type` doesn't exist")
})

test_that("create_pheno_file throws error with non-dataframe master_data", {
  FUNC_list <- list(
    master_data = "not a dataframe",
    project_dir = tempdir(),
    PhenoFile = list()
  )

  expect_error(create_pheno_file(FUNC_list), regexp = "data frame|must be a")
})


test_that("create_profile_file works with typical input", {
  master_data <- tibble(
    sample_name = c("S1", "S2", "S3", "S4"),
    Met1 = c(1.1, 2.2, 3.3, 4.4),
    Met2 = c(5.5, 6.6, 7.7, 8.8),
    SIL_Met = c(9.9, 10.1, 11.2, 12.3)
  )

  metabolite_list <- c("Met1", "Met2", "SIL_Met")

  pheno <- tibble(
    sample_name = c("S1", "S2", "S3", "S4"),
    sample = c("sample1", "sample2", "sample3", "sample4")
  )

  FUNC_list <- list(
    master_data = master_data,
    metabolite_list = metabolite_list,
    project_dir = tempdir(),
    PhenoFile = list(template_sample_id = pheno)
  )

  d <- Sys.Date()
  result <- create_profile_file(FUNC_list)

  expect_true("ProfileFile" %in% names(result))
  expect_true("ProfileFile" %in% names(result$ProfileFile))
  expect_true("metabolite_list" %in% names(result$ProfileFile))
  expect_equal(nrow(result$ProfileFile$ProfileFile), 2) # SIL metabolite excluded
  expect_true(file.exists(file.path(result$project_dir, paste0(d, "_signal_correction_results"), "ProfileFile.csv")))
})

test_that("create_profile_file handles empty metabolite list", {
  master_data <- tibble(sample_name = c("S1", "S2"))
  metabolite_list <- character(0)
  pheno <- tibble(sample_name = c("S1", "S2"), sample = c("sample1", "sample2"))

  FUNC_list <- list(
    master_data = master_data,
    metabolite_list = metabolite_list,
    project_dir = tempdir(),
    PhenoFile = list(template_sample_id = pheno)
  )

  expect_error(create_profile_file(FUNC_list), "metabolite_list cannot be empty")
})

test_that("create_profile_file errors with missing sample_name in master_data", {
  master_data <- tibble(Met1 = c(1, 2), Met2 = c(3, 4))
  metabolite_list <- c("Met1", "Met2")
  pheno <- tibble(sample_name = c("S1", "S2"), sample = c("sample1", "sample2"))

  FUNC_list <- list(
    master_data = master_data,
    metabolite_list = metabolite_list,
    project_dir = tempdir(),
    PhenoFile = list(template_sample_id = pheno)
  )

  expect_error(create_profile_file(FUNC_list),
               "Column `sample_name` doesn't exist")
})

test_that("create_profile_file errors with non-existent metabolite columns", {
  master_data <- tibble(sample_name = c("S1", "S2"), Met1 = c(1, 2))
  metabolite_list <- c("Met1", "MetX") # MetX does not exist
  pheno <- tibble(sample_name = c("S1", "S2"), sample = c("sample1", "sample2"))

  FUNC_list <- list(
    master_data = master_data,
    metabolite_list = metabolite_list,
    project_dir = tempdir(),
    PhenoFile = list(template_sample_id = pheno)
  )

  expect_error(create_profile_file(FUNC_list), "Element `MetX` doesn't exist")

})

test_that("create_profile_file errors with malformed PhenoFile", {
  master_data <- tibble(
    sample_name = c("S1", "S2"),
    Met1 = c(1.1, 2.2),
    Met2 = c(3.3, 4.4)
  )

  metabolite_list <- c("Met1", "Met2")

  FUNC_list <- list(
    master_data = master_data,
    metabolite_list = metabolite_list,
    project_dir = tempdir(),
    PhenoFile = list(template_sample_id = NULL) # Missing template_sample_id
  )

  expect_error(create_profile_file(FUNC_list), regexp = "data frame|must be a|NULL")
})


test_that("clean_statTarget_output handles sample1 column correctly", {
  input <- tibble(
    sample = c("class", "M1", "M2"),
    sample1 = c("not_numeric", "1.1", "2.2"),
    sample2 = c("not_numeric", "3.3", "4.4")
  )

  result <- clean_statTarget_output(input)

  expect_equal(colnames(result), c("name", "sample1", "sample2"))
  expect_false(any(result$name == "class"))
  expect_true(all(sapply(result[-1], is.numeric)))
})

test_that("clean_statTarget_output handles M1 column correctly", {
  input <- data.frame(
    M1 = c("class", "sample", "5.5"),
    M2 = c("class", "sample", "6.6"),
    row.names = c("class", "sample", "Met1")
  )

  result <- clean_statTarget_output(input)

  expect_true("name" %in% colnames(result))
  expect_false(any(result$name %in% c("class", "sample")))
})

test_that("clean_statTarget_output returns unchanged data if no matching columns", {
  input <- tibble(
    A = c("x", "y"),
    B = c("1", "2")
  )

  result <- clean_statTarget_output(input)

  expect_equal(result, input)
})

test_that("transpose_and_merge_corrected returns expected structure", {
  FUNC_list <- list(
    corrected_data = list(data = tibble(name = "M1", sample1 = "1.1")),
    ProfileFile = list(metabolite_list = tibble(name = "M1", metabolite_code = "M1")),
    PhenoFile = list(template_sample_id = tibble(
        sample = "sample1",
        sample_name = "S1",
        batch = 1,
        class = "qc",
        order = 1
      )),
    master_data = tibble(
      sample_name = "S1",
      sample_run_index = 1,
      sample_timestamp = "2025-09-08 10:00:00",
      sample_plate_id = "A",
      sample_plate_order = 1,
      sample_matrix = "plasma",
      sample_type = "qc",
      sample_type_factor = "qc",
      sample_type_factor_rev = "rev_qc",
      sample_data_source = "internal"
    )
  )

  result <- transpose_and_merge_corrected(FUNC_list)

  expect_true("data_transposed" %in% names(result$corrected_data))
  expect_equal(nrow(result$corrected_data$data_transposed), 1)
  expect_equal(colnames(result$corrected_data$data_transposed)[1], "sample_run_index")
})

test_that("transpose_and_merge_corrected succeeds with missing optional master_data columns", {
  FUNC_list <- list(
    corrected_data = list(data = tibble(name = "M1", sample1 = "1.1")),
    ProfileFile = list(metabolite_list = tibble(name = "M1", metabolite_code = "M1")),
    PhenoFile = list(template_sample_id = tibble(
      sample = "sample1",
      sample_name = "S1",
      batch = 1,
      class = "qc",
      order = 1
    )),
    master_data = tibble(sample_name = "S1") # missing optional columns
  )

  result <- transpose_and_merge_corrected(FUNC_list)
  expect_true("data_transposed" %in% names(result$corrected_data))
})

test_that("transpose_and_merge_corrected handles empty corrected data", {
  FUNC_list <- list(
    corrected_data = list(data = tibble()),
    ProfileFile = list(metabolite_list = tibble(name = "M1", metabolite_code = "M1")),
    PhenoFile = list(template_sample_id = tibble(sample = "sample1", sample_name = "S1")),
    master_data = tibble(sample_name = "S1")
  )

  expect_error(transpose_and_merge_corrected(FUNC_list), "Column `name` doesn't exist")

})

test_that("transpose_and_merge_corrected converts values to numeric", {
  FUNC_list <- list(
    corrected_data = list(data = tibble(name = "M1", sample1 = "1.1")),
    ProfileFile = list(metabolite_list = tibble(name = "M1", metabolite_code = "M1")),
    PhenoFile = list(template_sample_id = tibble(sample = "sample1",
                                                 sample_name = "S1",
                                                 batch = 1,
                                                 class = "qc",
                                                 order = 1)),
    master_data = tibble(
      sample_name = "S1",
      sample_run_index = 1,
      sample_timestamp = "2025-09-08 10:00:00",
      sample_plate_id = "A",
      sample_plate_order = 1,
      sample_matrix = "plasma",
      sample_type = "qc",
      sample_type_factor = "qc",
      sample_type_factor_rev = "rev_qc",
      sample_data_source = "internal"
    )
  )

  result <- transpose_and_merge_corrected(FUNC_list)
  numeric_cols <- result$corrected_data$data_transposed %>% select(!contains("sample"))
  expect_true(all(sapply(numeric_cols, function(x) all(!is.na(as.numeric(x))))))

})

test_that("adjust_qc_means correctly adjusts QC means with typical input", {
  FUNC_list <- list(
    metabolite_list = c("metabolite1", "metabolite2"),
    master_data = tibble(
      sample_type = c("qc", "qc", "sample"),
      metabolite1 = c(10, 20, 30),
      metabolite2 = c(5, 15, 25)
    ),
    corrected_data = list(
      data_transposed = tibble(
        sample_type = c("qc", "qc", "sample"),
        sample_type_factor = c("QC", "QC", "Sample"),
        metabolite1 = c(12, 24, 36),
        metabolite2 = c(6, 18, 30)
      )
    )
  )

  master_list <- list(
    data = list(
      concentration = list(
        imputed = list(
          tibble(sample_type = c("qc", "qc", "sample"),
                 sample_type_factor = c("QC", "QC", "Sample"))
        )
      )
    )
  )

  result <- adjust_qc_means(FUNC_list, master_list)

  expect_true("data_qc_mean_adjusted" %in% names(result$corrected_data))
  expect_equal(result$corrected_data$data_qc_mean_adjusted$metabolite1[1], 10)
  expect_equal(result$corrected_data$data_qc_mean_adjusted$metabolite2[1], 5)
  expect_equal(result$corrected_data$data_qc_mean_adjusted$sample_type[1], "qc")
})

test_that("adjust_qc_means handles edge case with zero original mean", {
  FUNC_list <- list(
    metabolite_list = c("metabolite1", "metabolite2"),
    master_data = tibble(
      sample_type = c("qc", "qc"),
      metabolite1 = c(0, 0),
      metabolite2 = c(10, 20)
    ),
    corrected_data = list(
      data_transposed = tibble(
        sample_type = c("qc", "qc"),
        sample_type_factor = c("QC", "QC"),
        metabolite1 = c(1, 2),
        metabolite2 = c(12, 24)
      )
    )
  )

  master_list <- list(
    data = list(
      concentration = list(
        imputed = list(
          tibble(sample_type = c("qc", "qc"),
                 sample_type_factor = c("QC", "QC"))
        )
      )
    )
  )

  warns <- testthat::capture_warnings(adjust_qc_means(FUNC_list, master_list))
  expect_true(any(grepl(
    "non-positive corrected/original QC-mean ratio", warns
  )))
})

test_that("adjust_qc_means excludes synthetic boundary QCs from corrected_means", {
  # Regression: synthetic boundary QCs (injected to satisfy REGfit's
  # first/last QC requirement) only exist in data_transposed, not in
  # master_data. Without exclusion, the corrected_means is over real +
  # synthetic QCs while original_means is over real QCs only -- the
  # asymmetry inflates ratios and previously tripped the >100x clamp in
  # bc_compute_mean_ratios for most metabolites on real ANPC plates.
  # Construct a setup where the synthetic value is far from real QCs:
  # if the synthetic were included, ratio = mean(c(10, 12, 1000))/10 = 34.07
  # (within the clamp but still wildly biased). With it excluded the
  # ratio is correctly 12/10 = 1.2.
  FUNC_list <- list(
    metabolite_list = "metabolite1",
    master_data = tibble(
      sample_type = c("qc", "sample"),
      sample_type_factor = c("QC", "Sample"),
      metabolite1 = c(10, 50)
    ),
    corrected_data = list(
      data_transposed = tibble(
        sample_type        = c("qc", "qc",   "sample"),
        sample_type_factor = c("QC", "QC",   "Sample"),
        synthetic_qc       = c(FALSE, TRUE,  FALSE),
        metabolite1        = c(12,    1000,  60)
      )
    )
  )
  master_list <- list(data = list(concentration = list(imputed = list(
    tibble(sample_type = c("qc", "sample"), sample_type_factor = c("QC", "Sample"))
  ))))
  result <- adjust_qc_means(FUNC_list, master_list)
  # Real-QC ratio: 12/10 = 1.2. After dividing the corrected real-QC value
  # (12) by 1.2, we should get back to 10.
  adj <- result$corrected_data$data_qc_mean_adjusted
  expect_equal(adj$metabolite1[1], 10)
})

test_that("adjust_qc_means throws error with missing required columns", {
  FUNC_list <- list(
    master_data = tibble(sample_type = c("qc", "qc")),
    corrected_data = list(
      data_transposed = tibble(sample_type = c("qc", "qc"),
                               sample_type_factor = c("QC", "QC"))
    )
  )

  master_list <- list(
    data = list(
      concentration = list(
        imputed = list(
          tibble(sample_type = c("qc", "qc"),
                 sample_type_factor = c("QC", "QC"))
        )
      )
    )
  )

  expect_error(adjust_qc_means(FUNC_list, master_list),
               regexp = "No metabolite columns found")
})


test_that("integrate_corrected_data integrates corrected data by sample_plate_id", {
  corrected_data <- tibble(
    sample_name = c("S1", "S2", "S3"),
    sample_ID = c("ID1", "ID2", "ID3"),
    sample_plate_id = c("plate1", "plate1", "plate2"),
    sample_type = c("qc", "sample", "qc"),
    sample_type_factor = c("QC", "Sample", "QC"),
    metabolite1 = c(1, 2, 3)
  )

  FUNC_list <- list(
    corrected_data = list(
      data_qc_mean_adjusted = corrected_data
    )
  )

  master_list <- list(
    data = list(
      concentration = list(),
      peakArea = list()
    )
  )

  result <- integrate_corrected_data(master_list, FUNC_list)

  expect_named(result$data$concentration$corrected, c("plate1", "plate2"))
  expect_equal(result$data$concentration$corrected$plate1$sample_data_source[1], ".peakAreaCorrected")
  expect_equal(result$data$peakArea$statTargetProcessed$plate1$sample_data_source[1], "concentration.statTarget")
  expect_equal(result$data$concentration$statTargetProcessed$plate2$sample_plate_id[1], "plate2")
})

test_that("integrate_corrected_data handles missing sample_plate_id column", {
  FUNC_list <- list(
    corrected_data = list(
      data_qc_mean_adjusted = tibble(
        sample_type = c("qc", "sample"),
        metabolite1 = c(1, 2)
      )
    )
  )

  master_list <- list(
    data = list(
      concentration = list(),
      peakArea = list()
    )
  )

  expect_error(integrate_corrected_data(master_list, FUNC_list), "sample_plate_id")
})

test_that("integrate_corrected_data handles empty corrected data", {
  FUNC_list <- list(
    corrected_data = list(
      data_qc_mean_adjusted = tibble(
        sample_name = character(),
        sample_ID = character(),
        sample_plate_id = character(),
        sample_type = character(),
        sample_type_factor = character(),
        metabolite1 = numeric()
      )
    )
  )

  master_list <- list(
    data = list(
      concentration = list(),
      peakArea = list()
    )
  )

  result <- integrate_corrected_data(master_list, FUNC_list)

  expect_equal(length(result$data$concentration$corrected), 0)
  expect_equal(length(result$data$peakArea$statTargetProcessed), 0)
  expect_equal(length(result$data$concentration$statTargetProcessed), 0)
})

test_that("integrate_corrected_data throws error with NULL inputs", {
  expect_error(integrate_corrected_data(NULL, NULL),
               regexp = "Missing required column: sample_plate_id")
  expect_error(integrate_corrected_data(list(), NULL),
               regexp = "Missing required column: sample_plate_id")
  expect_error(integrate_corrected_data(NULL, list()),
               regexp = "Missing required column: sample_plate_id")
})

test_that("integrate_corrected_data drops synthetic boundary QC rows", {
  # Regression: synthetic boundary QC rows (added by bc_prepare_qc_boundaries
  # for statTarget anchoring) leave NA sample_plate_id after the master_data
  # join and previously crashed integrate_corrected_data with
  # "replacement has 1 row, data has 0" when the loop hit the NA batch.
  corrected_data <- tibble(
    sample_name    = c("SYNTHETIC_QC_leading_plate1", "S1", "S2",
                       "SYNTHETIC_QC_leading_plate2", "S3"),
    sample_ID      = c(NA_character_, "ID1", "ID2", NA_character_, "ID3"),
    sample_plate_id = c(NA, "plate1", "plate1", NA, "plate2"),
    synthetic_qc    = c(TRUE, FALSE, FALSE, TRUE, FALSE),
    sample_type     = c("qc", "qc", "sample", "qc", "qc"),
    sample_type_factor = c("QC", "QC", "Sample", "QC", "QC"),
    metabolite1     = c(99, 1, 2, 99, 3)
  )
  FUNC_list <- list(corrected_data = list(data_qc_mean_adjusted = corrected_data))
  master_list <- list(data = list(concentration = list(), peakArea = list()))
  result <- integrate_corrected_data(master_list, FUNC_list)

  expect_named(result$data$concentration$corrected, c("plate1", "plate2"))
  # No NA-batch group, no synthetic rows in any output slot.
  for (slot in list(result$data$concentration$corrected,
                    result$data$peakArea$statTargetProcessed,
                    result$data$concentration$statTargetProcessed)) {
    expect_false(any(grepl("SYNTHETIC", unlist(lapply(slot, `[[`, "sample_name")))))
    expect_false(any(is.na(names(slot))))
  }
  expect_equal(nrow(result$data$concentration$corrected$plate1), 2)
  expect_equal(nrow(result$data$concentration$corrected$plate2), 1)
})

#Data Filtering set qc----
test_that("qcCheckR_set_qc sets QC type when one valid QC is present", {
  master_list <- list(
    project_details = list(
      project_name = "TestProject",
      global_qc_pass = c(pqc = "pass"),
      qc_passed = list()
    )
  )
  result <- suppressMessages(qcCheckR_set_qc(master_list))
  expect_equal(result$project_details$qc_type, "pqc")
  expect_type(result$filters, "list")
})

test_that("qcCheckR_set_qc uses user-supplied QC when multiple valid QCs exist", {
  master_list <- list(
    project_details = list(
      project_name = "TestProject",
      global_qc_pass = c(pqc = "pass", ltr = "pass"),
      qc_type = "ltr",
      qc_passed = list()
    )
  )

  result <- suppressMessages(qcCheckR_set_qc(master_list))
  expect_equal(result$project_details$qc_type, "ltr")
})

test_that("qcCheckR_set_qc errors when multiple QCs pass but user-supplied QC is invalid", {
  master_list <- list(
    project_details = list(
      project_name = "TestProject",
      global_qc_pass = c(pqc = "pass", ltr = "pass"),
      qc_type = "invalid_qc",
      qc_passed = list()
    )
  )

  expect_error(suppressMessages(qcCheckR_set_qc(master_list)), "STOPPING SCRIPT")
})

test_that("qcCheckR_set_qc errors when no valid QC types are present", {
  master_list <- list(
    project_details = list(
      project_name = "TestProject",
      global_qc_pass = c(pqc = "fail", ltr = "fail"),
      qc_passed = list()
    )
  )
  expect_error(suppressMessages(qcCheckR_set_qc(master_list)), "STOPPING SCRIPT")
})

test_that("qcCheckR_set_qc errors when global_qc_pass is NULL", {
  master_list <- list(
    project_details = list(
      project_name = "TestProject",
      global_qc_pass = NULL,
      qc_passed = list()
    )
  )
  expect_error(suppressMessages(qcCheckR_set_qc(master_list)), "STOPPING SCRIPT")
})

#Sample Filtering ----
test_that("qcCheckR_sample_filter flags samples correctly with valid input", {
  master_list <- list(
    project_details = list(mv_sample_threshold = 20),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = data.frame(
            sample_run_index = 1:3,
            sample_name = c("S1", "S2", "S3"),
            sample_plate_id = rep("plate1", 3),
            sample_type_factor = rep("Sample", 3),
            lipid1 = c(10000, 2000, 3000),
            lipid2 = c(12000, 1000, 4000),
            SIL1 = c(8000, 1000, 2000),
            SIL2 = c(9000, 500, 1000)
          )
        )
      )
    )
  )
  result <- qcCheckR_sample_filter(master_list)

  expect_true("samples.missingValues" %in% names(result$filters))
  expect_true("failed_samples" %in% names(result$filters))
  expect_type(result$filters$failed_samples, "character")
  expect_gt(length(result$filters$failed_samples), 0)
})

test_that("qcCheckR_sample_filter returns no failed samples when all values are good", {
  master_list <- list(
    project_details = list(mv_sample_threshold = 20),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = data.frame(
            sample_run_index = 1:2,
            sample_name = c("S1", "S2"),
            sample_plate_id = rep("plate1", 2),
            sample_type_factor = rep("Sample", 2),
            lipid1 = c(10000, 11000),
            lipid2 = c(12000, 13000),
            SIL1 = c(8000, 9000),
            SIL2 = c(9000, 9500)
          )
        )
      )
    )
  )
  result <- qcCheckR_sample_filter(master_list)
  expect_equal(length(result$filters$failed_samples), 0)
})

test_that("qcCheckR_sample_filter throws error with missing columns", {
  master_list <- list(
    project_details = list(mv_sample_threshold = 20),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = data.frame(
            sample_name = c("S1", "S2"),
            lipid1 = c(10000, 2000)
          )
        )
      )
    )
  )
  expect_error(qcCheckR_sample_filter(master_list), "Column `sample_run_index` doesn't exist")
})

test_that("qcCheckR_sample_filter handles empty batch gracefully", {
  master_list <- list(
    project_details = list(mv_sample_threshold = 20),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = data.frame()
        )
      )
    )
  )
  expect_error(qcCheckR_sample_filter(master_list), "Column `sample_run_index` doesn't exist")
})

test_that("qcCheckR_sample_filter does not count NAs as below-LOD pass (QC-C2)", {
  # QC-C2: rowSums(lipid_data < 5000) previously treated NA as pass with
  # na.rm = TRUE. An all-NA row should therefore NOT contribute to
  # missing.lipid counts -- instead the NA counter (`na.lipid`) picks them
  # up. Prior to the fix, the same NAs would have been double-counted.
  master_list <- list(
    project_details = list(mv_sample_threshold = 20),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = data.frame(
            sample_run_index = 1:2,
            sample_name = c("S_NA", "S_ok"),
            sample_plate_id = rep("plate1", 2),
            sample_type_factor = rep("Sample", 2),
            lipid1 = c(NA_real_, 10000),
            lipid2 = c(NA_real_, 11000),
            SIL1   = c(NA_real_, 8000),
            SIL2   = c(NA_real_, 9000)
          )
        )
      )
    )
  )
  result <- qcCheckR_sample_filter(master_list)
  mv <- result$filters$samples.missingValues
  s_na <- mv[mv$sample_name == "S_NA", ]
  # Below-LOD count must be zero (NAs are not < 5000); NAs bucket takes them.
  expect_equal(s_na$missing.lipid, 0)
  expect_equal(s_na$missing.SIL, 0)
  expect_equal(s_na$na.lipid, 2)
  expect_equal(s_na$na.SIL, 2)
})

#SIL Filter ----
test_that("qcCheckR_sil_IntStd_filter correctly flags SIL internal standards", {
  master_list <- list(
    project_details = list(),
    templates = list(`Plate SIL version` = list(batch1 = "v1")),
    filters = list(
      samples.missingValues = tibble(
        sample_name = c("S1", "S2", "S3"),
        sample_plate_id = c("batch1", "batch1", "batch1"),
        sample.flag = c(0, 0, 1)
      )
    ),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble(
            sample_name = c("S1", "S2", "S3"),
            sample_plate_id = c("batch1", "batch1", "batch1"),
            SIL_A = c(6000, 4000, 3000),
            SIL_B = c(7000, NA, Inf)
          )
        )
      )
    )
  )

  result <- qcCheckR_sil_IntStd_filter(master_list)

  expect_true("sil.intStd.missingValues" %in% names(result$filters))
  expect_true("summary" %in% names(result$filters$sil.intStd.missingValues))
  expect_true("failed_sil.intStds" %in% names(result$filters))
  expect_type(result$filters$failed_sil.intStds, "character")
})

test_that("qcCheckR_sil_IntStd_filter handles absence of SIL columns gracefully", {
  master_list <- list(
    project_details = list(),
    templates = list(`Plate SIL version` = list(batch1 = "v1")),
    filters = list(
      samples.missingValues = tibble(
        sample_name = c("S1", "S2"),
        sample_plate_id = c("batch1", "batch1"),
        sample.flag = c(0, 0)
      )
    ),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble(
            sample_name = c("S1", "S2"),
            sample_plate_id = c("batch1", "batch1"),
            lipid1 = c(10000, 12000)
          )
        )
      )
    )
  )

  result <- qcCheckR_sil_IntStd_filter(master_list)

  expect_true(nrow(result$filters$sil.intStd.missingValues$summary) == 0)
  expect_equal(length(result$filters$failed_sil.intStds), 0)
})

test_that("qcCheckR_sil_IntStd_filter handles all samples flagged", {
  master_list <- list(
    project_details = list(),
    templates = list(`Plate SIL version` = list(batch1 = "v1")),
    filters = list(
      samples.missingValues = tibble(
        sample_name = c("S1", "S2"),
        sample_plate_id = c("batch1", "batch1"),
        sample.flag = c(1, 1)
      )
    ),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble(
            sample_name = c("S1", "S2"),
            sample_plate_id = c("batch1", "batch1"),
            SIL_A = c(6000, 4000),
            SIL_B = c(7000, 3000)
          )
        )
      )
    )
  )
  result <- qcCheckR_sil_IntStd_filter(master_list)

  expect_true(all(result$filters$sil.intStd.missingValues$summary$totalMissingValues == 0))
  expect_equal(length(result$filters$failed_sil.intStds), 0)
})

test_that("qcCheckR_sil_IntStd_filter throws error with missing template version", {
  master_list <- list(
    project_details = list(),
    templates = list(),
    filters = list(
      samples.missingValues = tibble(
        sample_name = c("S1", "S2"),
        sample_plate_id = c("plate1", "plate1"),
        sample.flag = c(0, 0)
      )
    ),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble(
            sample_name = c("S1", "S2"),
            sample_plate_id = c("plate1", "plate1"),
            SIL_A = c(6000, 4000),
            SIL_B = c(7000, 3000)
          )
        )
      )
    )
  )

  expect_error(qcCheckR_sil_IntStd_filter(master_list), "Missing required column: template_version")
})


#Lipid Filter ----
test_that("qcCheckR_lipid_filter flags lipids correctly with valid input", {
  master_list <- list(
    project_details = list(mv_sample_threshold = 50),
    templates = list(
      `Plate SIL version` = list(batch1 = "v1"),
      mrm_guides = list(v1 = list(SIL_guide = tibble(Note = "SIL_A", `Precursor Name` = "lipid1")))
    ),
    filters = list(
      samples.missingValues = tibble(sample_name = c("S1", "S2"), sample.flag = c(0, 0)),
      failed_sil.intStds = "SIL_A"
    ),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble(
            sample_name = c("S1", "S2"),
            sample_plate_id = c("batch1", "batch1"),
            lipid1 = c(4000, 3000),
            lipid2 = c(6000, NA)
          )
        )
      )
    )
  )

  result <- qcCheckR_lipid_filter(master_list)

  expect_true("lipid.missingValues" %in% names(result$filters))
  expect_true("failed_lipids" %in% names(result$filters))
  expect_type(result$filters$failed_lipids, "character")
  expect_true(any(result$filters$lipid.missingValues$summary$flag.Lipid.Plate == 1))

})

test_that("qcCheckR_lipid_filter handles all samples flagged", {
  master_list <- list(
    project_details = list(mv_sample_threshold = 50),
    templates = list(`Plate SIL version` = list(batch1 = "v1")),
    filters = list(
      samples.missingValues = tibble(sample_name = c("S1", "S2"), sample.flag = c(1, 1)),
      failed_sil.intStds = character()
    ),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble(
            sample_name = c("S1", "S2"),
            sample_plate_id = c("batch1", "batch1"),
            lipid1 = c(4000, 3000),
            lipid2 = c(6000, NA)
          )
        )
      )
    )
  )

  result <- qcCheckR_lipid_filter(master_list)

  expect_true("lipid.missingValues" %in% names(result$filters))
  expect_true(nrow(result$filters$lipid.missingValues$summary) >= 0)
  # When all samples are flagged, lipid flags show 0 silFilter
  if (nrow(result$filters$lipid.missingValues$summary) > 0) {
    expect_true(all(result$filters$lipid.missingValues$summary$silFilter.flag.Lipid == 0))
  }
})

test_that("qcCheckR_lipid_filter throws error with missing template version", {
  master_list <- list(
    project_details = list(mv_sample_threshold = 50),
    templates = list(),
    filters = list(
      samples.missingValues = tibble(sample_name = c("S1", "S2"), sample.flag = c(0, 0)),
      failed_sil.intStds = character()
    ),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble(
            sample_name = c("S1", "S2"),
            sample_plate_id = c("plate1", "plate1"),
            lipid1 = c(4000, 3000),
            lipid2 = c(6000, NA)
          )
        )
      )
    )
  )

  expect_error(qcCheckR_lipid_filter(master_list), "Missing template version")

})

test_that("qcCheckR_lipid_filter handles absence of lipid columns gracefully", {
  master_list <- list(
    project_details = list(mv_sample_threshold = 50),
    templates = list(`Plate SIL version` = list(batch1 = "v1")),
    filters = list(
      samples.missingValues = tibble(sample_name = c("S1", "S2"), sample.flag = c(0, 0)),
      failed_sil.intStds = character()
    ),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble(
            sample_name = c("S1", "S2"),
            sample_plate_id = c("plate1", "plate1"),
            SIL_A = c(4000, 3000),
            SIL_B = c(6000, NA)
          )
        )
      )
    )
  )
  result <- qcCheckR_lipid_filter(master_list)

  expect_true("lipid.missingValues" %in% names(result$filters))
  expect_equal(nrow(result$filters$lipid.missingValues$summary), 0)
  expect_equal(length(result$filters$failed_lipids), 0)
})

#RSD Filter----
test_that("qcCheckR_RSD_filter calculates RSDs correctly for valid QC data", {
  qc_batch <- tibble::tibble(
    sample_name = c("QC1", "QC2", "QC3"),
    sample_type = c("qc", "qc", "qc"),
    feature1 = c(100, 110, 120),
    feature2 = c(200, 190, 180)
  )
  conc_batch <- tibble::tibble(
    sample_name = c("QC1", "QC2", "QC3"),
    sample_type = c("qc", "qc", "qc"),
    feature1 = c(10, 12, 14),
    feature2 = c(20, 18, 16)
  )
  st_batch <- tibble::tibble(
    sample_name = c("QC1", "QC2", "QC3"),
    sample_type = c("qc", "qc", "qc"),
    feature1 = c(10, 11, 12),
    feature2 = c(19, 18, 17)
  )
  master_list <- list(
    filters = list(failed_samples = character(0)),
    project_details = list(script_log = list(timestamps = list(
      start_time = Sys.time(), data_preparation = Sys.time()))),
    data = list(
      peakArea = list(
        # QC-C3: RSD now uses the pre-imputation "sorted" slot.
        sorted = list(batch1 = qc_batch),
        imputed = list(batch1 = qc_batch)
      ),
      concentration = list(
        sorted = list(batch1 = conc_batch),
        imputed = list(batch1 = conc_batch),
        statTargetProcessed = list(batch1 = st_batch)
      )
    )
  )

  result <- qcCheckR_RSD_filter(master_list)

  expect_true("rsd" %in% names(result$filters))
  expect_s3_class(result$filters$rsd, "tbl_df")
  expect_true(all(c("dataSource", "dataBatch") %in% names(result$filters$rsd)))
  expect_true(all(sapply(result$filters$rsd[-c(1, 2)], is.numeric)))
})

test_that("qcCheckR_RSD_filter skips batches with no QC samples (QC-H6 placeholders)", {
  # QC-C3: RSD now reads sorted (non-imputed) data.
  # QC-H6: when no batch produces rows, calculate_rsd emits labelled NA
  # placeholder rows so downstream consumers can see the stage ran.
  master_list <- list(
    filters = list(failed_samples = character()),
    project_details = list(script_log = list(timestamps = list(
      start_time = Sys.time(), data_preparation = Sys.time()))),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble::tibble(
            sample_name = c("S1", "S2"),
            sample_type = c("sample", "sample"),
            feature1 = c(100, 110)
          )
        )
      ),
      concentration = list(
        sorted = list(),
        statTargetProcessed = list()
      )
    )
  )

  result <- qcCheckR_RSD_filter(master_list)

  # No actual RSD values should be present: all placeholder rows are
  # labelled with dataSource/dataBatch only.
  expect_true(nrow(result$filters$rsd) > 0)
  expect_true(all(c("dataSource", "dataBatch") %in% names(result$filters$rsd)))
  # Every value column should be NA because no QC rows existed.
  val_cols <- setdiff(names(result$filters$rsd), c("dataSource", "dataBatch"))
  if (length(val_cols) > 0) {
    expect_true(all(is.na(unlist(result$filters$rsd[val_cols]))))
  }
})

test_that("qcCheckR_RSD_filter skips failed QC samples (QC-H6 placeholders)", {
  master_list <- list(
    filters = list(failed_samples = c("QC1", "QC2")),
    project_details = list(script_log = list(timestamps = list(
      start_time = Sys.time(), data_preparation = Sys.time()))),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble::tibble(
            sample_name = c("QC1", "QC2"),
            sample_type = c("qc", "qc"),
            feature1 = c(100, 110)
          )
        )
      ),
      concentration = list(
        sorted = list(),
        statTargetProcessed = list()
      )
    )
  )

  result <- qcCheckR_RSD_filter(master_list)

  # All QCs excluded -> placeholder rows with NA values only.
  expect_true(nrow(result$filters$rsd) > 0)
  val_cols <- setdiff(names(result$filters$rsd), c("dataSource", "dataBatch"))
  if (length(val_cols) > 0) {
    expect_true(all(is.na(unlist(result$filters$rsd[val_cols]))))
  }
})

test_that("qcCheckR_RSD_filter skips batches with missing required columns", {
  master_list <- list(
    filters = list(failed_samples = character()),
    project_details = list(script_log = list(timestamps = list(
      start_time = Sys.time(), data_preparation = Sys.time()))),
    data = list(
      peakArea = list(
        sorted = list(
          batch1 = tibble::tibble(
            feature1 = c(100, 110)
          )
        )
      ),
      concentration = list(
        sorted = list(),
        statTargetProcessed = list()
      )
    )
  )

  result <- qcCheckR_RSD_filter(master_list)
  # Malformed batch (no sample_name) -> no real rows, only placeholders.
  val_cols <- setdiff(names(result$filters$rsd), c("dataSource", "dataBatch"))
  if (length(val_cols) > 0) {
    expect_true(all(is.na(unlist(result$filters$rsd[val_cols]))))
  }
})


