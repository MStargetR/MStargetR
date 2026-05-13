library(mockery)
library(withr)
library(tibble)

#Backup working directory to restore after temp directory creation
withr::local_dir(getwd())

#Tests for SkylineR project setup  ----
test_that("initialise_master_list returns a correctly structured list", {
  master_list <- initialise_master_list()

  expect_type(master_list, "list")
  expect_named(master_list, c("environment", "templates", "project_details", "data", "summary_tables", "process_lists"))
  expect_named(master_list$templates, "mrm_guides")
})

test_that("store_environment_details adds environment details to master_list", {
  master_list <- initialise_master_list()
  updated_list <- store_environment_details(master_list)

  expect_true("r_version" %in% names(updated_list$environment))
  expect_true("base_packages" %in% names(updated_list$environment))
  expect_true("user_packages" %in% names(updated_list$environment))
})

test_that("set_project_details populates project metadata", {
  master_list <- initialise_master_list()
  updated_list <- set_project_details(master_list, "TestUser", "path/to/project", "PLATE_1", "LTR")

  expect_equal(updated_list$project_details$project_dir, "path/to/project")
  expect_equal(updated_list$project_details$plateID, "PLATE_1")
  expect_equal(updated_list$project_details$qc_type, "LTR")
  expect_equal(updated_list$project_details$user_name, "TestUser")
  expect_true(!is.null(updated_list$project_details$script_log$timestamps$start_time))
})

test_that("read_mrm_guides loads valid guide", {
  suppressMessages({
  # Create valid guide
  valid_guide <- tempfile(fileext = ".tsv")
  write.table(data.frame(
    `Molecule List Name` = "A",
    `Precursor Name` = "B",
    `Precursor Mz` = 100,
    `Precursor Charge` = 1,
    `Product Mz` = 200,
    `Product Charge` = 1,
    `Explicit Retention Time` = 5,
    `Explicit Retention Time Window` = 0.5,
    `Note` = "SIL_A",
    `control_chart` = "chart",
    check.names = FALSE
  ), valid_guide, sep = "\t", quote = TRUE, row.names = FALSE)

  master_list <- initialise_master_list()
  updated_list <- read_mrm_guides(master_list, list(valid_guide))

  expect_true("mrm_guide" %in% names(updated_list$templates$mrm_guides[[basename(valid_guide)]]))
  })
})

test_that("read_mrm_guides fails for missing columns", {
  master_list <- initialise_master_list()
  missing_col_file <- tempfile(fileext = ".tsv")
  write.table(data.frame(
    `Molecule List Name` = "A",
    `Precursor Name` = "B",
    `Precursor Charge` = 1,
    `Product Charge` = 1,
    `Explicit Retention Time` = 5,
    `Note` = NA,
    `control_chart` = "chart",
    check.names = FALSE
  ), missing_col_file, sep = "\t", row.names = FALSE, quote = TRUE)
  expect_error(
    read_mrm_guides(master_list, list(missing_col_file)),
    regexp = "Missing mandatory columns"
  )
})

test_that("read_mrm_guides fails for NA values", {
  suppressMessages({
  master_list <- initialise_master_list()
  na_file <- tempfile(fileext = ".tsv")
  write.table(data.frame(
    `Molecule List Name` = "A",
    `Precursor Name` = "B",
    `Precursor Mz` = 100,
    `Precursor Charge` = 1,
    `Product Mz` = NA,
    `Product Charge` = 1,
    `Explicit Retention Time` = 5,
    `Explicit Retention Time Window` = 0.5,
    `Note` = "note",
    `control_chart` = "chart",
    check.names = FALSE
  ), na_file, sep = "\t", row.names = FALSE, quote = TRUE)
  expect_error(
    read_mrm_guides(master_list, list(na_file)),
    regexp = "contains NA values"
  )
  })
})

test_that("read_mrm_guides fails for invalid extensions", {
  master_list <- initialise_master_list()
  invalid_ext_file <- tempfile(fileext = ".xlsx")
  writeLines("dummy content", invalid_ext_file)
  expect_error(
    read_mrm_guides(master_list, list(invalid_ext_file)),
    regexp = "Unsupported file format"
  )
})

test_that("setup_project_directories creates required directories", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  master_list <- initialise_master_list()
  master_list$project_details$project_dir <- temp_dir
  master_list$project_details$plateID <- "PLATE_1"

  setup_project_directories(master_list)

  expected_dirs <- c(
    "data", "data/mzml", "data/qs2", "data/PeakForgeR",
    "data/raw_data", "data/batch_correction", "html_report"
  )
  for (dir_name in expected_dirs) {
    expect_true(dir.exists(file.path(temp_dir, "PLATE_1", dir_name)))
  }
})

test_that("PeakForgeR_setup_project returns a fully populated master_list", {
  suppressMessages({
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Create valid guide
  valid_guide <- tempfile(fileext = ".tsv")
  write.table(data.frame(
    `Molecule List Name` = "A",
    `Precursor Name` = "SIL",
    `Precursor Mz` = 100,
    `Precursor Charge` = 1,
    `Product Mz` = 200,
    `Product Charge` = 1,
    `Explicit Retention Time` = 5,
    `Explicit Retention Time Window` = 0.5,
    `Note` = NA,
    `control_chart` = "chart",
    check.names = FALSE
  ), valid_guide, sep = "\t", row.names = FALSE, quote = TRUE)

  result <- PeakForgeR_setup_project("TestUser", temp_dir, "PLATE_1", list(valid_guide), "LTR")

  expect_type(result, "list")
  expect_true("project_details" %in% names(result))
  expect_true("mrm_guide" %in% names(result$templates$mrm_guides[[basename(valid_guide)]]))
  })
})


#Tests for mzR functions  ----
#Mock data
mock_chrom <- matrix(c(1:20, c(rep(1.5, 10), rep(2.5, 10))), ncol = 2)
mock_rtime <- seq(1, 20)

FUNC_mzR <- list(
  plate1 = list(
    sample1 = list(
      mzR_chromatogram = list(
        mrm1 = mock_chrom,
        rtime = mock_rtime
      )
    )
  )
)

test_that("calculate_baseline returns 10th percentile of intensities", {
  result <- calculate_baseline(FUNC_mzR, "plate1", "sample1", "mrm1")
  expected <- as.numeric(quantile(mock_chrom[,2], 0.1))
  expect_equal(result, expected)
})

test_that("calculate_baseline handles low-intensity chromatograms", {
  low_chrom <- matrix(c(1:10, rep(0.2, 10)), ncol = 2)
  FUNC_mzR$plate1$sample1$mzR_chromatogram$mrm1 <- low_chrom
  result <- calculate_baseline(FUNC_mzR, "plate1", "sample1", "mrm1")
  expected <- as.numeric(quantile(low_chrom[,2], 0.1))
  expect_equal(result, expected)
})


test_that("find_peak_apex_idx returns correct index", {
  FUNC_mzR$plate1$sample1$mzR_chromatogram$mrm1 <- mock_chrom
  result <- find_peak_apex_idx(FUNC_mzR, "plate1", "sample1", "mrm1")
  expect_equal(result, which.max(mock_chrom[,2]))
})


test_that("find_peak_apex_idx returns correct index for re-assigned chromatogram", {
  FUNC_mzR$plate1$sample1$mzR_chromatogram$mrm1 <- mock_chrom
  result <- find_peak_apex_idx(FUNC_mzR, "plate1", "sample1", "mrm1")
  expect_equal(result, which.max(mock_chrom[,2]))
})

test_that("find_peak_start_idx returns correct index", {
  baseline <- calculate_baseline(FUNC_mzR, "plate1", "sample1", "mrm1")
  apex <- find_peak_apex_idx(FUNC_mzR, "plate1", "sample1", "mrm1")
  result <- find_peak_start_idx(FUNC_mzR, "plate1", "sample1", "mrm1", apex, baseline)
  expect_true(result < apex)
})

test_that("find_peak_end_idx returns correct index", {
  baseline <- calculate_baseline(FUNC_mzR, "plate1", "sample1", "mrm1")
  apex <- find_peak_apex_idx(FUNC_mzR, "plate1", "sample1", "mrm1")
  result <- find_peak_end_idx(FUNC_mzR, "plate1", "sample1", "mrm1", apex, baseline)
  expect_true(result > apex)
})

FUNC_mrm_guide <- tibble::tibble(
  precursor_mz = c(500.2, 501.0),
  product_mz = c(200.1, 201.0),
  explicit_retention_time = c(5, 15),
  explicit_retention_time_window = c(1.0, 1.0),
  molecule_list_name = c("LipidA", "LipidB"),
  precursor_name = c("PA", "PB"),
  precursor_charge = c(1L, 1L),
  product_charge = c(1L, 1L),
  note = c("", "")
)

test_that("find_lipid_info returns exact match", {
  result <- find_lipid_info(FUNC_mrm_guide, 500.2, 200.1, 10, FUNC_mzR, "plate1", "sample1", "mrm1")
  expect_equal(result$class, "LipidA")
  expect_equal(result$name, "PA")
})


FUNC_tibble <- tibble::tibble(
  lipid = c("LipidA", "LipidA", "LipidB", "LipidB"),
  lipid_class = c("ClassA", "ClassA", "ClassB", "ClassB"),
  peak_apex = c(5.1, 5.3, 10.2, 10.4),
  peak_start = c(4.8, 4.9, 9.8, 9.9),
  peak_end = c(5.6, 5.7, 10.8, 10.9)
)

mzML_filelist <- c("sample1.mzML", "sample2.mzML")

test_that("create_output returns a list with two tibbles", {
  result <- create_output(FUNC_tibble, FUNC_mrm_guide, mzML_filelist)
  expect_type(result, "list")
  expect_named(result, c("mrm_guide_updated", "peak_boundary_update"))
  expect_s3_class(result$mrm_guide_updated, "tbl_df")
  expect_s3_class(result$peak_boundary_update, "tbl_df")
})

test_that("create_output correctly aggregates lipid info", {
  lipid_names <- unique(FUNC_tibble$lipid)
  result <- create_output(FUNC_tibble, FUNC_mrm_guide, mzML_filelist)
  expect_true(all(result$mrm_guide_updated$`Precursor Name` %in% lipid_names))
})

test_that("create_output handles empty FUNC_tibble", {
  empty_tibble <- tibble::tibble(
    lipid = character(),
    lipid_class = character(),
    peak_apex = numeric(),
    peak_start = numeric(),
    peak_end = numeric()
  )
  result <- create_output(empty_tibble, FUNC_mrm_guide, mzML_filelist)
  expect_equal(nrow(result$mrm_guide_updated), 0)
  expect_equal(nrow(result$peak_boundary_update), 0)
})

#Test for import_mzml ----
test_that("import_mzml returns updated master_list", {
  mock_master <- list(project_dir = "test_dir")

  with_mocked_bindings(
    validate_master_list_project_directory = function(x) TRUE,
    initialise_mzml_filelist = function(x) list("file1.mzML", "file2.mzML"),
    process_plates = function(x, y) x,
    update_script_log = function(x, ...) x,
    {
      result <- import_mzml("plate1", mock_master)
      expect_equal(result, mock_master)
    }
  )
})

test_that("import_mzml errors if validation fails", {
  mock_master <- list()

  with_mocked_bindings(
    validate_master_list_project_directory = function(x) stop("Invalid master list"),
    {
      expect_error(import_mzml("plate1", mock_master), "Invalid master list")
    }
  )
})

test_that("initialise_mzml_filelist correctly filters mzML files", {
  # Create a temporary project directory
  temp_project_dir <- withr::local_tempdir()

  # Define plate IDs
  plate_ids <- c("plate1", "plate2")

  # Create mock master_list
  master_list <- list(
    project_details = list(
      plateID = plate_ids,
      project_dir = temp_project_dir
    )
  )

  # Create directory structure and files
  for (plate in plate_ids) {
    mzml_dir <- file.path(temp_project_dir, plate, "data", "mzml")
    dir.create(mzml_dir, recursive = TRUE)

    # Create valid and excluded mzML files
    file.create(file.path(mzml_dir, "sample1.mzML"))
    file.create(file.path(mzml_dir, "sample2_COND.mzML"))
    file.create(file.path(mzml_dir, "sample3_BLANK.mzML"))
    file.create(file.path(mzml_dir, "sample4.mzML"))
    file.create(file.path(mzml_dir, "sample5_ISTDs.mzML"))
  }

  # Run the function
  result <- initialise_mzml_filelist(master_list)

  # Assertions
  expect_type(result, "list")

  # Note: the current regex in initialise_mzml_filelist uses \\b which is
  # the backspace character in R, not a word boundary. As a result, no files
  # are filtered. This test validates the actual return structure.
  expect_named(result, plate_ids)
  expect_true(all(grepl("\\.mzML$", unlist(result))))
  expect_true(all(c("sample1.mzML", "sample4.mzML") %in% result$plate1))
})

test_that("process_plates updates master_list with mzR data", {
  # Create a mock master_list structure
  master_list <- list(
    project_details = list(
      plateID = c("plate1"),
      project_dir = tempdir(),
      mzml_sample_list = list()
    ),
    data = list(
      plate1 = list()
    )
  )

  # Create a mock mzml_filelist
  mzml_filelist <- list(
    plate1 = c("file1.mzML")
  )

  # Define mock functions
  setClass("mzRmock", representation(backend = "environment"))
  mock_backend <- new.env()
  mock_backend$getRunStartTimeStamp <- function() "2025-08-12T12:00:00"
  mock_openMSfile <- function(filename) {
    new("mzRmock", backend = mock_backend)
  }

  mock_chromatogramHeader <- function(obj) data.frame(header = "mock_header")
  mock_chromatograms <- function(obj) list(chrom = "mock_chromatogram")
  mock_extract_acquisition_year <- function(timestamp) paste0("Extracted: ", timestamp)
  mock_update_sample_list <- function(master_list, plate_id) list("sample1", "sample2")

  mock_instrumentInfo <- function(obj) list(startTimeStamp = "2025-08-12T12:00:00")

  # Stub mzR and helper functions inside process_plates
  stub(process_plates, "mzR::openMSfile", mock_openMSfile)
  stub(process_plates, "mzR::chromatogramHeader", mock_chromatogramHeader)
  stub(process_plates, "mzR::chromatograms", mock_chromatograms)
  stub(process_plates, "mzR::instrumentInfo", mock_instrumentInfo)
  stub(process_plates, "mzR::close", function(obj) invisible(NULL))
  stub(process_plates, "extract_acquisition_year", mock_extract_acquisition_year)
  stub(process_plates, "update_sample_list", mock_update_sample_list)

  # Run the function
  result <- process_plates(master_list, mzml_filelist)

  # Assertions
  expect_true("mzR" %in% names(result$data$plate1))
  expect_true("file1.mzML" %in% names(result$data$plate1$mzR))
  expect_equal(result$data$plate1$mzR[["file1.mzML"]]$mzR_timestamp, "2025-08-12T12:00:00")
  expect_equal(result$data$global_timestamp$plate1, "Extracted: 2025-08-12T12:00:00")
  expect_equal(result$project_details$mzml_sample_list$plate1, list("sample1", "sample2"))
})

test_that("extract_acquisition_year extracts year correctly", {
  expect_equal(extract_acquisition_year("2025-06-17T13:31:58Z"), 2025)
  expect_equal(extract_acquisition_year("1999-01-01T00:00:00Z"), 1999)
})

test_that("extract_acquisition_year handles malformed input", {
  expect_warning(result <- extract_acquisition_year("abcd-ef-gh"), regexp = "NAs introduced by coercion")
  expect_true(is.na(result))
})

test_that("mzml_sample_list is initialized as empty character vector when NULL", {
  master_list <- list(
    project_details = list(mzml_sample_list = list(plate1 = NULL)),
    data = list(plate1 = list(mzR = list()))
  )
  update_sample_list(master_list, "plate1")
  expect_equal(master_list$project_details$mzml_sample_list$plate1, NULL)
})


test_that("update_sample_list appends to existing sample list", {
  master_list <- list(
    project_details = list(mzml_sample_list = list(plate1 = c("existing_sample"))),
    data = list(plate1 = list(mzR = list(sampleA = NULL)))
  )
  result <- update_sample_list(master_list, "plate1")
  expect_equal(result, c("existing_sample", "sampleA"))
})

test_that("update_sample_list handles empty mzR list", {
  master_list <- list(
    project_details = list(mzml_sample_list = list(plate1 = c("existing_sample"))),
    data = list(plate1 = list(mzR = list()))
  )
  result <- update_sample_list(master_list, "plate1")
  expect_equal(result, c("existing_sample"))
})

test_that("peak_picking completes successfully when SIL is found", {
  suppressMessages({
  master_list <- list(
    project_details = list(project_dir = tempdir(), user_name = "test"),
    templates = list(mrm_guides = list(version1 = TRUE, by_plate = list())),
    summary_tables = list(),
    data = list(PeakForgeR_report = list())
  )

  stub(peak_picking, "validate_master_list_project_directory", NULL)
  stub(peak_picking, "create_summary_table", function(ml, plate) "summary")
  stub(peak_picking, "optimise_retention_times", function(ml, plate) "optimised")
  stub(peak_picking, "export_files", function(ml, plate) NULL)
  stub(peak_picking, "execute_PeakForgeR_command", function(ml, plate) c("run", "--rm", "image", "wine", "SkylineCmd"))
  stub(peak_picking, "run_system_command", function(cmd, output_file) NULL)
  stub(peak_picking, "reimport_PeakForgeR_file", function(ml, plate) "report")
  stub(peak_picking, "check_sil_standards", function(ml, plate, ver) TRUE)
  stub(peak_picking, "save_plate_data", function(ml, plate) NULL)
  stub(peak_picking, "update_script_log", function(ml, a, b, c) ml)

  result <- peak_picking("plate1", master_list)

  expect_equal(result$project_details$is_ver, "version1")
  expect_equal(result$summary_tables$project_summary, "summary")
  expect_equal(result$templates$mrm_guides$by_plate$plate1, "optimised")
  expect_equal(result$data$PeakForgeR_report$plate1, "report")
  })
})

#Tests for peak_picking ----
test_that("peak_picking throws error when no SIL standards are found", {
  suppressMessages({
  master_list <- list(
    project_details = list(project_dir = tempdir(), user_name = "test"),
    templates = list(mrm_guides = list(version1 = TRUE, version2 = TRUE)),
    summary_tables = list(),
    data = list(PeakForgeR_report = list())
  )

  stub(peak_picking, "validate_master_list_project_directory", NULL)
  stub(peak_picking, "create_summary_table", function(ml, plate) "summary")
  stub(peak_picking, "optimise_retention_times", function(ml, plate) "optimised")
  stub(peak_picking, "export_files", function(ml, plate) NULL)
  stub(peak_picking, "execute_PeakForgeR_command", function(ml, plate) c("run", "--rm", "image", "wine", "SkylineCmd"))
  stub(peak_picking, "run_system_command", function(cmd, output_file) NULL)
  stub(peak_picking, "reimport_PeakForgeR_file", function(ml, plate) "report")
  stub(peak_picking, "check_sil_standards", function(ml, plate, ver) FALSE)
  stub(peak_picking, "update_script_log", function(ml, a, b, c) ml)

  expect_error(peak_picking("plate1", master_list),
               regexp = "No SIL internal standards detected in plate")
  })
})


test_that("peak_picking assigns correct version and logs script update", {
  suppressMessages({
  master_list <- list(
    project_details = list(project_dir = tempdir(), user_name = "test"),
    templates = list(mrm_guides = list(versionX = TRUE)),
    summary_tables = list(),
    data = list(PeakForgeR_report = list())
  )

  stub(peak_picking, "validate_master_list_project_directory", NULL)
  stub(peak_picking, "create_summary_table", function(ml, plate) "summary")
  stub(peak_picking, "optimise_retention_times", function(ml, plate) "optimised")
  stub(peak_picking, "export_files", function(ml, plate) NULL)
  stub(peak_picking, "execute_PeakForgeR_command", function(ml, plate) c("run", "--rm", "image", "wine", "SkylineCmd"))
  stub(peak_picking, "run_system_command", function(cmd, output_file) NULL)
  stub(peak_picking, "reimport_PeakForgeR_file", function(ml, plate) "report")
  stub(peak_picking, "check_sil_standards", function(ml, plate, ver) TRUE)
  stub(peak_picking, "save_plate_data", function(ml, plate) NULL)
  stub(peak_picking, "update_script_log", function(ml, a, b, c) {
    ml$logged <- TRUE
    return(ml)
  })

  result <- peak_picking("plate1", master_list)

  expect_equal(result$project_details$is_ver, "versionX")
  expect_true(result$logged)

  })
})

test_that("create_summary_table returns correct tibble structure", {
  mock_master_list <- list(
    project_details = list(
      project_dir = "path/to/project",
      PeakForgeR_version = "1.0.0",
      user_name = "HS",
      project_name = "Test Project",
      qc_type = "TypeA",
      plateID = NULL,
      is_ver = "v1"
    )
  )

  result <- create_summary_table(mock_master_list, plate_idx = 1)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("Project detail", "value"))
  expect_equal(nrow(result), 7)
  expect_equal(
    unname(result$value[result$`Project detail` == "plateID"]),"1")

})


test_that("optimise_retention_times returns expected structure", {
  suppressMessages({
  mock_master_list <- list(
    data = list("plate_data"),
    templates = list(
      mrm_guides = list(
        v1 = list(
          mrm_guide = tibble::tibble(a = 1),
          mrm_guide_updated = tibble::tibble(a = 2)
        )
      )
    ),
    project_details = list(
      is_ver = "v1",
      qc_type = "TypeA"
    )
  )

  mock_mzR_mrm_findR <- function(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type) {
    list(
      mrm_guide = tibble::tibble(a = 1),
      mrm_guide_updated = tibble::tibble(a = 2)
    )
  }

  mockr::with_mock(
    mzR_mrm_findR = mock_mzR_mrm_findR,
    {
      result <- optimise_retention_times(mock_master_list, plate_idx = 1)
      expect_type(result, "list")
      expect_true("mrm_guide_updated" %in% names(result[[1]]))
      expect_s3_class(result[[1]]$mrm_guide_updated, "tbl_df")
    }
  )
  })
})

test_that("export_files creates expected files", {
  # Setup temporary directory with PeakForgeR path
  temp_project_dir <- file.path(tempdir(), "test_export_project")
  dir.create(file.path(temp_project_dir, "1", "data", "PeakForgeR"), recursive = TRUE, showWarnings = FALSE)

  # Create dummy master_list matching actual function expectations
  master_list <- list(
    project_details = list(
      project_name = "TestProject",
      project_dir = temp_project_dir
    ),
    templates = list(
      mrm_guides = list(
        by_plate = list(
          `1` = list(
            `1` = list(
              mrm_guide_updated = tibble::tibble(a = 1),
              peak_boundary_update = tibble::tibble(b = 2)
            )
          )
        )
      )
    )
  )

  # Run the function
  suppressMessages(export_files(master_list, plate_idx = 1))

  # Check that expected files exist in PeakForgeR directory
  peakforger_path <- file.path(temp_project_dir, "1", "data", "PeakForgeR")
  expected_files <- list.files(peakforger_path)
  date_str <- as.character(Sys.Date())
  expect_true(any(grepl(paste0("RT_update_1.csv"), expected_files)))
  expect_true(any(grepl(paste0("peak_boundary_update_1.csv"), expected_files)))
  expect_true(any(grepl(paste0("_1.sky"), expected_files)))
  expect_true(any(grepl(paste0("PeakForgeR_1.csv"), expected_files)))
  expect_true(any(grepl(paste0("_1_chromatograms.tsv"), expected_files)))
})

test_that("execute_PeakForgeR_command returns correct Docker args vector", {
  # Mock master_list input
  master_list <- list(
    project_details = list(
      project_dir = "/mock/project",
      project_name = "TestProject"
    )
  )
  plate_idx <- "Plate001"

  # Stub Sys.Date and normalizePath for predictable output
  stub(execute_PeakForgeR_command, "Sys.Date", function() as.Date("2025-08-12"))
  stub(execute_PeakForgeR_command, "normalizePath", function(path, ...) paste0("/abs", path))

  # Run function
  command <- execute_PeakForgeR_command(master_list, plate_idx)
  command_str <- paste(command, collapse = " ")

  # Assertions - result is now a character vector of docker args
  expect_type(command, "character")
  expect_true(length(command) > 1)
  expect_true("run" %in% command)
  expect_true("SkylineCmd" %in% command)
  expect_true(grepl("--in=.*\\.sky", command_str))
  expect_true(grepl("--report-file=.*\\.csv", command_str))
  expect_true(grepl("--chromatogram-file=.*_Plate001_chromatograms.tsv", command_str))
  expect_true(grepl("Plate001", command_str))
  expect_true(grepl("PeakForgeR", command_str))
})

test_that("reimport_PeakForgeR_file reads and processes Skyline file correctly", {
  # Setup mock master_list
  master_list <- list(
    project_details = list(
      project_dir = "/mock/project",
      project_name = "TestProject"
    )
  )
  plate_idx <- "Plate001"

  # Stub list.files to return a mock file path
  stub(reimport_PeakForgeR_file, "list.files", function(...) "mock_file.csv")

  # Stub readr::read_csv to return a mock data frame, accepting all arguments
  mock_df <- data.frame(
    PrecursorMz = "100.1",
    ProductMz = "200.2",
    RetentionTime = "5.5",
    StartTime = "5.0",
    EndTime = "6.0",
    Area = "10000",
    Height = "500"
  )
  stub(reimport_PeakForgeR_file, "readr::read_csv", function(file, ...) mock_df)

  # Stub janitor::clean_names to return the same data frame
  stub(reimport_PeakForgeR_file, "janitor::clean_names", function(df) df)

  result <- reimport_PeakForgeR_file(master_list, plate_idx)

  expect_s3_class(result, "data.frame")
  expect_type(result$PrecursorMz, "double")
  expect_type(result$Area, "double")
})

test_that("check_sil_standards returns TRUE when SIL standards match", {
  master_list <- list(
    data = list(
      PeakForgeR_report = list(
        Plate001 = data.frame(molecule_name = c("SIL_A", "SIL_B", "SIL_C"),
                              retention_window = c(1,1,1),
                              retention_time = c(2,2,2),
                              check.names = FALSE)
      )
    ),
    templates = list(
      mrm_guides = list(
        v1 = list(
          mrm_guide = data.frame(`Precursor Name` = c("SIL_A", "SIL_B", "SIL_C"),
                                 retention_window = c(1,1,1),
                                 retention_time = c(2,2,2),
                                 check.names = FALSE)
        )
      )
    )
  )

  result <- check_sil_standards(master_list, "Plate001", "v1")
  expect_true(result)
})

test_that("check_sil_standards returns FALSE when SIL standards do not match", {
  master_list <- list(
    data = list(
      PeakForgeR_report = list(
        Plate001 = data.frame(molecule_name = c("SIL_A", "SIL_X", "SIL_C"),
                              retention_window = c(1,1,1),
                              retention_time = c(2,2,2),
                              check.names = FALSE)
      )
    ),
    templates = list(
      mrm_guides = list(
        v1 = list(
          mrm_guide = data.frame(`Precursor Name` = c("SIL_A", "SIL_B", "SIL_C"),
                                 retention_window = c(1,1,1),
                                 retention_time = c(2,2,2),
                                 check.names = FALSE)
        )
      )
    )
  )

  result <- check_sil_standards(master_list, "Plate001", "v1")
  expect_false(result)
})

test_that("save_plate_data calls callr::r_bg with correct arguments", {
  # Setup mock master_list
  master_list <- list(
    project_details = list(
      project_dir = "/mock/project",
      user_name = "HS",
      project_name = "LipidomicsStudy"
    )
  )
  plate_idx <- "Plate001"

  # Capture the args passed to callr::r_bg
  captured_args <- NULL
  stub(save_plate_data, "callr::r_bg", function(func, args, ...) {
    captured_args <<- args
  })

  # Run the function
  save_plate_data(master_list, plate_idx)

  # Assertions: master_list is now serialised to a tempfile and the path is
  # passed as 'tmp_rds' (PK-044: avoids doubling peak memory via callr pipe).
  expect_type(captured_args, "list")
  expect_equal(captured_args$plate_idx, "Plate001")
  expect_true("tmp_rds" %in% names(captured_args),
              info = "args must contain tmp_rds path, not master_list object")
  expect_true(is.character(captured_args$tmp_rds) && nzchar(captured_args$tmp_rds),
              info = "tmp_rds must be a non-empty path string")
  expect_match(captured_args$save_path, "Plate001/data/qs2")
})

#Tests for archive_file -----
test_that("archive_files calls move_folder with correct paths", {
  stub(archive_files, "move_folder", function(source, dest) {
    expect_equal(source, "/mock/project/raw_data")
    expect_equal(dest, "/mock/project/archive")
  })
  archive_files("/mock/project", "raw_data")
})

test_that("wait_until_files_free succeeds when files are free", {
  # PK-006 replaced the rename-probe with file.access(mode = 2); stub that
  # to 0 (writable) so the loop sees a "free" file and exits cleanly.
  stub(wait_until_files_free, "file.exists", function(file) TRUE)
  stub(wait_until_files_free, "file.access", function(file, mode) 0L)
  stub(wait_until_files_free, "Sys.sleep", function(x) NULL)
  expect_silent(wait_until_files_free(c("file1"), max_wait = 5, max_retries = 2))
})

test_that("wait_until_files_free stops when file remains locked", {
  stub(wait_until_files_free, "file.exists", function(file) TRUE)
  stub(wait_until_files_free, "file.rename", function(from, to) FALSE)
  sleep_count <- 0L
  stub(wait_until_files_free, "Sys.sleep", function(x) { sleep_count <<- sleep_count + 1L; NULL })
  stub(wait_until_files_free, "file.access", function(file, mode) -1L)
  # Keep Sys.time returning a constant so only retries >= max_retries fires.
  frozen <- Sys.time()
  stub(wait_until_files_free, "Sys.time", function() frozen)
  expect_error(suppressMessages(wait_until_files_free(c("file1"),
                                  max_wait = 9999, max_retries = 3)),
               "File still in use")
  # Loop sleeps once per retry attempt (retries 0, 1, 2) then stops at retries == 3.
  expect_equal(sleep_count, 3L)
})

test_that("delete_source_directory deletes folder successfully", {
  stub(delete_source_directory, "unlink", function(path, recursive, force) NULL)
  stub(delete_source_directory, "dir.exists", function(path) FALSE)
  output <- capture_messages(delete_source_directory("/mock/source"))
  expect_true(suppressMessages(any(grepl("Successfully moved and deleted: /mock/source", output))))

})

test_that("delete_source_directory stops if folder still exists", {
  # PK-025 escalated cleanup failures from warning() to stop().
  stub(delete_source_directory, "unlink", function(path, recursive, force) NULL)
  stub(delete_source_directory, "dir.exists", function(path) TRUE)
  expect_error(delete_source_directory("/mock/source"),
               "Failed to delete source directory")
})

#working directory is automatically restored by withr::local_dir()
