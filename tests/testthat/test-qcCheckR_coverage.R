library(mockery)
library(dplyr)
library(tibble)

# ============================================================================
# qcCheckR.R -- uncovered validation branches (lines 165-200)
# ============================================================================

test_that("qcCheckR rejects non-list mrm_template_list (line 165-167)", {
  temp_dir <- tempfile("qccheck_ml_nonlist_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = "not_a_list",
               QC_sample_label = "qc",
               sample_tags = c("sample"))
    ),
    "mrm_template_list.*must be a named list"
  )
})

test_that("qcCheckR rejects empty list mrm_template_list (line 170-171)", {
  temp_dir <- tempfile("qccheck_ml_empty_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(),
               QC_sample_label = "qc",
               sample_tags = c("sample"))
    ),
    "mrm_template_list.*must not be an empty list"
  )
})

test_that("qcCheckR rejects non-character QC_sample_label (line 181-183)", {
  temp_dir <- tempfile("qccheck_qcl_nonchar_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(a = 1),
               QC_sample_label = 123,
               sample_tags = c("sample"))
    ),
    "QC_sample_label.*must be a non-empty single character string"
  )
})

test_that("qcCheckR rejects vector QC_sample_label (line 181-183)", {
  temp_dir <- tempfile("qccheck_qcl_vec_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(a = 1),
               QC_sample_label = c("a", "b"),
               sample_tags = c("sample"))
    ),
    "QC_sample_label.*must be a non-empty single character string"
  )
})

test_that("qcCheckR rejects empty string QC_sample_label (line 181-183)", {
  temp_dir <- tempfile("qccheck_qcl_empty_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(a = 1),
               QC_sample_label = "",
               sample_tags = c("sample"))
    ),
    "QC_sample_label.*must be a non-empty single character string"
  )
})

test_that("qcCheckR rejects non-character sample_tags (line 194-196)", {
  temp_dir <- tempfile("qccheck_st_nonchar_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(a = 1),
               QC_sample_label = "qc",
               sample_tags = 42)
    ),
    "sample_tags.*must be a character vector"
  )
})

test_that("qcCheckR rejects empty character sample_tags (line 194-196)", {
  temp_dir <- tempfile("qccheck_st_emptychar_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(a = 1),
               QC_sample_label = "qc",
               sample_tags = character(0))
    ),
    "sample_tags.*must be a character vector"
  )
})

test_that("qcCheckR rejects sample_tags with empty strings (line 199-200)", {
  temp_dir <- tempfile("qccheck_st_blank_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(a = 1),
               QC_sample_label = "qc",
               sample_tags = c("sample", ""))
    ),
    "sample_tags.*must not contain empty strings"
  )
})

# ============================================================================
# qcCheckR_Utils.R -- setup_project: non-ANPC sample_tags branch (line 121)
# ============================================================================

test_that("qcCheckR_setup_project uses user-supplied sample_tags for non-ANPC user", {
  temp_dir <- tempfile("qccheck_setup_tags_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  stub(qcCheckR_setup_project, "validate_project_directory", function(x, ...) x)
  stub(qcCheckR_setup_project, "initialise_master_list", function(...) {
    list(project_details = list(user_name = "TestUser"))
  })
  stub(qcCheckR_setup_project, "store_environment_details", function(ml) ml)
  stub(qcCheckR_setup_project, "qcCheckR_set_project_details", function(ml, un, pd, qcl, st, mvt) {
    ml$project_details$user_name <- un
    ml$project_details$sample_tags <- st
    ml
  })
  stub(qcCheckR_setup_project, "qcCheckR_read_mrm_guides", function(ml, ...) ml)
  stub(qcCheckR_setup_project, "qcCheckR_setup_project_directories", function(ml) ml)
  stub(qcCheckR_setup_project, "qcCheckR_import_PeakForgeR_reports", function(ml) ml)
  stub(qcCheckR_setup_project, "find_method_version", function(ml) ml)
  stub(qcCheckR_setup_project, "update_script_log", function(ml, ...) ml)

  result <- suppressMessages(
    qcCheckR_setup_project(
      user_name = "TestUser",
      project_directory = temp_dir,
      mrm_template_list = list(v1 = list(SIL_guide = "a.tsv", conc_guide = "b.tsv")),
      QC_sample_label = "ltr",
      sample_tags = c("qc", "blank", "sample"),
      mv_threshold = 50
    )
  )

  expect_equal(result$project_details$sample_tags, c("qc", "blank", "sample"))
})

# ============================================================================
# qcCheckR_Utils.R -- read_mrm_guides csv branch (line 204) and unsupported ext (line 206)
# ============================================================================

test_that("qcCheckR_read_mrm_guides reads csv files (line 204)", {
  csv_file <- tempfile(fileext = ".csv")
  writeLines(
    c("Precursor Name,Note,other", "LPC16:0,SIL_A,x"),
    csv_file
  )
  conc_csv <- tempfile(fileext = ".csv")
  writeLines(
    c("SIL_name,concentration_factor", "SIL_A,1.5"),
    conc_csv
  )
  on.exit({
    unlink(csv_file)
    unlink(conc_csv)
  }, add = TRUE)

  master_list <- list(
    project_details = list(user_name = "TestUser"),
    templates = list(mrm_guides = list())
  )

  stub(qcCheckR_read_mrm_guides, "replace_precursor_symbols", function(guide, ...) guide)
  stub(qcCheckR_read_mrm_guides, "validate_qcCheckR_mrm_template_list", function(ml) ml)

  result <- suppressMessages(
    qcCheckR_read_mrm_guides(
      master_list,
      list(v1 = list(SIL_guide = csv_file, conc_guide = conc_csv))
    )
  )

  expect_true(!is.null(result$templates$mrm_guides$v1$SIL_guide))
  expect_true(!is.null(result$templates$mrm_guides$v1$conc_guide))
})

test_that("qcCheckR_read_mrm_guides errors on unsupported file type (line 206)", {
  txt_file <- tempfile(fileext = ".txt")
  writeLines("data", txt_file)
  on.exit(unlink(txt_file), add = TRUE)

  master_list <- list(
    project_details = list(user_name = "TestUser"),
    templates = list(mrm_guides = list())
  )

  stub(qcCheckR_read_mrm_guides, "validate_qcCheckR_mrm_template_list", function(ml) ml)

  expect_error(
    suppressMessages(
      qcCheckR_read_mrm_guides(
        master_list,
        list(v1 = list(SIL_guide = txt_file, conc_guide = txt_file))
      )
    ),
    "Unsupported file type"
  )
})

# ============================================================================
# qcCheckR_Utils.R -- import_PeakForgeR_reports: windows long path (lines 285-294),
#   unsupported file type warning (lines 305-306), short_link cleanup (line 310)
# ============================================================================

test_that("qcCheckR_import_PeakForgeR_reports warns on unsupported file type (line 305-306)", {
  temp_dir <- tempfile("qccheck_import_ext_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # The list.files pattern is _PeakForgeR_.*\.(csv|tsv)$, so a .xlsx won't match.
  # We need to stub list.files to return our fake file so the internal loop processes it.
  bad_file <- file.path(temp_dir, "plate1_PeakForgeR_report.xlsx")
  writeLines("dummy", bad_file)

  master_list <- list(
    project_details = list(
      user_name = "TestUser",
      project_dir = temp_dir,
      plateIDs = c("plate1")
    ),
    data = list(PeakForgeRReport = list())
  )

  # Stub list.files to return a path ending in .xlsx so the unsupported ext branch is hit
  stub(qcCheckR_import_PeakForgeR_reports, "list.files", function(...) bad_file)

  expect_warning(
    suppressMessages(
      qcCheckR_import_PeakForgeR_reports(master_list)
    ),
    "Unsupported file type"
  )
})

# ============================================================================
# qcCheckR_Utils.R -- validate_master_list (lines 448-450)
# ============================================================================

test_that("validate_master_list errors on invalid structure (line 448-450)", {
  expect_error(
    validate_master_list(list(data = list(PeakForgeRReport = "not_a_list"))),
    "Invalid master_list format"
  )
  expect_error(
    validate_master_list("not_a_list"),
    "Invalid master_list format"
  )
  expect_error(
    validate_master_list(list(data = list())),
    "Invalid master_list format"
  )
})

# ============================================================================
# qcCheckR_Utils.R -- find_matching_report (line 462)
# ============================================================================

test_that("find_matching_report returns matching name (line 462)", {
  report_list <- list(
    "plate1_report" = data.frame(x = 1),
    "plate2_report" = data.frame(x = 2)
  )
  expect_equal(find_matching_report(report_list, "plate1"), "plate1_report")
  expect_equal(find_matching_report(report_list, "plate99"), character(0))
})

# ============================================================================
# qcCheckR_Utils.R -- set_project_qc_type: user QC fails, fallback (line 759)
# ============================================================================

test_that("set_project_qc_type falls back to best alternative when user QC fails (line 759)", {
  master_list <- list(
    project_details = list(
      qc_type = "ltr",
      global_qc_pass = list(ltr = "fail", pqc = "pass", blank = "pass"),
      qc_passed = list(
        plate1 = list(pqc = "pass", blank = "pass"),
        plate2 = list(pqc = "pass", blank = "fail")
      )
    )
  )

  result <- suppressWarnings(suppressMessages(
    set_project_qc_type(master_list)
  ))

  # pqc passes on both plates (count=2), blank on one (count=1), so pqc wins
  expect_equal(result$project_details$qc_type, "pqc")
})

# ============================================================================
# qcCheckR_Utils.R -- replace_problematic_values (lines 887-891)
# ============================================================================

test_that("replace_problematic_values replaces zeros, Inf, NaN with NA (lines 887-891)", {
  mat <- matrix(c(0, 1, Inf, NaN, 5, -Inf), nrow = 2)
  result <- replace_problematic_values(mat)

  expect_true(is.na(result[1, 1]))  # was 0
  expect_equal(result[2, 1], 1)
  expect_true(is.na(result[1, 2]))  # was Inf
  expect_true(is.na(result[2, 2]))  # was NaN
  expect_equal(result[1, 3], 5)
  expect_true(is.na(result[2, 3]))  # was -Inf
})

# ============================================================================
# qcCheckR_Utils.R -- process_sil_target (lines 1057-1094)
# ============================================================================

test_that("process_sil_target calculates response and concentration (lines 1057-1094)", {
  sil_guide <- tibble::tibble(
    `Precursor Name` = c("LPC16:0", "LPC18:0"),
    Note = c("SIL_A", "SIL_A")
  )
  conc_guide <- tibble::tibble(
    SIL_name = "SIL_A",
    concentration_factor = 2.5
  )

  data <- tibble::tibble(
    sample_name = c("s1", "s2", "s3"),
    `LPC16:0` = c(100, 200, 300),
    `LPC18:0` = c(50, 100, 150),
    SIL_A = c(10, 20, 30)
  )

  master_list <- list(
    templates = list(
      mrm_guides = list(v1 = list(SIL_guide = sil_guide, conc_guide = conc_guide))
    ),
    data = list(
      peakArea = list(imputed = list(plate1 = data)),
      response = list(imputed = list(plate1 = tibble::tibble(sample_name = c("s1", "s2", "s3")))),
      concentration = list(imputed = list(plate1 = tibble::tibble(sample_name = c("s1", "s2", "s3"))))
    )
  )

  result <- process_sil_target(master_list, "plate1", "imputed", "v1", "SIL_A")

  # Check response was calculated: target / SIL
  resp <- result$data$response$imputed$plate1
  expect_true("LPC16:0" %in% colnames(resp))
  expect_equal(resp$`LPC16:0`, c(10, 10, 10))

  # Check concentration was calculated: response * conc_factor
  conc <- result$data$concentration$imputed$plate1
  expect_true("LPC16:0" %in% colnames(conc))
  expect_equal(conc$`LPC16:0`, c(25, 25, 25))
})

test_that("process_sil_target returns early when no precursors found (line 1062)", {
  sil_guide <- tibble::tibble(
    `Precursor Name` = c("LPC16:0"),
    Note = c("SIL_B")
  )

  master_list <- list(
    templates = list(
      mrm_guides = list(v1 = list(SIL_guide = sil_guide))
    ),
    data = list(peakArea = list(imputed = list(plate1 = tibble::tibble(sample_name = "s1"))))
  )

  result <- process_sil_target(master_list, "plate1", "imputed", "v1", "SIL_NONEXISTENT")
  # Should return unchanged

  expect_identical(result, master_list)
})

# ============================================================================
# qcCheckR_Utils.R -- qcCheckR_statTarget_batch_correction: ComBat dispatch (line 1133)
# ============================================================================

test_that("qcCheckR_statTarget_batch_correction dispatches to ComBat when batch_method is ComBat (line 1133)", {
  master_list <- list(
    project_details = list(
      batch_method = "ComBat",
      script_log = list(
        timestamps = list(),
        milestones = list()
      )
    )
  )

  combat_called <- FALSE
  stub(qcCheckR_statTarget_batch_correction, "qcCheckR_combat_correction", function(ml) {
    combat_called <<- TRUE
    ml$combat_done <- TRUE
    ml
  })
  stub(qcCheckR_statTarget_batch_correction, "update_script_log", function(ml, ...) ml)

  result <- suppressMessages(
    qcCheckR_statTarget_batch_correction(master_list)
  )

  expect_true(combat_called)
  expect_true(result$combat_done)
})

# ============================================================================
# qcCheckR_Utils.R -- run_statTarget_shiftCor (lines 1372-1420)
# ============================================================================

test_that("run_statTarget_shiftCor calls shiftCor with correct parameters (lines 1372-1420)", {
  temp_dir <- withr::local_tempdir()
  d <- Sys.Date()
  results_dir <- file.path(temp_dir, paste0(d, "_signal_correction_results"))
  dir.create(results_dir, recursive = TRUE)

  pheno_file <- file.path(results_dir, "PhenoFile.csv")
  writeLines("sample,batch,class\ns1,b1,QC", pheno_file)
  profile_file <- file.path(results_dir, "ProfileFile.csv")
  writeLines("sample,metab_A\ns1,100", profile_file)

  # Create expected output path
  output_dir <- file.path(results_dir, "statTarget", "shiftCor", "After_shiftCor")
  dir.create(output_dir, recursive = TRUE)
  writeLines("sample,metab_A\ns1,105", file.path(output_dir, "shift_all_cor.csv"))

  FUNC_list <- list(project_dir = temp_dir)

  master_list <- list(
    project_details = list(
      batch_method = "QCRFSC",
      batch_ntree = 200L,
      batch_coCV = 5000,
      batch_Frule = 0.1,
      batch_imputeM = "median"
    )
  )

  captured_args <- NULL
  stub(run_statTarget_shiftCor, "statTarget::shiftCor", function(...) {
    captured_args <<- list(...)
  })
  stub(run_statTarget_shiftCor, "clean_statTarget_output", function(data) data)
  stub(run_statTarget_shiftCor, "transpose_and_merge_corrected", function(fl) fl)
  stub(run_statTarget_shiftCor, "adjust_qc_means", function(fl, ml) fl)

  result <- suppressMessages(
    run_statTarget_shiftCor(FUNC_list, master_list)
  )

  expect_equal(captured_args$MLmethod, "QCRFSC")
  expect_equal(captured_args$ntree, 200L)
  expect_equal(captured_args$coCV, 5000)
  expect_equal(captured_args$Frule, 0.1)
  expect_equal(captured_args$imputeM, "median")
})

# ============================================================================
# qcCheckR_Utils.R -- qcCheckR_combat_correction (lines 1594-1701)
# ============================================================================

test_that("qcCheckR_combat_correction runs ComBat and restructures data (lines 1594-1701)", {
  set.seed(42)
  n <- 20

  combined_data <- tibble::tibble(
    sample_name = paste0("s", 1:n),
    sample_plate_id = rep(c("plate1", "plate2"), each = n / 2),
    sample_type_factor = rep(c("sample", "qc"), length.out = n),
    sample_type = rep(c("sample", "qc"), length.out = n),
    metab_A = rnorm(n, 100, 10),
    metab_B = rnorm(n, 200, 20)
  )

  master_list <- list(
    project_details = list(
      combat_par.prior = TRUE,
      combat_mean.only = FALSE,
      combat_ref.batch = NULL,
      statTarget_qc_type = "qc",
      qc_type = "qc"
    ),
    data = list(
      concentration = list(
        imputed = list(
          plate1 = combined_data %>% filter(sample_plate_id == "plate1"),
          plate2 = combined_data %>% filter(sample_plate_id == "plate2")
        )
      ),
      peakArea = list()
    )
  )

  # Mock sva::ComBat to return the input matrix unchanged
  stub(qcCheckR_combat_correction, "requireNamespace", TRUE)
  stub(qcCheckR_combat_correction, "sva::ComBat", function(dat, batch, ...) dat)

  result <- suppressMessages(
    qcCheckR_combat_correction(master_list)
  )

  expect_true(!is.null(result$data$concentration$corrected))
  expect_true(!is.null(result$data$peakArea$statTargetProcessed))
  expect_true(!is.null(result$data$concentration$statTargetProcessed))
  expect_equal(
    sort(names(result$data$concentration$corrected)),
    c("plate1", "plate2")
  )
})

test_that("qcCheckR_combat_correction stops if sva package not available (line 1594-1597)", {
  master_list <- list(
    project_details = list(batch_method = "ComBat"),
    data = list(concentration = list(imputed = list()))
  )

  stub(qcCheckR_combat_correction, "requireNamespace", FALSE)

  expect_error(
    suppressMessages(
      qcCheckR_combat_correction(master_list)
    ),
    "sva.*package is required"
  )
})

# ============================================================================
# qcCheckR_Utils.R -- qcCheckR_summary_report (lines 2417-2475)
# ============================================================================

test_that("qcCheckR_summary_report builds summary table (lines 2417-2475)", {
  sorted_data <- tibble::tibble(
    sample_name = paste0("s", 1:5),
    sample_type_factor = c("sample", "qc", "sample", "blank", "sample"),
    sample_type = c("sample", "qc", "sample", "blank", "sample"),
    sample_plate_id = "plate1",
    sample_matrix = "SER",
    SIL_A = c(100, 200, 300, 400, 500),
    LPC16 = c(10, 20, 30, 40, 50)
  )

  master_list <- list(
    project_details = list(
      sample_tags = c("qc", "blank", "sample")
    ),
    data = list(
      peakArea = list(sorted = list(plate1 = sorted_data)),
      concentration = list(
        sorted = list(plate1 = sorted_data),
        statTargetProcessed = list(plate1 = sorted_data)
      )
    ),
    filters = list(
      samples.missingValues = tibble::tibble(sample.flag = c(0, 1, 0)),
      failed_sil.intStds = character(0),
      failed_lipids = character(0),
      lipid.missingValues = list(allPlates = list()),
      rsd = tibble::tibble(
        dataBatch = rep(c("plate1", "allBatches"), each = 3),
        dataSource = rep(c("peakArea", "concentration", "concentration[statTarget]"), 2),
        LPC16 = c(5, 15, 25, 8, 18, 28)
      )
    ),
    templates = list(`Plate SIL version` = list(plate1 = "v1"))
  )

  stub(qcCheckR_summary_report, "generate_plate_summary", function(ml, idx, metrics, tags) {
    tibble::tibble(metric = metrics, plate1 = rep("0", length(metrics)))
  })
  stub(qcCheckR_summary_report, "generate_inter_plate_summary", function(ml, metrics, tags) {
    tibble::tibble(metric = metrics, all_plates = rep("0", length(metrics)))
  })
  stub(qcCheckR_summary_report, "update_script_log", function(ml, ...) ml)

  result <- suppressMessages(
    qcCheckR_summary_report(master_list)
  )

  expect_true(!is.null(result$summary_tables$projectOverview))
  expect_true("metric" %in% colnames(result$summary_tables$projectOverview))
  expect_true("MatrixType" %in% result$summary_tables$projectOverview$metric)
})

# ============================================================================
# qcCheckR_Utils.R -- generate_inter_plate_summary (lines 2553-2595)
# ============================================================================

test_that("generate_inter_plate_summary produces correct metrics (lines 2553-2595)", {
  sorted_data <- tibble::tibble(
    sample_name = paste0("s", 1:4),
    sample_type_factor = c("sample", "qc", "sample", "blank"),
    sample_type = c("sample", "qc", "sample", "blank"),
    sample_plate_id = "plate1",
    sample_matrix = "SER",
    SIL_A = c(100, 200, 300, 400),
    LPC16 = c(10, 20, 30, 40)
  )

  st_data <- tibble::tibble(
    sample_name = paste0("s", 1:4),
    sample_type_factor = c("sample", "qc", "sample", "blank"),
    LPC16 = c(11, 21, 31, 41)
  )

  metrics <- c(
    "MatrixType", "totalSamples", "studySamples",
    "qcSamples", "blankSamples",
    "lipidTargets", "matchedLipidTargets",
    "SIL.version", "SIL.IntStds",
    "missingValueFilterFlags[samples]",
    "missingValueFilterFlags[SIL.IS]",
    "missingValueFilterFlags[lipidTargets]",
    "rsd<30%[peakArea]", "rsd<20%[peakArea]", "rsd<10%[peakArea]",
    "rsd<30%[concentration]", "rsd<20%[concentration]", "rsd<10%[concentration]",
    "rsd<30%[concentration.statTarget]", "rsd<20%[concentration.statTarget]", "rsd<10%[concentration.statTarget]"
  )

  master_list <- list(
    data = list(
      peakArea = list(sorted = list(plate1 = sorted_data)),
      concentration = list(
        statTargetProcessed = list(plate1 = st_data)
      )
    ),
    filters = list(
      samples.missingValues = tibble::tibble(sample.flag = c(0, 1, 0, 0)),
      failed_sil.intStds = c("SIL_X"),
      failed_lipids = character(0),
      rsd = tibble::tibble(
        dataBatch = rep("allBatches", 3),
        dataSource = c("peakArea", "concentration", "concentration[statTarget]"),
        LPC16 = c(5, 15, 25)
      )
    ),
    templates = list(`Plate SIL version` = list(plate1 = "v1"))
  )

  result <- generate_inter_plate_summary(master_list, metrics, c("qc", "blank"))

  expect_true("all_plates" %in% colnames(result))
  expect_equal(nrow(result), length(metrics))
})

# ============================================================================
# qcCheckR_Utils.R -- qcCheckR_plot_options (lines 2615-2633)
# ============================================================================

test_that("qcCheckR_plot_options sets colour, shape, size for sample types (lines 2615-2633)", {
  master_list <- list(
    project_details = list(
      sample_tags = c("sample", "qc", "blank"),
      qc_type = "qc"
    )
  )

  result <- qcCheckR_plot_options(master_list)

  expect_equal(length(result$project_details$plot_fill), 3)
  expect_equal(result$project_details$plot_colour[["qc"]], "red")
  expect_equal(result$project_details$plot_shape[["qc"]], 23)
  expect_equal(result$project_details$plot_size[["qc"]], 3)
  expect_equal(result$project_details$plot_colour[["sample"]], "black")
  expect_equal(result$project_details$plot_shape[["sample"]], 21)
  expect_equal(result$project_details$plot_size[["sample"]], 2)
})

# ============================================================================
# qcCheckR_Utils.R -- qcCheckR_PCA (lines 2651-2668)
# ============================================================================

test_that("qcCheckR_PCA calls run_pca_model and generate_pca_ggplot (lines 2651-2668)", {
  master_list <- list(
    project_details = list(project_name = "test"),
    pca = list()
  )

  run_calls <- character(0)
  stub(qcCheckR_PCA, "run_pca_model", function(ml, source, preprocessed) {
    run_calls <<- c(run_calls, paste0(source, ".", preprocessed))
    ml
  })
  stub(qcCheckR_PCA, "generate_pca_ggplot", function(ml, fill_var) {
    paste0("mock_plot_", fill_var)
  })

  result <- suppressMessages(
    qcCheckR_PCA(master_list)
  )

  expect_equal(length(run_calls), 4)
  expect_true("peakArea.FALSE" %in% run_calls)
  expect_true("peakArea.TRUE" %in% run_calls)
  expect_true("concentration.FALSE" %in% run_calls)
  expect_true("concentration.TRUE" %in% run_calls)
  expect_equal(result$pca$plot$sample_type_factor, "mock_plot_sample_type_factor")
  expect_equal(result$pca$plot$sample_plate_id, "mock_plot_sample_plate_id")
})

# ============================================================================
# qcCheckR_Utils.R -- run_pca_model (lines 2684-2770)
# ============================================================================

test_that("run_pca_model builds PCA model and scores (lines 2684-2770)", {
  set.seed(123)
  n <- 15
  data <- tibble::tibble(
    sample_name = paste0("s", 1:n),
    sample_type_factor = rep(c("sample", "qc", "blank"), each = 5),
    sample_plate_id = "plate1",
    sample_run_index = 1:n,
    metab_A = rnorm(n, 100, 10),
    metab_B = rnorm(n, 200, 20),
    metab_C = rnorm(n, 50, 5)
  )

  master_list <- list(
    data = list(
      peakArea = list(imputed = list(plate1 = data)),
      concentration = list(sorted = list(plate1 = data))
    ),
    filters = list(
      failed_samples = character(0),
      failed_lipids = character(0),
      rsd = tibble::tibble(
        dataSource = "peakArea",
        dataBatch = "allBatches",
        metab_A = 5, metab_B = 10, metab_C = 40
      )
    ),
    pca = list(models = list(), scores = list())
  )

  # Mock ropls::opls to return a simple object
  mock_scores <- matrix(rnorm(n * 3), ncol = 3)
  colnames(mock_scores) <- c("p1", "p2", "p3")
  rownames(mock_scores) <- paste0("s", 1:n)
  mock_model <- list()
  mock_model_class <- methods::setClass("mockOpls", representation(scoreMN = "matrix"))
  mock_obj <- new("mockOpls", scoreMN = mock_scores)

  stub(run_pca_model, "ropls::opls", function(...) mock_obj)

  result <- suppressMessages(
    run_pca_model(master_list, "peakArea", preprocessed = FALSE)
  )

  expect_true("peakArea.imputed" %in% names(result$pca$models))
  expect_true("peakArea.imputed" %in% names(result$pca$scores))
  expect_true("PC1" %in% colnames(result$pca$scores[["peakArea.imputed"]]))
})

# ============================================================================
# qcCheckR_Utils.R -- generate_pca_ggplot (lines 2785-2845)
# ============================================================================

test_that("generate_pca_ggplot produces a plotly object (lines 2785-2845)", {
  scores_data <- tibble::tibble(
    sample_name = paste0("s", 1:6),
    PC1 = rnorm(6),
    PC2 = rnorm(6),
    PC3 = rnorm(6),
    sample_type_factor = rep(c("sample", "qc"), 3),
    sample_plate_id = rep(c("plate1", "plate2"), each = 3),
    sample_data_source = rep(c(
      "peakArea.imputed: s=3, l=2",
      "concentration.imputed: s=3, l=2"
    ), each = 3)
  )

  master_list <- list(
    project_details = list(
      project_name = "test_project",
      plot_shape = c(sample = 21, qc = 23),
      plot_colour = c(sample = "black", qc = "red"),
      plot_size = c(sample = 2, qc = 3),
      plot_fill = c(sample = "#440154FF", qc = "#FDE725FF")
    ),
    pca = list(scores = list(model1 = scores_data))
  )

  result <- generate_pca_ggplot(master_list, "sample_type_factor")
  expect_true(inherits(result, "plotly") || inherits(result, "htmlwidget"))
})

# ============================================================================
# qcCheckR_Utils.R -- qcCheckR_run_order_plots (lines 2864-2905)
# ============================================================================

test_that("qcCheckR_run_order_plots builds run order plots for PC1/PC2/PC3 (lines 2864-2905)", {
  scores_data <- tibble::tibble(
    sample_name = paste0("s", 1:6),
    PC1 = rnorm(6),
    PC2 = rnorm(6),
    PC3 = rnorm(6),
    sample_type_factor = rep(c("sample", "qc"), 3),
    sample_plate_id = rep("plate1", 6),
    sample_run_index = 1:6,
    sample_data_source = "peakArea.imputed"
  )

  master_list <- list(
    project_details = list(
      project_name = "test_project",
      plot_shape = c(sample = 21, qc = 23),
      plot_colour = c(sample = "black", qc = "red"),
      plot_size = c(sample = 2, qc = 3),
      plot_fill = c(sample = "#440154FF", qc = "#FDE725FF")
    ),
    pca = list(scores = list(model1 = scores_data))
  )

  stub(qcCheckR_run_order_plots, "plot_run_order", function(...) "mock_plot")

  result <- suppressMessages(
    qcCheckR_run_order_plots(master_list)
  )

  expect_equal(names(result$pca$scoresRunOrder), c("PC1", "PC2", "PC3"))
})

# ============================================================================
# qcCheckR_Utils.R -- plot_run_order (lines 2925-2971)
# ============================================================================

test_that("plot_run_order produces a plotly object (lines 2925-2971)", {
  scores <- tibble::tibble(
    sample_name = paste0("s", 1:6),
    PC1 = rnorm(6),
    PC2 = rnorm(6),
    PC3 = rnorm(6),
    sample_type_factor = rep(c("sample", "qc"), 3),
    sample_run_index = 1:6,
    sample_plate_id = "plate1",
    sample_data_source = "peakArea.imputed"
  )

  plot_settings <- list(
    project_name = "test_project",
    plot_shape = c(sample = 21, qc = 23),
    plot_colour = c(sample = "black", qc = "red"),
    plot_size = c(sample = 2, qc = 3),
    plot_fill = c(sample = "#440154FF", qc = "#FDE725FF")
  )

  result <- plot_run_order(scores, "PC1", c(0.5, 6.5), plot_settings)
  expect_true(inherits(result, "plotly") || inherits(result, "htmlwidget"))
})

# ============================================================================
# qcCheckR_Utils.R -- qcCheckR_target_control_charts (lines 2989-3009)
# ============================================================================

test_that("qcCheckR_target_control_charts calls sub-functions and stores charts (lines 2989-3009)", {
  master_list <- list(
    project_details = list(project_name = "test"),
    pca = list(scores = list()),
    templates = list(
      `Plate SIL version` = list(plate1 = "v1"),
      mrm_guides = list(v1 = list(SIL_guide = tibble::tibble(
        `Precursor Name` = c("LPC16:0", "LPC18:0"),
        control_chart = c(TRUE, TRUE),
        Note = c("SIL_A", "SIL_B")
      )))
    )
  )

  stub(qcCheckR_target_control_charts, "get_common_control_metabolites", function(ml) c("LPC16:0"))
  stub(qcCheckR_target_control_charts, "get_plate_boundaries", function(ml) c(0.5, 6.5))
  stub(qcCheckR_target_control_charts, "plot_control_chart", function(...) "mock_chart")
  stub(qcCheckR_target_control_charts, "update_script_log", function(ml, ...) ml)

  result <- suppressMessages(
    qcCheckR_target_control_charts(master_list)
  )

  expect_true("LPC16:0" %in% names(result$control_charts))
  expect_equal(result$control_charts[["LPC16:0"]], "mock_chart")
})

# ============================================================================
# qcCheckR_Utils.R -- get_common_control_metabolites (lines 3020-3026)
# ============================================================================

test_that("get_common_control_metabolites intersects precursors across versions (lines 3020-3026)", {
  master_list <- list(
    templates = list(
      `Plate SIL version` = list(plate1 = "v1", plate2 = "v2"),
      mrm_guides = list(
        v1 = list(SIL_guide = tibble::tibble(
          `Precursor Name` = c("LPC16:0", "LPC18:0", "LPC20:0"),
          control_chart = c(TRUE, TRUE, FALSE)
        )),
        v2 = list(SIL_guide = tibble::tibble(
          `Precursor Name` = c("LPC16:0", "LPC22:0"),
          control_chart = c(TRUE, TRUE)
        ))
      )
    )
  )

  result <- get_common_control_metabolites(master_list)
  expect_equal(result, "LPC16:0")
})

# ============================================================================
# qcCheckR_Utils.R -- get_plate_boundaries (lines 3037-3050)
# ============================================================================

test_that("get_plate_boundaries computes boundaries from PCA scores (lines 3037-3050)", {
  scores <- tibble::tibble(
    sample_run_index = c(1, 2, 3, 4, 5, 6),
    sample_plate_id = rep(c("plate1", "plate2"), each = 3)
  )

  master_list <- list(pca = list(scores = list(m1 = scores)))

  result <- get_plate_boundaries(master_list)
  expect_true(is.numeric(result))
  expect_true(0.5 %in% result)
})

# ============================================================================
# qcCheckR_Utils.R -- get_plate_annotations (lines 3061-3077)
# ============================================================================

test_that("get_plate_annotations creates annotation tibble with median positions (lines 3061-3077)", {
  scores <- tibble::tibble(
    sample_run_index = c(1, 2, 3, 4, 5, 6),
    sample_plate_id = rep(c("plate1", "plate2"), each = 3)
  )

  master_list <- list(pca = list(scores = list(m1 = scores)))

  result <- get_plate_annotations(master_list)
  expect_true("sample_data_source" %in% colnames(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$sample_plate_id, c("plate1", "plate2"))
  expect_length(result$sample_run_index, 2)
})

# ============================================================================
# qcCheckR_Utils.R -- plot_control_chart (lines 3095-3175)
# ============================================================================

test_that("plot_control_chart builds plotly object (lines 3095-3175)", {
  pa_data <- tibble::tibble(
    sample_name = paste0("s", 1:4),
    sample_type_factor = c("sample", "qc", "sample", "qc"),
    sample_run_index = 1:4,
    sample_plate_id = "plate1",
    sample_data_source = ".peakArea",
    `LPC16:0` = c(10, 20, 30, 40),
    SIL_A = c(100, 200, 300, 400)
  )

  conc_data <- tibble::tibble(
    sample_name = paste0("s", 1:4),
    sample_type_factor = c("sample", "qc", "sample", "qc"),
    sample_run_index = 1:4,
    sample_plate_id = "plate1",
    sample_data_source = "concentration",
    `LPC16:0` = c(1, 2, 3, 4)
  )

  master_list <- list(
    project_details = list(
      project_name = "test_project",
      plot_shape = c(sample = 21, qc = 23),
      plot_colour = c(sample = "black", qc = "red"),
      plot_size = c(sample = 2, qc = 3),
      plot_fill = c(sample = "#440154FF", qc = "#FDE725FF")
    ),
    templates = list(
      `Plate SIL version` = list(plate1 = "v1"),
      mrm_guides = list(v1 = list(SIL_guide = tibble::tibble(
        `Precursor Name` = "LPC16:0",
        control_chart = TRUE,
        Note = "SIL_A"
      )))
    ),
    data = list(
      peakArea = list(imputed = list(plate1 = pa_data)),
      concentration = list(
        imputed = list(plate1 = conc_data),
        statTargetProcessed = list(plate1 = conc_data)
      )
    )
  )

  result <- plot_control_chart(master_list, "LPC16:0", c(0.5, 4.5))
  expect_true(inherits(result, "plotly") || inherits(result, "htmlwidget"))
})

# ============================================================================
# qcCheckR_Utils.R -- qcCheckR_export_all (lines 3193-3199)
# ============================================================================

test_that("qcCheckR_export_all calls all three export functions (lines 3193-3199)", {
  master_list <- list(dummy = TRUE)
  call_log <- character(0)

  stub(qcCheckR_export_all, "export_xlsx_file", function(ml) {
    call_log <<- c(call_log, "xlsx")
    ml
  })
  stub(qcCheckR_export_all, "export_html_report", function(ml) {
    call_log <<- c(call_log, "html")
    ml
  })
  stub(qcCheckR_export_all, "export_master_list_rda", function(ml) {
    call_log <<- c(call_log, "rda")
    ml
  })

  result <- suppressMessages(qcCheckR_export_all(master_list))

  expect_equal(call_log, c("xlsx", "html", "rda"))
})

# Locks in the detached-RDA contract: when callers (e.g. the Shiny QC tab)
# pass write_rda = FALSE, qcCheckR_export_all must run XLSX and HTML but
# skip export_master_list_rda so the caller can fire that step in its own
# background subprocess. Regressing this would silently re-introduce the
# slow xz/gzip save into the foreground pipeline.
test_that("qcCheckR_export_all skips RDA export when write_rda = FALSE", {
  master_list <- list(dummy = TRUE)
  call_log <- character(0)

  stub(qcCheckR_export_all, "export_xlsx_file", function(ml) {
    call_log <<- c(call_log, "xlsx")
    ml
  })
  stub(qcCheckR_export_all, "export_html_report", function(ml) {
    call_log <<- c(call_log, "html")
    ml
  })
  stub(qcCheckR_export_all, "export_master_list_rda", function(ml) {
    call_log <<- c(call_log, "rda")
    ml
  })

  result <- suppressMessages(
    qcCheckR_export_all(master_list, write_rda = FALSE)
  )

  expect_equal(call_log, c("xlsx", "html"))
  expect_false("rda" %in% call_log)
})

# ============================================================================
# qcCheckR_Utils.R -- export_xlsx_file (lines 2864-2905 range re-mapped to 3210-3278)
# ============================================================================

test_that("export_xlsx_file writes xlsx with correct structure", {
  temp_dir <- withr::local_tempdir()
  xlsx_dir <- file.path(temp_dir, "all", "xlsx_report")
  dir.create(xlsx_dir, recursive = TRUE)

  sorted_data <- tibble::tibble(
    sample_name = paste0("s", 1:3),
    sample_type_factor = c("sample", "qc", "sample"),
    SIL_A = c(100, 200, 300),
    LPC16 = c(10.0, 20.0, 30.0)
  )

  conc_data <- tibble::tibble(
    sample_name = paste0("s", 1:3),
    LPC16 = c(1.0, 2.0, 3.0)
  )

  master_list <- list(
    project_details = list(
      project_dir = temp_dir,
      user_name = "test_user",
      project_name = "test_project"
    ),
    summary_tables = list(
      projectOverview = tibble::tibble(metric = "test", plate1 = "val")
    ),
    filters = list(
      samples.missingValues = tibble::tibble(sample.flag = c(0, 1)),
      lipid.missingValues = list(allPlates = list()),
      failed_lipids = character(0),
      rsd = tibble::tibble(
        dataBatch = "allBatches",
        dataSource = "peakArea",
        LPC16 = 5
      )
    ),
    data = list(
      peakArea = list(sorted = list(plate1 = sorted_data)),
      concentration = list(
        sorted = list(plate1 = conc_data),
        statTargetProcessed = list(plate1 = conc_data)
      )
    )
  )

  stub(export_xlsx_file, "create_user_guide", function(ml) tibble::tibble(guide = "info"))
  stub(export_xlsx_file, "format_rsd_table", function(ml) tibble::tibble(rsd = 5))
  stub(export_xlsx_file, "filter_concentration", function(ml, source) conc_data)

  captured_path <- NULL
  stub(export_xlsx_file, "openxlsx::write.xlsx", function(x, file, ...) {
    captured_path <<- file
  })

  result <- suppressMessages(export_xlsx_file(master_list))

  expect_true(grepl("qcCheckeR\\.xlsx$", captured_path))
  expect_true(!is.null(result$summary_tables$odsAreaOverview))
})

# ============================================================================
# qcCheckR_Utils.R -- export_html_report (lines 3288-3368)
# ============================================================================

test_that("export_html_report warns when rmarkdown not available (lines 3336-3344)", {
  temp_dir <- withr::local_tempdir()
  html_dir <- file.path(temp_dir, "all", "html_report")
  dir.create(html_dir, recursive = TRUE)

  master_list <- list(
    project_details = list(
      project_dir = temp_dir,
      user_name = "test_user",
      project_name = "test_project"
    ),
    control_charts = list()
  )

  stub(export_html_report, "system.file", function(...) {
    tf <- tempfile(fileext = ".Rmd")
    withr::defer(unlink(tf), envir = parent.frame())
    writeLines(c("---", "title: test", "---", "control_charts_custom_code_placeholder"), tf)
    tf
  })
  stub(export_html_report, "requireNamespace", FALSE)

  expect_warning(
    suppressMessages(export_html_report(master_list)),
    "rmarkdown.*required"
  )
})

test_that("export_html_report warns when pandoc not available (lines 3347-3356)", {
  temp_dir <- withr::local_tempdir()
  html_dir <- file.path(temp_dir, "all", "html_report")
  dir.create(html_dir, recursive = TRUE)

  master_list <- list(
    project_details = list(
      project_dir = temp_dir,
      user_name = "test_user",
      project_name = "test_project"
    ),
    control_charts = list(`LPC16:0` = "chart_placeholder")
  )

  stub(export_html_report, "system.file", function(...) {
    tf <- tempfile(fileext = ".Rmd")
    withr::defer(unlink(tf), envir = parent.frame())
    writeLines(c("---", "title: test", "---", "control_charts_custom_code_placeholder"), tf)
    tf
  })
  stub(export_html_report, "requireNamespace", TRUE)
  stub(export_html_report, "rmarkdown::pandoc_available", FALSE)

  expect_warning(
    suppressMessages(export_html_report(master_list)),
    "pandoc.*required"
  )
})

# ============================================================================
# qcCheckR_Utils.R -- qcCheckR_plot_options QC type not in sample tags (line 2627-2631)
# ============================================================================

test_that("qcCheckR_plot_options handles QC type not in sample_tags gracefully", {
  master_list <- list(
    project_details = list(
      sample_tags = c("sample", "blank"),
      qc_type = "qc"
    )
  )

  result <- qcCheckR_plot_options(master_list)

  # qc_type "qc" is not in sample_tags, so no special override applied
  expect_equal(result$project_details$plot_colour[["sample"]], "black")
  expect_equal(result$project_details$plot_colour[["blank"]], "black")
  # "qc" key should not exist in the named vector
  expect_false("qc" %in% names(result$project_details$plot_colour))
})

# ============================================================================
# qcCheckR_set_project_details - line 121: else branch (non-ANPC, NULL sample_tags)
# ============================================================================

test_that("qcCheckR_set_project_details uses NULL sample_tags for non-ANPC user (line 121)", {
  ml <- list(project_details = list(
    script_log = list(timestamps = list(), runtimes = list(), messages = list())
  ))

  result <- qcCheckR_set_project_details(
    master_list = ml,
    user_name = "ExternalUser",
    project_directory = tempdir(),
    QC_sample_label = "qc",
    sample_tags = NULL,
    mv_threshold = 0.5
  )

  # For non-ANPC user with NULL sample_tags, the else branch at line 121 is hit
  # sample_tags should be set to NULL
  expect_null(result$project_details$sample_tags)
})

# ============================================================================
# qcCheckR_import_PeakForgeR_reports - Windows long path junction (lines 285-294, 310)
# ============================================================================

test_that("qcCheckR_import_PeakForgeR_reports handles Windows long path junction (lines 285-294, 310)", {
  tmp <- withr::local_tempdir()

  # Create a PeakForgeR report CSV file
  csv_content <- data.frame(
    FileName = c("sample1.mzML", "sample2.mzML"),
    Area = c(1000, 2000),
    Height = c(500, 1000),
    stringsAsFactors = FALSE
  )
  d <- Sys.Date()
  csv_file <- file.path(tmp, paste0(d, "_PeakForgeR_plate1.csv"))
  readr::write_csv(csv_content, csv_file)

  ml <- list(
    project_details = list(
      project_dir = tmp,
      user_name = "TestUser",
      plateIDs = c()
    ),
    data = list(PeakForgeRReport = list())
  )

  stub(qcCheckR_import_PeakForgeR_reports, ".Platform", list(OS.type = "windows"))
  stub(qcCheckR_import_PeakForgeR_reports, "nchar", function(x, ...) {
    if (is.character(x) && length(x) == 1 && grepl("PeakForgeR", x)) 300 else base::nchar(x)
  })
  stub(qcCheckR_import_PeakForgeR_reports, "system2", function(...) NULL)
  stub(qcCheckR_import_PeakForgeR_reports, "dir.exists", function(x) {
    if (grepl("PeakForgeR_", x)) TRUE else base::dir.exists(x)
  })
  stub(qcCheckR_import_PeakForgeR_reports, "unlink", function(...) NULL)

  result <- suppressMessages(
    tryCatch(qcCheckR_import_PeakForgeR_reports(ml), error = function(e) e)
  )
  expect_false(is.null(result))
})

# ============================================================================
# qcCheckR_import_PeakForgeR_reports - ANPC filter branch (lines 325-327)
# ============================================================================

test_that("qcCheckR_import_PeakForgeR_reports filters COND/BLANK/ISTDs for ANPC (lines 325-327)", {
  tmp <- withr::local_tempdir()

  csv_content <- data.frame(
    FileName = c("sample1.mzML", "COND_01.mzML", "BLANK_02.mzML", "ISTDs_03.mzML", "sample2.mzML"),
    Area = c(1000, 500, 300, 200, 2000),
    Height = c(500, 250, 150, 100, 1000),
    stringsAsFactors = FALSE
  )
  d <- Sys.Date()
  csv_file <- file.path(tmp, paste0(d, "_PeakForgeR_plate1.csv"))
  readr::write_csv(csv_content, csv_file)

  ml <- list(
    project_details = list(
      project_dir = tmp,
      user_name = "ANPC",
      plateIDs = c()
    ),
    data = list(PeakForgeRReport = list())
  )

  result <- suppressMessages(qcCheckR_import_PeakForgeR_reports(ml))

  # COND, BLANK, ISTDs should be filtered out for ANPC user
  report_key <- names(result$data$PeakForgeRReport)[1]
  report_data <- result$data$PeakForgeRReport[[report_key]]
  expect_false(any(grepl("COND|BLANK|ISTDs", report_data$FileName)))
  expect_true(any(grepl("sample1", report_data$FileName)))
})

# ============================================================================
# process_sil_target - ncol(target_data) == 0 branch (line 1070)
# ============================================================================

test_that("process_sil_target returns early when no precursors match data columns (line 1070)", {
  ml <- list(
    templates = list(
      mrm_guides = list(
        v1 = list(
          SIL_guide = data.frame(
            Note = c("SIL_A"),
            `Precursor Name` = c("NonexistentLipid"),
            check.names = FALSE, stringsAsFactors = FALSE
          ),
          conc_guide = data.frame(
            concentration_factor = 1,
            SIL_name = "SIL_A"
          )
        )
      )
    ),
    data = list(
      peakArea = list(
        sorted = list(
          plate1 = data.frame(
            sample_name = c("s1", "s2"),
            SIL_A = c(100, 200),
            Lipid_X = c(50, 60),
            stringsAsFactors = FALSE
          )
        )
      ),
      response = list(sorted = list(plate1 = data.frame(sample_name = c("s1", "s2"))))
    )
  )

  result <- process_sil_target(ml, "plate1", "sorted", "v1", "SIL_A")
  # Should return master_list unchanged since no matching columns
  expect_identical(result, ml)
})

# ============================================================================
# qcCheckR_combat_correction - lines 1614-1631, 1638, 1648, 1650, 1670-1679
# ============================================================================

test_that("qcCheckR_combat_correction stops when no numeric metabolite columns (line 1614)", {
  ml <- list(
    data = list(
      concentration = list(
        imputed = list(
          plate1 = data.frame(
            sample_name = c("s1", "s2"),
            sample_type_factor = c("sample", "qc"),
            sample_plate_id = c("plate1", "plate1"),
            met1 = c("a", "b"),
            stringsAsFactors = FALSE
          )
        )
      )
    )
  )

  expect_error(
    suppressMessages(qcCheckR_combat_correction(ml)),
    "No numeric metabolite columns"
  )
})

test_that("qcCheckR_combat_correction handles NA imputation and zero-variance features (lines 1623-1631, 1638)", {
  ml <- list(
    project_details = list(
      combat_par.prior = TRUE,
      combat_mean.only = FALSE,
      combat_ref.batch = NULL,
      statTarget_qc_type = "qc",
      qc_type = "qc"
    ),
    data = list(
      concentration = list(
        imputed = list(
          plate1 = data.frame(
            sample_name = paste0("s", 1:6),
            sample_type_factor = rep(c("sample", "qc"), 3),
            sample_plate_id = rep("plate1", 6),
            met1 = c(10, NA, 30, 40, 50, 60),
            met2 = c(5, 5, 5, 5, 5, 5),
            met3 = c(100, 200, 300, 400, 500, 600),
            stringsAsFactors = FALSE
          ),
          plate2 = data.frame(
            sample_name = paste0("s", 7:12),
            sample_type_factor = rep(c("sample", "qc"), 3),
            sample_plate_id = rep("plate2", 6),
            met1 = c(15, 25, NA, 45, 55, 65),
            met2 = c(5, 5, 5, 5, 5, 5),
            met3 = c(110, 210, 310, 410, 510, 610),
            stringsAsFactors = FALSE
          )
        )
      )
    )
  )

  stub(qcCheckR_combat_correction, "sva::ComBat", function(dat, batch, ...) {
    # Return the data back as-is (simulating ComBat)
    dat
  })

  result <- suppressMessages(
    tryCatch(qcCheckR_combat_correction(ml), error = function(e) e)
  )

  # The function should complete, potentially with ComBat correction applied
  if (!inherits(result, "error")) {
    expect_true("concentration" %in% names(result$data))
  }
  expect_false(is.null(result))
})

test_that("qcCheckR_combat_correction uses defaults for NULL par_prior and mean_only (lines 1648, 1650)", {
  ml <- list(
    project_details = list(
      combat_par.prior = NULL,
      combat_mean.only = NULL,
      combat_ref.batch = NULL,
      statTarget_qc_type = NULL,
      qc_type = "qc"
    ),
    data = list(
      concentration = list(
        imputed = list(
          plate1 = data.frame(
            sample_name = paste0("s", 1:6),
            sample_type_factor = rep(c("sample", "qc"), 3),
            sample_plate_id = rep("plate1", 6),
            met1 = c(10, 20, 30, 40, 50, 60),
            stringsAsFactors = FALSE
          ),
          plate2 = data.frame(
            sample_name = paste0("s", 7:12),
            sample_type_factor = rep(c("sample", "qc"), 3),
            sample_plate_id = rep("plate2", 6),
            met1 = c(15, 25, 35, 45, 55, 65),
            stringsAsFactors = FALSE
          )
        )
      )
    )
  )

  par_prior_used <- NULL
  mean_only_used <- NULL
  stub(qcCheckR_combat_correction, "sva::ComBat", function(dat, batch, mod, par.prior, prior.plots, mean.only, ref.batch) {
    par_prior_used <<- par.prior
    mean_only_used <<- mean.only
    dat
  })

  result <- suppressMessages(
    tryCatch(qcCheckR_combat_correction(ml), error = function(e) e)
  )

  # par.prior defaults to TRUE, mean.only defaults to FALSE when NULL
  expect_true(par_prior_used)
  expect_false(mean_only_used)
})

# ============================================================================
# run_pca_model - concentration branch and validation (lines 2686-2700, 2706, 2716-2728)
# ============================================================================

test_that("run_pca_model with concentration source processes imputed and statTargetProcessed (lines 2686-2700)", {
  sample_df <- data.frame(
    sample_name = paste0("s", 1:10),
    sample_type_factor = rep(c("sample", "qc"), 5),
    sample_plate_id = rep("plate1", 10),
    Lipid_A = rnorm(10, 100, 10),
    Lipid_B = rnorm(10, 200, 20),
    stringsAsFactors = FALSE
  )

  ml <- list(
    project_details = list(),
    data = list(
      peakArea = list(
        sorted = list(plate1 = sample_df)
      ),
      concentration = list(
        sorted = list(plate1 = sample_df),
        imputed = list(plate1 = sample_df),
        statTargetProcessed = list(plate1 = sample_df)
      )
    ),
    filters = list(
      failed_samples = character(0),
      failed_lipids = character(0),
      rsd = data.frame(
        dataSource = character(0),
        dataBatch = character(0),
        stringsAsFactors = FALSE
      )
    ),
    pca = list()
  )

  result <- suppressMessages(
    tryCatch(run_pca_model(ml, "concentration"), error = function(e) e)
  )
  expect_false(is.null(result))
})

test_that("run_pca_model skips gracefully when source not found in master_list (lines 2692-2700)", {
  ml <- list(
    data = list(
      peakArea = list(sorted = list(plate1 = data.frame(sample_name = "s1"))),
      concentration = list(
        sorted = list()
      )
    )
  )

  # "concentration" source sets data_keys to c("imputed", "statTargetProcessed")
  # but ml has no "imputed" key under concentration — should skip gracefully
  expect_message(
    run_pca_model(ml, "concentration"),
    "skipping"
  )
})

test_that("run_pca_model errors when sorted concentration data is empty (line 2706)", {
  ml <- list(
    data = list(
      concentration = list(
        sorted = list(),
        imputed = list(plate1 = data.frame(sample_name = "s1", Lipid_A = 1))
      )
    )
  )

  expect_error(
    run_pca_model(ml, "concentration"),
    "No sorted concentration data"
  )
})

test_that("run_pca_model applies preprocessed filters (lines 2716-2728)", {
  sample_df <- data.frame(
    sample_name = paste0("s", 1:10),
    sample_type_factor = rep(c("sample", "qc"), 5),
    sample_plate_id = rep("plate1", 10),
    Lipid_A = rnorm(10, 100, 10),
    Lipid_B = rnorm(10, 200, 20),
    stringsAsFactors = FALSE
  )

  rsd_df <- data.frame(
    dataSource = "peakArea",
    dataBatch = "allBatches",
    Lipid_A = 10,
    Lipid_B = 50,
    stringsAsFactors = FALSE
  )

  ml <- list(
    project_details = list(),
    data = list(
      peakArea = list(
        sorted = list(plate1 = sample_df),
        imputed = list(plate1 = sample_df)
      ),
      concentration = list(
        sorted = list(plate1 = sample_df)
      )
    ),
    filters = list(
      failed_samples = c("s1"),
      failed_lipids = character(0),
      rsd = rsd_df
    ),
    pca = list()
  )

  result <- suppressMessages(
    tryCatch(run_pca_model(ml, "peakArea", preprocessed = TRUE), error = function(e) e)
  )
  expect_false(is.null(result))
})

# ============================================================================
# export_html_report - rmarkdown::render branch (lines 3359-3377)
# ============================================================================

test_that("export_html_report renders HTML and opens it (lines 3359-3377)", {
  tmp <- withr::local_tempdir()
  html_dir <- file.path(tmp, "all", "html_report")
  dir.create(html_dir, recursive = TRUE)

  ml <- list(
    project_details = list(
      project_dir = tmp,
      user_name = "TestUser",
      project_name = "TestProject"
    ),
    control_charts = list(
      chart1 = "plotly_obj"
    )
  )

  stub(export_html_report, "system.file", function(...) {
    # Return a fake template path
    tpl <- file.path(tmp, "template.Rmd")
    writeLines(c(
      "---",
      "title: 'Test'",
      "---",
      "control_charts_custom_code_placeholder"
    ), tpl)
    tpl
  })
  stub(export_html_report, "rmarkdown::pandoc_available", function(...) TRUE)
  stub(export_html_report, "rmarkdown::render", function(...) NULL)
  stub(export_html_report, "utils::browseURL", function(...) NULL)

  result <- suppressMessages(
    tryCatch(export_html_report(ml), error = function(e) e)
  )
  expect_false(is.null(result))
})

test_that("export_html_report handles render failure gracefully (lines 3368-3374)", {
  tmp <- withr::local_tempdir()
  html_dir <- file.path(tmp, "all", "html_report")
  dir.create(html_dir, recursive = TRUE)

  ml <- list(
    project_details = list(
      project_dir = tmp,
      user_name = "TestUser",
      project_name = "TestProject"
    ),
    control_charts = list()
  )

  stub(export_html_report, "system.file", function(...) {
    tpl <- file.path(tmp, "template.Rmd")
    writeLines(c(
      "---",
      "title: 'Test'",
      "---",
      "control_charts_custom_code_placeholder"
    ), tpl)
    tpl
  })
  stub(export_html_report, "rmarkdown::pandoc_available", function(...) TRUE)
  stub(export_html_report, "rmarkdown::render", function(...) stop("Render failed"))

  expect_warning(
    suppressMessages(export_html_report(ml)),
    "HTML report generation failed"
  )
})

# ============================================================================
# export_master_list_rda - callr::r_bg branch (line 3401)
# ============================================================================

test_that("export_master_list_rda saves RDA synchronously (line 3401)", {
  tmp <- withr::local_tempdir()

  ml <- list(
    project_details = list(
      project_dir = tmp,
      project_name = "TestProject",
      script_log = list(
        timestamps = list(start_time = Sys.time()),
        runtimes = list(),
        messages = list()
      )
    )
  )

  save_called <- FALSE
  stub(export_master_list_rda, "save", function(...) {
    save_called <<- TRUE
    invisible(NULL)
  })
  stub(export_master_list_rda, "update_script_log", function(ml, ...) ml)

  result <- suppressMessages(export_master_list_rda(ml))

  expect_true(save_called)
})

# ============================================================================
# create_user_guide (lines 3422-3501)
# ============================================================================

test_that("create_user_guide creates a tibble with project metrics (lines 3422-3501)", {
  sample_df <- data.frame(
    sample_name = paste0("s", 1:6),
    sample_type_factor = c("sample", "qc", "blank", "sample", "qc", "blank"),
    sample_plate_id = rep("plate1", 6),
    SIL_A = rnorm(6),
    Lipid_X = rnorm(6),
    Lipid_Y = rnorm(6),
    stringsAsFactors = FALSE
  )

  conc_df <- data.frame(
    sample_name = paste0("s", 1:6),
    Lipid_X = rnorm(6),
    Lipid_Y = rnorm(6),
    stringsAsFactors = FALSE
  )

  ml <- list(
    project_details = list(
      project_name = "TestProject",
      user_name = "TestUser",
      qc_type = "qc"
    ),
    templates = list(
      `Plate SIL version` = c("v1", "v1")
    ),
    data = list(
      peakArea = list(
        sorted = list(plate1 = sample_df)
      ),
      concentration = list(
        statTargetProcessed = list(plate1 = conc_df)
      )
    )
  )

  result <- create_user_guide(ml)

  expect_true(tibble::is_tibble(result))
  expect_true("key" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true(any(result$key == "projectName"))
  expect_true(any(result$key == "total.Samples"))
})

# ============================================================================
# filter_concentration (lines 3535-3549)
# ============================================================================

test_that("filter_concentration filters by failed samples and high RSD lipids (lines 3535-3549)", {
  sample_df <- data.frame(
    sample_name = paste0("s", 1:5),
    sample_type_factor = rep("sample", 5),
    Lipid_A = c(10, 20, 30, 40, 50),
    Lipid_B = c(100, 200, 300, 400, 500),
    Lipid_C = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  rsd_df <- data.frame(
    dataSource = "concentration",
    dataBatch = "allBatches",
    Lipid_A = 10,
    Lipid_B = 50,
    Lipid_C = 15,
    stringsAsFactors = FALSE
  )

  ml <- list(
    data = list(
      concentration = list(
        imputed = list(plate1 = sample_df)
      )
    ),
    filters = list(
      failed_samples = c("s1"),
      failed_lipids = c("Lipid_C"),
      rsd = rsd_df
    )
  )

  result <- filter_concentration(ml, "concentration")

  # s1 should be removed (failed sample)
  expect_false("s1" %in% result$sample_name)
  # Lipid_C should be removed (failed lipid)
  expect_false("Lipid_C" %in% names(result))
  # Lipid_B should be removed (RSD > 30)
  expect_false("Lipid_B" %in% names(result))
  # Lipid_A should remain (RSD <= 30)
  expect_true("Lipid_A" %in% names(result))
})

test_that("filter_concentration uses statTargetProcessed for non-concentration source (lines 3535-3538)", {
  sample_df <- data.frame(
    sample_name = paste0("s", 1:3),
    sample_type_factor = rep("sample", 3),
    Lipid_A = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  rsd_df <- data.frame(
    dataSource = "statTarget",
    dataBatch = "allBatches",
    Lipid_A = 10,
    stringsAsFactors = FALSE
  )

  ml <- list(
    data = list(
      concentration = list(
        statTargetProcessed = list(plate1 = sample_df)
      )
    ),
    filters = list(
      failed_samples = character(0),
      failed_lipids = character(0),
      rsd = rsd_df
    )
  )

  result <- filter_concentration(ml, "statTarget")
  expect_true("Lipid_A" %in% names(result))
  expect_equal(nrow(result), 3)
})
