library(mockery)

#Tests for inputs ----
test_that("qcCheckR throws error when required arguments are missing or invalid", {
  suppressMessages({

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(qcCheckR(project_directory = temp_dir),
               "user_name.*required")

  expect_error(qcCheckR(user_name = "user", project_directory = NULL),
               "project_directory.*must be a single")

  expect_error(qcCheckR(user_name = "user", project_directory = temp_dir,
                        QC_sample_label = "qc", sample_tags = c("sample")),
               "mrm_template_list.*required")

  expect_error(qcCheckR(user_name = "user", project_directory = temp_dir,
                        mrm_template_list = list(a = 1), sample_tags = c("sample")),
               "QC_sample_label.*required")

  expect_error(qcCheckR(user_name = "user", project_directory = temp_dir,
                        mrm_template_list = list(a = 1), QC_sample_label = "qc"),
               "sample_tags.*required")

  expect_error(qcCheckR(user_name = "user", project_directory = temp_dir,
                        mrm_template_list = list(a = 1), QC_sample_label = "qc",
                        sample_tags = c("sample"), mv_threshold = 150),
               "mv_threshold.*must be.*numeric.*between 0 and 100")
  })
})

#Tests for ANPC Exception logic
test_that("qcCheckR allows missing templates for ANPC user", {
  suppressMessages({
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(qcCheckR(user_name = "ANPC", project_directory = temp_dir, mv_threshold = 50),
               "No report files found in the specified project directory.")
  #If it gets to this point then ANPC exception logic is working
  })
})

# ============================================================================
# Additional input validation edge cases
# ============================================================================

test_that("qcCheckR rejects numeric user_name", {
  expect_error(
    qcCheckR(user_name = 42, project_directory = tempdir()),
    "user_name.*must be.*non-empty.*single character"
  )
})

test_that("qcCheckR rejects empty string user_name", {
  expect_error(
    qcCheckR(user_name = "", project_directory = tempdir()),
    "user_name.*must be.*non-empty.*single character"
  )
})

test_that("qcCheckR rejects vector user_name", {
  expect_error(
    qcCheckR(user_name = c("user1", "user2"), project_directory = tempdir()),
    "user_name.*must be.*non-empty.*single character"
  )
})

test_that("qcCheckR rejects nonexistent project_directory", {
  expect_error(
    qcCheckR(user_name = "user", project_directory = "/nonexistent/path_99999"),
    "does not exist"
  )
})

test_that("qcCheckR emits welcome message for valid user_name", {
  temp_dir <- tempfile("qccheck_welcome_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_message(
    tryCatch(
      qcCheckR(user_name = "TestUser", project_directory = temp_dir,
               mrm_template_list = list(a = 1), QC_sample_label = "qc",
               sample_tags = c("sample")),
      error = function(e) invisible(NULL)
    ),
    "Welcome TestUser!"
  )
})

test_that("qcCheckR rejects mv_threshold of NA", {
  temp_dir <- tempfile("qccheck_na_mv_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "user", project_directory = temp_dir,
             mrm_template_list = list(a = 1), QC_sample_label = "qc",
             sample_tags = c("sample"), mv_threshold = NA),
    "mv_threshold.*must be.*numeric.*between 0 and 100"
  )
})

test_that("qcCheckR rejects negative mv_threshold", {
  temp_dir <- tempfile("qccheck_neg_mv_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "user", project_directory = temp_dir,
             mrm_template_list = list(a = 1), QC_sample_label = "qc",
             sample_tags = c("sample"), mv_threshold = -1),
    "mv_threshold.*must be.*numeric.*between 0 and 100"
  )
})

test_that("qcCheckR accepts mv_threshold at boundary values 0 and 100", {
  temp_dir <- tempfile("qccheck_boundary_mv_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  dummy_template <- list(v1 = list(
    SIL_guide = tempfile(fileext = ".tsv"),
    conc_guide = tempfile(fileext = ".tsv")
  ))

  # mv_threshold = 0 should pass validation, then fail downstream
  # (fails reading nonexistent template files, confirming mv_threshold was accepted)
  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = dummy_template, QC_sample_label = "qc",
               sample_tags = c("sample"), mv_threshold = 0)
    )
  )

  # mv_threshold = 100 should also pass validation
  expect_error(
    suppressMessages(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = dummy_template, QC_sample_label = "qc",
               sample_tags = c("sample"), mv_threshold = 100)
    )
  )
})

test_that("qcCheckR rejects character mv_threshold", {
  temp_dir <- tempfile("qccheck_char_mv_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "user", project_directory = temp_dir,
             mrm_template_list = list(a = 1), QC_sample_label = "qc",
             sample_tags = c("sample"), mv_threshold = "fifty"),
    "mv_threshold.*must be.*numeric.*between 0 and 100"
  )
})

# ============================================================================
# ANPC user path: skips mrm_template_list, QC_sample_label, sample_tags checks
# ============================================================================

test_that("qcCheckR ANPC user skips QC_sample_label validation", {
  temp_dir <- tempfile("qccheck_anpc_qc_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # ANPC user with no QC_sample_label should not error on that param
  # It should proceed to setup and fail on missing report files
  expect_error(
    suppressMessages(
      qcCheckR(user_name = "ANPC", project_directory = temp_dir)
    ),
    "No report files found"
  )
})

test_that("qcCheckR ANPC user skips sample_tags validation", {
  temp_dir <- tempfile("qccheck_anpc_tags_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # ANPC user with no sample_tags should reach setup stage
  expect_error(
    suppressMessages(
      qcCheckR(user_name = "ANPC", project_directory = temp_dir, sample_tags = NULL)
    ),
    "No report files found"
  )
})

# ============================================================================
# Workflow orchestration tests (mocked sub-functions)
# ============================================================================

test_that("qcCheckR calls workflow functions in correct order for non-ANPC user", {
  temp_dir <- tempfile("qccheck_workflow_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  call_log <- character(0)
  mock_ml <- list(dummy = TRUE)

  stub(qcCheckR, "validate_project_directory", function(x, ...) x)
  stub(qcCheckR, "qcCheckR_setup_project", function(...) {
    call_log <<- c(call_log, "setup_project")
    mock_ml
  })
  stub(qcCheckR, "qcCheckR_transpose_data", function(ml) {
    call_log <<- c(call_log, "transpose")
    ml
  })
  # ... absorbs the date_order arg the real qcCheckR() forwards.
  stub(qcCheckR, "qcCheckR_sort_data", function(ml, ...) {
    call_log <<- c(call_log, "sort")
    ml
  })
  stub(qcCheckR, "qcCheckR_impute_data", function(ml) {
    call_log <<- c(call_log, "impute")
    ml
  })
  stub(qcCheckR, "qcCheckR_calculate_response_concentration", function(ml) {
    call_log <<- c(call_log, "calc_conc")
    ml
  })
  stub(qcCheckR, "qcCheckR_statTarget_batch_correction", function(ml) {
    call_log <<- c(call_log, "batch_correct")
    ml
  })
  stub(qcCheckR, "qcCheckR_set_qc", function(ml) {
    call_log <<- c(call_log, "set_qc")
    ml
  })
  stub(qcCheckR, "qcCheckR_sample_filter", function(ml) {
    call_log <<- c(call_log, "sample_filter")
    ml
  })
  stub(qcCheckR, "qcCheckR_sil_IntStd_filter", function(ml) {
    call_log <<- c(call_log, "sil_filter")
    ml
  })
  stub(qcCheckR, "qcCheckR_lipid_filter", function(ml) {
    call_log <<- c(call_log, "lipid_filter")
    ml
  })
  stub(qcCheckR, "qcCheckR_RSD_filter", function(ml) {
    call_log <<- c(call_log, "rsd_filter")
    ml
  })
  stub(qcCheckR, "qcCheckR_summary_report", function(ml) {
    call_log <<- c(call_log, "summary_report")
    ml
  })
  stub(qcCheckR, "qcCheckR_plot_options", function(ml) {
    call_log <<- c(call_log, "plot_options")
    ml
  })
  stub(qcCheckR, "qcCheckR_PCA", function(ml) {
    call_log <<- c(call_log, "pca")
    ml
  })
  stub(qcCheckR, "qcCheckR_run_order_plots", function(ml) {
    call_log <<- c(call_log, "run_order")
    ml
  })
  stub(qcCheckR, "qcCheckR_target_control_charts", function(ml) {
    call_log <<- c(call_log, "control_charts")
    ml
  })
  # Stub accepts ... to absorb the write_rda arg the real qcCheckR()
  # forwards (see qcCheckR.R: qcCheckR_export_all(master_list,
  # write_rda = write_rda)). Without ..., this raises "unused argument".
  stub(qcCheckR, "qcCheckR_export_all", function(ml, ...) {
    call_log <<- c(call_log, "export")
    ml
  })

  suppressMessages(
    qcCheckR(user_name = "TestUser",
             project_directory = temp_dir,
             mrm_template_list = list(a = 1),
             QC_sample_label = "qc",
             sample_tags = c("sample"),
             mv_threshold = 50)
  )

  expected_order <- c(
    "setup_project", "transpose", "sort", "impute", "calc_conc",
    "batch_correct", "set_qc", "sample_filter", "sil_filter",
    "lipid_filter", "rsd_filter", "summary_report", "plot_options",
    "pca", "run_order", "control_charts", "export"
  )
  expect_equal(call_log, expected_order)
})

test_that("qcCheckR passes all parameters to qcCheckR_setup_project", {
  temp_dir <- tempfile("qccheck_params_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  captured_args <- NULL

  stub(qcCheckR, "validate_project_directory", function(x, ...) x)
  stub(qcCheckR, "qcCheckR_setup_project", function(user_name, project_directory, mrm_template_list, QC_sample_label, sample_tags, mv_threshold, lod_threshold) {
    captured_args <<- list(
      user_name = user_name,
      project_directory = project_directory,
      mrm_template_list = mrm_template_list,
      QC_sample_label = QC_sample_label,
      sample_tags = sample_tags,
      mv_threshold = mv_threshold,
      lod_threshold = lod_threshold
    )
    list(dummy = TRUE)
  })
  # Stub remaining workflow steps to return the master_list unchanged
  # Stub lambda takes ... so qcCheckR_export_all (which the real
  # qcCheckR() now calls with write_rda = write_rda) doesn't raise an
  # "unused argument" error. The other stubbed functions only ever
  # receive a single arg, so ... is harmless for them.
  for (fn_name in c("qcCheckR_transpose_data", "qcCheckR_sort_data",
                     "qcCheckR_impute_data", "qcCheckR_calculate_response_concentration",
                     "qcCheckR_statTarget_batch_correction", "qcCheckR_set_qc",
                     "qcCheckR_sample_filter", "qcCheckR_sil_IntStd_filter",
                     "qcCheckR_lipid_filter", "qcCheckR_RSD_filter",
                     "qcCheckR_summary_report", "qcCheckR_plot_options",
                     "qcCheckR_PCA", "qcCheckR_run_order_plots",
                     "qcCheckR_target_control_charts", "qcCheckR_export_all")) {
    stub(qcCheckR, fn_name, function(ml, ...) ml)
  }

  suppressMessages(
    qcCheckR(user_name = "Jane",
             project_directory = temp_dir,
             mrm_template_list = list(a = 1),
             QC_sample_label = "LTR",
             sample_tags = c("sample", "qc"),
             mv_threshold = 30)
  )

  expect_equal(captured_args$user_name, "Jane")
  expect_equal(captured_args$project_directory, temp_dir)
  expect_equal(captured_args$mrm_template_list, list(a = 1))
  expect_equal(captured_args$QC_sample_label, "LTR")
  expect_equal(captured_args$sample_tags, c("sample", "qc"))
  expect_equal(captured_args$mv_threshold, 30)
  expect_equal(captured_args$lod_threshold, 5000)
})

test_that("qcCheckR returns result of qcCheckR_export_all", {
  temp_dir <- tempfile("qccheck_return_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  stub(qcCheckR, "validate_project_directory", function(x, ...) x)
  stub(qcCheckR, "qcCheckR_setup_project", function(...) list(dummy = TRUE))
  for (fn_name in c("qcCheckR_transpose_data", "qcCheckR_sort_data",
                     "qcCheckR_impute_data", "qcCheckR_calculate_response_concentration",
                     "qcCheckR_statTarget_batch_correction", "qcCheckR_set_qc",
                     "qcCheckR_sample_filter", "qcCheckR_sil_IntStd_filter",
                     "qcCheckR_lipid_filter", "qcCheckR_RSD_filter",
                     "qcCheckR_summary_report", "qcCheckR_plot_options",
                     "qcCheckR_PCA", "qcCheckR_run_order_plots",
                     "qcCheckR_target_control_charts")) {
    # ... absorbs the date_order arg qcCheckR() forwards to qcCheckR_sort_data.
    stub(qcCheckR, fn_name, function(ml, ...) ml)
  }
  # ... absorbs the write_rda arg the real qcCheckR() forwards.
  stub(qcCheckR, "qcCheckR_export_all", function(ml, ...) {
    ml$final <- "exported"
    ml
  })

  result <- suppressMessages(
    qcCheckR(user_name = "user",
             project_directory = temp_dir,
             mrm_template_list = list(a = 1),
             QC_sample_label = "qc",
             sample_tags = c("sample"),
             mv_threshold = 50)
  )

  expect_equal(result$final, "exported")
})

# ============================================================================
# Batch correction parameter validation: batch_method
# ============================================================================

test_that("qcCheckR accepts valid batch_method values", {
  temp_dir <- tempfile("qccheck_bm_valid_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  for (method in c("QCRFSC", "ComBat", "QCRLSC")) {
    # Valid batch_method should pass validation and fail downstream (no report files)
    expect_error(
      suppressMessages(
        qcCheckR(user_name = "ANPC", project_directory = temp_dir,
                 batch_method = method)
      ),
      "No report files found"
    )
  }
})

test_that("qcCheckR rejects invalid qcrlsc_method", {
  temp_dir <- tempfile("qccheck_qcrlsc_invalid_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = "QCRLSC", qcrlsc_method = "nope"),
    "should be one of|'arg'"
  )
})

test_that("qcCheckR rejects non-logical qcrlsc_shift", {
  temp_dir <- tempfile("qccheck_qcrlsc_shift_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = "QCRLSC", qcrlsc_shift = "yes"),
    "qcrlsc_shift.*must be TRUE or FALSE"
  )
})

test_that("qcCheckR rejects invalid batch_method string", {
  temp_dir <- tempfile("qccheck_bm_invalid_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = "INVALID"),
    "batch_method.*must be one of"
  )
})

test_that("qcCheckR rejects non-character batch_method", {
  temp_dir <- tempfile("qccheck_bm_nonchar_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = 123),
    "batch_method.*must be one of"
  )
})

test_that("qcCheckR rejects NULL batch_method", {
  temp_dir <- tempfile("qccheck_bm_null_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = NULL),
    "batch_method.*must be one of"
  )
})

# ============================================================================
# Batch correction parameter validation: batch_ntree
# ============================================================================

test_that("qcCheckR accepts valid batch_ntree", {
  temp_dir <- tempfile("qccheck_bn_valid_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Valid positive integer should pass validation and fail downstream
  expect_error(
    suppressMessages(
      qcCheckR(user_name = "ANPC", project_directory = temp_dir,
               batch_ntree = 100)
    ),
    "No report files found"
  )
})

test_that("qcCheckR rejects zero batch_ntree", {
  temp_dir <- tempfile("qccheck_bn_zero_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_ntree = 0),
    "batch_ntree.*must be a positive integer"
  )
})

test_that("qcCheckR rejects negative batch_ntree", {
  temp_dir <- tempfile("qccheck_bn_neg_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_ntree = -10),
    "batch_ntree.*must be a positive integer"
  )
})

test_that("qcCheckR rejects non-integer batch_ntree", {
  temp_dir <- tempfile("qccheck_bn_float_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_ntree = 500.5),
    "batch_ntree.*must be a positive integer"
  )
})

test_that("qcCheckR rejects non-numeric batch_ntree", {
  temp_dir <- tempfile("qccheck_bn_char_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_ntree = "five hundred"),
    "batch_ntree.*must be a positive integer"
  )
})

# ============================================================================
# Batch correction parameter validation: batch_coCV
# ============================================================================

test_that("qcCheckR accepts valid batch_coCV values", {
  temp_dir <- tempfile("qccheck_coCV_valid_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  for (val in c(0, 30, 100)) {
    expect_error(
      suppressMessages(
        qcCheckR(user_name = "ANPC", project_directory = temp_dir,
                 batch_coCV = val)
      ),
      "No report files found"
    )
  }
})

test_that("qcCheckR rejects negative batch_coCV", {
  temp_dir <- tempfile("qccheck_coCV_neg_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_coCV = -1),
    "batch_coCV.*must be a non-negative numeric"
  )
})

test_that("qcCheckR rejects non-numeric batch_coCV", {
  temp_dir <- tempfile("qccheck_coCV_char_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_coCV = "high"),
    "batch_coCV.*must be a non-negative numeric"
  )
})

# ============================================================================
# Batch correction parameter validation: batch_Frule
# ============================================================================

test_that("qcCheckR accepts valid batch_Frule values", {
  temp_dir <- tempfile("qccheck_Frule_valid_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  for (val in c(0, 0.5, 1)) {
    expect_error(
      suppressMessages(
        qcCheckR(user_name = "ANPC", project_directory = temp_dir,
                 batch_Frule = val)
      ),
      "No report files found"
    )
  }
})

test_that("qcCheckR rejects batch_Frule greater than 1", {
  temp_dir <- tempfile("qccheck_Frule_high_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_Frule = 1.5),
    "batch_Frule.*must be a numeric value between 0 and 1"
  )
})

test_that("qcCheckR rejects negative batch_Frule", {
  temp_dir <- tempfile("qccheck_Frule_neg_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_Frule = -0.1),
    "batch_Frule.*must be a numeric value between 0 and 1"
  )
})

test_that("qcCheckR rejects non-numeric batch_Frule", {
  temp_dir <- tempfile("qccheck_Frule_char_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_Frule = "zero"),
    "batch_Frule.*must be a numeric value between 0 and 1"
  )
})

# ============================================================================
# Batch correction parameter validation: batch_imputeM
# ============================================================================

test_that("qcCheckR accepts valid batch_imputeM values", {
  temp_dir <- tempfile("qccheck_imputeM_valid_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  for (method in c("minHalf", "median", "mean", "knn")) {
    expect_error(
      suppressMessages(
        qcCheckR(user_name = "ANPC", project_directory = temp_dir,
                 batch_imputeM = method)
      ),
      "No report files found"
    )
  }
})

test_that("qcCheckR rejects invalid batch_imputeM string", {
  temp_dir <- tempfile("qccheck_imputeM_invalid_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_imputeM = "interpolate"),
    "batch_imputeM.*must be one of"
  )
})

test_that("qcCheckR rejects non-character batch_imputeM", {
  temp_dir <- tempfile("qccheck_imputeM_nonchar_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_imputeM = 42),
    "batch_imputeM.*must be one of"
  )
})

# ============================================================================
# ComBat-specific parameter validation
# ============================================================================

test_that("batch_method accepts ComBat", {
  temp_dir <- file.path(tempdir(), paste0("qccheck_combat_", gsub("[^a-z0-9]", "", tolower(tempfile("")))))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = "ComBat"),
    "No report files found"
  )
})

test_that("combat_par.prior must be logical", {
  temp_dir <- file.path(tempdir(), paste0("qccheck_combat_pp_", gsub("[^a-z0-9]", "", tolower(tempfile("")))))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = "ComBat", combat_par.prior = "yes"),
    "combat_par.prior"
  )
})

test_that("combat_mean.only must be logical", {
  temp_dir <- file.path(tempdir(), paste0("qccheck_combat_mo_", gsub("[^a-z0-9]", "", tolower(tempfile("")))))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = "ComBat", combat_mean.only = 1),
    "combat_mean.only"
  )
})

test_that("combat_ref.batch must be character or NULL", {
  temp_dir <- file.path(tempdir(), paste0("qccheck_combat_rb_", gsub("[^a-z0-9]", "", tolower(tempfile("")))))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = "ComBat", combat_ref.batch = 123),
    "combat_ref.batch"
  )
})

test_that("batch_column must be character or NULL", {
  temp_dir <- file.path(tempdir(), paste0("qccheck_combat_bc_", gsub("[^a-z0-9]", "", tolower(tempfile("")))))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = "ComBat", batch_column = 42),
    "batch_column"
  )
  expect_error(
    qcCheckR(user_name = "ANPC", project_directory = temp_dir,
             batch_method = "ComBat", batch_column = ""),
    "batch_column"
  )
})

