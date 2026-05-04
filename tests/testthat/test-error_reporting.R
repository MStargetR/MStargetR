# test-error_reporting.R
# Comprehensive tests for error reporting and input validation across all
# exported functions in MStargetR. Verifies that every exported function
# produces helpful, consistent error messages when given bad input.
#
# Conventions:
#   - Error messages should contain the function name and parameter name
#   - Error messages should describe what went wrong
#   - All stop() calls should use call. = FALSE


# ============================================================================
# Helper data constructors
# ============================================================================

# make_bc_data() is defined in helper-fixtures.R (auto-sourced by testthat)

#' Build a minimal valid transition data.frame
make_transition_df <- function(n = 5) {
  data.frame(
    `Molecule List Name` = rep("Lipids", n),
    `Precursor Name` = paste0("Met_", seq_len(n)),
    `Precursor Mz` = seq(100, by = 10, length.out = n),
    `Precursor Charge` = rep(1, n),
    `Product Mz` = seq(50, by = 5, length.out = n),
    `Product Charge` = rep(1, n),
    `Explicit Retention Time` = seq(1.0, by = 0.5, length.out = n),
    `Explicit Retention Time Window` = rep(0.5, n),
    `Note` = paste0("Note_", seq_len(n)),
    `control_chart` = rep(TRUE, n),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Build a minimal valid concentration guide data.frame
make_conc_guide <- function(sil_names = c("Note_1", "Note_2")) {
  data.frame(
    SIL_name = sil_names,
    concentration_factor = rep(1.0, length(sil_names)),
    stringsAsFactors = FALSE
  )
}


# ============================================================================
# 1. batchCorrectR() error reporting
# ============================================================================

test_that("batchCorrectR: wrong data types produce informative errors", {
  # Non-data.frame

  expect_error(
    batchCorrectR(data = "not_a_df"),
    regexp = "batchCorrectR.*data.*must be a data.frame"
  )

  expect_error(
    batchCorrectR(data = list(a = 1)),
    regexp = "batchCorrectR.*must be data.frames"
  )

  expect_error(
    batchCorrectR(data = 42),
    regexp = "batchCorrectR.*data.*must be a data.frame"
  )
})

test_that("batchCorrectR: missing required columns produce informative errors", {
  bad_df <- data.frame(x = 1, y = 2)
  expect_error(
    batchCorrectR(data = bad_df),
    regexp = "batchCorrectR.*Missing required column"
  )

  # Missing just one column
  partial_df <- data.frame(
    sample_name = "S1", batch = "B1", sample_type = "qc",
    stringsAsFactors = FALSE
  )
  expect_error(
    batchCorrectR(data = partial_df),
    regexp = "batchCorrectR.*Missing required column.*run_order"
  )
})

test_that("batchCorrectR: invalid method produces informative error", {
  df <- make_bc_data()
  expect_error(
    batchCorrectR(data = df, method = "INVALID"),
    regexp = "batchCorrectR.*Invalid.*method.*INVALID"
  )
})

test_that("batchCorrectR: out-of-range numeric params produce informative errors", {
  df <- make_bc_data()

  # ntree must be positive

  expect_error(
    batchCorrectR(data = df, ntree = -5),
    regexp = "batchCorrectR.*ntree.*must be a positive"
  )

  # coCV must be positive

  expect_error(
    batchCorrectR(data = df, coCV = -1),
    regexp = "batchCorrectR.*coCV.*must be a positive"
  )

  # Frule must be between 0 and 1
  expect_error(
    batchCorrectR(data = df, Frule = 2),
    regexp = "batchCorrectR.*Frule.*must be between 0 and 1"
  )

  # Invalid imputeM
  expect_error(
    batchCorrectR(data = df, imputeM = "bogus"),
    regexp = "batchCorrectR.*Invalid.*imputeM.*bogus"
  )
})

test_that("batchCorrectR: non-numeric run_order produces informative error", {
  df <- make_bc_data()
  df$run_order <- as.character(df$run_order)
  expect_error(
    batchCorrectR(data = df),
    regexp = "batchCorrectR.*run_order.*must be numeric"
  )
})

test_that("batchCorrectR: duplicate sample_name produces informative error", {
  df <- make_bc_data()
  df$sample_name[2] <- df$sample_name[1]
  expect_error(
    batchCorrectR(data = df),
    regexp = "batchCorrectR.*sample_name.*must be unique"
  )
})

test_that("batchCorrectR: no QC samples produces informative error", {
  df <- make_bc_data()
  df$sample_type <- "sample"
  expect_error(
    batchCorrectR(data = df),
    regexp = "batchCorrectR.*No QC samples found"
  )
})


# ============================================================================
# 2. qcCheckR() error reporting
# ============================================================================

test_that("qcCheckR: missing user_name produces informative error", {
  suppressMessages({
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    expect_error(
      qcCheckR(project_directory = temp_dir),
      regexp = "qcCheckR.*user_name.*required"
    )
  })
})

test_that("qcCheckR: invalid directory produces informative error", {
  suppressMessages({
    expect_error(
      qcCheckR(user_name = "test", project_directory = NULL),
      regexp = "project_directory.*must be a single"
    )

    expect_error(
      qcCheckR(user_name = "test",
               project_directory = file.path(tempdir(), "nonexistent_dir_12345")),
      regexp = "project directory does not exist"
    )
  })
})

test_that("qcCheckR: missing templates produce informative error", {
  suppressMessages({
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    expect_error(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               QC_sample_label = "qc", sample_tags = c("sample")),
      regexp = "qcCheckR.*mrm_template_list.*required"
    )
  })
})

test_that("qcCheckR: invalid mv_threshold produces informative error", {
  suppressMessages({
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    expect_error(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(a = 1), QC_sample_label = "qc",
               sample_tags = c("sample"), mv_threshold = 150),
      regexp = "qcCheckR.*mv_threshold.*must be.*numeric.*between 0 and 100"
    )

    expect_error(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(a = 1), QC_sample_label = "qc",
               sample_tags = c("sample"), mv_threshold = "fifty"),
      regexp = "qcCheckR.*mv_threshold.*must be.*numeric"
    )

    expect_error(
      qcCheckR(user_name = "user", project_directory = temp_dir,
               mrm_template_list = list(a = 1), QC_sample_label = "qc",
               sample_tags = c("sample"), mv_threshold = -10),
      regexp = "qcCheckR.*mv_threshold.*must be.*numeric.*between 0 and 100"
    )
  })
})

test_that("qcCheckR: non-character user_name produces informative error", {
  suppressMessages({
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    expect_error(
      qcCheckR(user_name = 123, project_directory = temp_dir),
      regexp = "qcCheckR.*user_name.*must be.*character"
    )

    expect_error(
      qcCheckR(user_name = "", project_directory = temp_dir),
      regexp = "qcCheckR.*user_name.*must be.*non-empty"
    )
  })
})


# ============================================================================
# 3. PeakForgeR() error reporting
# ============================================================================

test_that("PeakForgeR: empty user_name produces informative error", {
  expect_error(
    PeakForgeR(user_name = "", project_directory = tempdir()),
    regexp = "PeakForgeR.*user_name.*must be.*non-empty"
  )
})

test_that("PeakForgeR: non-string user_name produces informative error", {
  expect_error(
    PeakForgeR(user_name = 123, project_directory = tempdir()),
    regexp = "PeakForgeR.*user_name.*must be.*character"
  )
})

test_that("PeakForgeR: non-existent directory produces informative error", {
  fake_dir <- file.path(tempdir(), "nonexistent_PeakForgeR_test_12345")
  expect_error(
    PeakForgeR(user_name = "test", project_directory = fake_dir),
    regexp = "project directory does not exist"
  )
})

test_that("PeakForgeR: invalid QC_sample_label produces informative error", {
  suppressMessages({
    temp_dir <- tempfile()
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    # When QC_sample_label = NULL, validate_mrm_template_list fires first
    # because mrm_template_list defaults to NULL for non-ANPC users.
    # So we test with a valid mrm_template_list to reach the QC check.
    expect_error(
      PeakForgeR(user_name = "test", project_directory = temp_dir,
                 mrm_template_list = list(v1 = make_transition_df()),
                 QC_sample_label = NULL),
      regexp = "PeakForgeR.*QC_sample_label.*must be.*non-empty"
    )

    expect_error(
      PeakForgeR(user_name = "test", project_directory = temp_dir,
                 mrm_template_list = list(v1 = make_transition_df()),
                 QC_sample_label = ""),
      regexp = "PeakForgeR.*QC_sample_label.*must be.*non-empty"
    )

    expect_error(
      PeakForgeR(user_name = "test", project_directory = temp_dir,
                 mrm_template_list = list(v1 = make_transition_df()),
                 QC_sample_label = 42),
      regexp = "PeakForgeR.*QC_sample_label.*must be.*character"
    )
  })
})


# ============================================================================
# 4. msConvertR() error reporting
# ============================================================================

test_that("msConvertR: non-string input_directory produces informative error", {
  expect_error(
    msConvertR(input_directory = 123, output_directory = tempdir()),
    regexp = "input_directory.*must be a single character"
  )

  expect_error(
    msConvertR(input_directory = c("dir1", "dir2"), output_directory = tempdir()),
    regexp = "input_directory.*must be a single character"
  )
})

test_that("msConvertR: non-existent input directory produces informative error", {
  fake_dir <- file.path(tempdir(), "nonexistent_msconvert_test_12345")
  expect_error(
    msConvertR(input_directory = fake_dir, output_directory = tempdir()),
    regexp = "input directory does not exist"
  )
})

test_that("msConvertR: non-string output_directory produces informative error", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create raw_data subdirectory so validate_input_directory passes
  dir.create(file.path(temp_dir, "raw_data"), showWarnings = FALSE)

  expect_error(
    msConvertR(input_directory = temp_dir, output_directory = 123),
    regexp = "msConvertR.*output_directory.*must be a single character"
  )

  expect_error(
    msConvertR(input_directory = temp_dir, output_directory = ""),
    regexp = "msConvertR.*output_directory.*must not be.*empty"
  )
})

test_that("msConvertR: empty string input_directory produces informative error", {
  expect_error(
    msConvertR(input_directory = "", output_directory = tempdir()),
    regexp = "input_directory.*must not be.*empty"
  )
})


# ============================================================================
# 5. transition_checkR() error reporting
# ============================================================================

test_that("transition_checkR: non-dataframe input produces informative error", {
  expect_error(
    transition_checkR("not_a_df"),
    regexp = "transition_checkR.*transition_df.*must be a data.frame"
  )

  expect_error(
    transition_checkR(42),
    regexp = "transition_checkR.*transition_df.*must be a data.frame"
  )

  expect_error(
    transition_checkR(list(a = 1)),
    regexp = "transition_checkR.*transition_df.*must be a data.frame"
  )
})

test_that("transition_checkR: empty dataframe produces informative error", {
  empty_df <- data.frame(
    `Precursor Mz` = numeric(0),
    `Product Mz` = numeric(0),
    `Precursor Name` = character(0),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_error(
    transition_checkR(empty_df),
    regexp = "transition_checkR.*transition_df.*must not be empty"
  )
})

test_that("transition_checkR: missing columns produces informative error", {
  bad_df <- data.frame(x = 1, y = 2)
  expect_error(
    transition_checkR(bad_df),
    regexp = "transition_checkR.*transition_df.*missing required column"
  )
})

test_that("transition_checkR: non-numeric Q1/Q3 columns produce informative error", {
  df <- make_transition_df()
  df$`Precursor Mz` <- as.character(df$`Precursor Mz`)
  expect_error(
    transition_checkR(df),
    regexp = "transition_checkR.*Precursor Mz.*must be numeric"
  )

  df2 <- make_transition_df()
  df2$`Product Mz` <- as.character(df2$`Product Mz`)
  expect_error(
    transition_checkR(df2),
    regexp = "transition_checkR.*Product Mz.*must be numeric"
  )
})

test_that("transition_checkR: valid input produces no error", {
  df <- make_transition_df()
  expect_no_error(suppressMessages(transition_checkR(df)))
})


# ============================================================================
# 6. compare_mrm_template_with_guide() error reporting
# ============================================================================

test_that("compare_mrm_template_with_guide: non-dataframe mrm_template produces error", {
  cg <- make_conc_guide()
  expect_error(
    compare_mrm_template_with_guide("not_a_df", cg),
    regexp = "compare_mrm_template_with_guide.*mrm_template.*must be a data.frame"
  )
})

test_that("compare_mrm_template_with_guide: non-dataframe concentration_guide produces error", {
  df <- make_transition_df()
  expect_error(
    compare_mrm_template_with_guide(df, "not_a_df"),
    regexp = "compare_mrm_template_with_guide.*concentration_guide.*must be a data.frame"
  )
})

test_that("compare_mrm_template_with_guide: missing columns produce informative errors", {
  df_no_note <- data.frame(x = 1)
  cg <- make_conc_guide()
  expect_error(
    compare_mrm_template_with_guide(df_no_note, cg),
    regexp = "compare_mrm_template_with_guide.*mrm_template.*missing required column.*Note"
  )

  df <- make_transition_df()
  cg_no_sil <- data.frame(x = 1)
  expect_error(
    compare_mrm_template_with_guide(df, cg_no_sil),
    regexp = "compare_mrm_template_with_guide.*concentration_guide.*missing required column.*SIL_name"
  )
})

test_that("compare_mrm_template_with_guide: valid input works", {
  df <- make_transition_df(n = 3)
  cg <- make_conc_guide(sil_names = df$Note)
  expect_no_error(suppressMessages(
    compare_mrm_template_with_guide(df, cg)
  ))
})


# ============================================================================
# 7. launchMStargetR() error reporting
# ============================================================================

test_that("launchMStargetR: invalid port produces informative error", {
  expect_error(
    launchMStargetR(port = "abc"),
    regexp = "launchMStargetR.*port.*must be.*integer"
  )

  expect_error(
    launchMStargetR(port = -1),
    regexp = "launchMStargetR.*port.*must be.*integer.*between 1 and 65535"
  )

  expect_error(
    launchMStargetR(port = 99999),
    regexp = "launchMStargetR.*port.*must be.*integer.*between 1 and 65535"
  )

  expect_error(
    launchMStargetR(port = c(3000, 4000)),
    regexp = "launchMStargetR.*port.*must be.*integer"
  )
})

test_that("launchMStargetR: invalid launch.browser produces informative error", {
  expect_error(
    launchMStargetR(launch.browser = "yes"),
    regexp = "launchMStargetR.*launch.browser.*must be TRUE or FALSE"
  )

  expect_error(
    launchMStargetR(launch.browser = NA),
    regexp = "launchMStargetR.*launch.browser.*must be TRUE or FALSE"
  )
})

test_that("launchMStargetR: invalid host produces informative error", {
  expect_error(
    launchMStargetR(host = 123),
    regexp = "launchMStargetR.*host.*must be.*character"
  )

  expect_error(
    launchMStargetR(host = ""),
    regexp = "launchMStargetR.*host.*must be.*non-empty"
  )
})


# ============================================================================
# 8. config.R internal functions error reporting
# ============================================================================

test_that("update_script_log: non-list master_list produces informative error", {
  expect_error(
    update_script_log("not_a_list", "s1", "start", "s2"),
    regexp = "update_script_log.*master_list.*must be a list"
  )
})

test_that("update_script_log: missing script_log structure produces informative error", {
  expect_error(
    update_script_log(list(), "s1", "start", "s2"),
    regexp = "update_script_log.*master_list.*script_log"
  )
})

test_that("update_script_log: non-character section_name produces informative error", {
  ml <- list(project_details = list(
    script_log = list(timestamps = list(start_time = Sys.time()),
                      runtimes = list(), messages = list())
  ))
  expect_error(
    update_script_log(ml, 123, "start_time", "s2"),
    regexp = "update_script_log.*section_name.*must be.*character"
  )
})

test_that("validate_previous_section: missing section provides available sections", {
  ml <- list(project_details = list(
    script_log = list(timestamps = list(start_time = Sys.time()))
  ))
  expect_error(
    validate_previous_section(ml, "nonexistent"),
    regexp = "validate_previous_section.*nonexistent.*not found.*start_time"
  )
})

test_that("validate_project_directory: error messages include function name", {
  expect_error(
    validate_project_directory(123),
    regexp = "validate_project_directory.*project_directory.*must be"
  )

  expect_error(
    validate_project_directory(""),
    regexp = "validate_project_directory.*must not be.*empty"
  )

  expect_error(
    validate_project_directory(file.path(tempdir(), "nonexistent_99999")),
    regexp = "validate_project_directory.*does not exist"
  )
})

test_that("replace_precursor_symbols: validates input types", {
  expect_error(
    replace_precursor_symbols("not_a_df"),
    regexp = "replace_precursor_symbols.*mrm_template.*must be a data.frame"
  )

  df <- data.frame(x = "a/b", stringsAsFactors = FALSE)
  expect_error(
    replace_precursor_symbols(df, columns = c("Precursor Name")),
    regexp = "replace_precursor_symbols.*missing column.*Precursor Name"
  )
})


# ============================================================================
# 9. Cross-cutting: call. = FALSE (no call stack in errors)
# ============================================================================

test_that("Error messages do not contain call stack (call. = FALSE)", {
  # transition_checkR
  err <- tryCatch(transition_checkR("bad"), error = function(e) e)
  expect_null(err$call)

  # compare_mrm_template_with_guide
  err <- tryCatch(
    compare_mrm_template_with_guide("bad", data.frame()),
    error = function(e) e
  )
  expect_null(err$call)

  # launchMStargetR
  err <- tryCatch(launchMStargetR(port = "bad"), error = function(e) e)
  expect_null(err$call)

  # validate_project_directory
  err <- tryCatch(validate_project_directory(123), error = function(e) e)
  expect_null(err$call)

  # update_script_log
  err <- tryCatch(
    update_script_log("bad", "s1", "s2", "s3"),
    error = function(e) e
  )
  expect_null(err$call)
})
