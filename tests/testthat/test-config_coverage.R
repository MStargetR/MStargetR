# Tests for config.R coverage gaps ----
library(mockery)

# ============================================================================
# sanitize_identifier()
# ============================================================================

test_that("sanitize_identifier returns sanitized string for valid input", {
  expect_equal(sanitize_identifier("hello_world"), "hello_world")
  expect_equal(sanitize_identifier("test.name"), "test.name")
  expect_equal(sanitize_identifier("user@domain"), "user@domain")
  expect_equal(sanitize_identifier("with spaces"), "with spaces")
  expect_equal(sanitize_identifier("parens(1)"), "parens(1)")
})

test_that("sanitize_identifier replaces unsafe characters", {
  expect_equal(sanitize_identifier("name;drop"), "name_drop")
  expect_equal(sanitize_identifier("pipe|char"), "pipe_char")
  expect_equal(sanitize_identifier("quote'test"), "quote_test")
})

test_that("sanitize_identifier rejects non-character input", {
  expect_error(
    sanitize_identifier(123),
    "sanitize_identifier.*must be a non-empty string"
  )
  expect_error(
    sanitize_identifier(NULL),
    "sanitize_identifier.*must be a non-empty string"
  )
  expect_error(
    sanitize_identifier(TRUE),
    "sanitize_identifier.*must be a non-empty string"
  )
})

test_that("sanitize_identifier rejects multi-element vector", {
  expect_error(
    sanitize_identifier(c("a", "b")),
    "sanitize_identifier.*must be a non-empty string"
  )
})

test_that("sanitize_identifier rejects empty string", {
  expect_error(
    sanitize_identifier(""),
    "sanitize_identifier.*must be a non-empty string"
  )
})

test_that("sanitize_identifier detects path traversal with ..", {
  expect_error(
    sanitize_identifier("../../etc"),
    "sanitize_identifier.*invalid path characters"
  )
  expect_error(
    sanitize_identifier("name..traversal"),
    "sanitize_identifier.*invalid path characters"
  )
})

test_that("sanitize_identifier detects path separators", {
  expect_error(
    sanitize_identifier("dir/subdir"),
    "sanitize_identifier.*invalid path characters"
  )
  expect_error(
    sanitize_identifier("dir\\subdir"),
    "sanitize_identifier.*invalid path characters"
  )
})

test_that("sanitize_identifier replaces control characters with underscores", {
  # Control chars get replaced with _, result is non-empty
  result <- sanitize_identifier("\x01A\x02")
  expect_equal(result, "_A_")
})

# ============================================================================
# update_script_log() - input validation branches
# ============================================================================

test_that("update_script_log rejects non-character previous_section_name", {
  ml <- list(project_details = list(
    script_log = list(timestamps = list(start_time = Sys.time()),
                      runtimes = list(), messages = list())
  ))
  expect_error(
    update_script_log(ml, "s1", 123, "s2"),
    "update_script_log.*previous_section_name.*must be.*character"
  )
})

test_that("update_script_log rejects non-character next_section_name", {
  ml <- list(project_details = list(
    script_log = list(timestamps = list(start_time = Sys.time()),
                      runtimes = list(), messages = list())
  ))
  expect_error(
    update_script_log(ml, "s1", "start_time", 42),
    "update_script_log.*next_section_name.*must be.*character"
  )
})

# ============================================================================
# calculate_runtime() - error branches
# ============================================================================

test_that("calculate_runtime errors when section_name timestamp missing", {
  ml <- list(project_details = list(
    script_log = list(
      timestamps = list(start_time = Sys.time()),
      runtimes = list()
    )
  ))
  expect_error(
    calculate_runtime(ml, "nonexistent", "start_time"),
    "calculate_runtime.*section_name.*nonexistent.*not found"
  )
})

test_that("calculate_runtime errors when previous_section_name timestamp missing", {
  ml <- list(project_details = list(
    script_log = list(
      timestamps = list(section_1 = Sys.time()),
      runtimes = list()
    )
  ))
  expect_error(
    calculate_runtime(ml, "section_1", "nonexistent"),
    "calculate_runtime.*previous_section_name.*nonexistent.*not found"
  )
})

# ============================================================================
# validate_master_list_project_directory() - NULL project_dir
# ============================================================================

test_that("validate_master_list_project_directory errors when project_dir is NULL", {
  ml <- list(project_details = list(project_dir = NULL))
  expect_error(
    validate_master_list_project_directory(ml),
    "Project directory is not set"
  )
})

# ============================================================================
# log_error()
# ============================================================================

test_that("log_error writes to log file", {
  tmp <- withr::local_tempdir()
  log_error("Test error message", "plate1", project_directory = tmp)

  log_file <- file.path(tmp, "MStargetR_logs", "plate1_MStargetR_log.txt")
  expect_true(file.exists(log_file))
  content <- readLines(log_file)
  expect_true(any(grepl("Test error message", content)))
})

test_that("log_error creates MStargetR_logs dir if missing", {
  tmp <- withr::local_tempdir()
  log_dir <- file.path(tmp, "MStargetR_logs")
  expect_false(dir.exists(log_dir))

  log_error("msg", "p1", project_directory = tmp)
  expect_true(dir.exists(log_dir))
})

test_that("log_error appends to existing log file", {
  tmp <- withr::local_tempdir()
  log_error("first", "plate1", project_directory = tmp)
  log_error("second", "plate1", project_directory = tmp)

  log_file <- file.path(tmp, "MStargetR_logs", "plate1_MStargetR_log.txt")
  content <- readLines(log_file)
  expect_true(any(grepl("first", content)))
  expect_true(any(grepl("second", content)))
})

test_that("log_error rejects non-character error_message", {
  expect_error(
    log_error(123, "plate1"),
    "log_error.*error_message.*must be a single character"
  )
  expect_error(
    log_error(c("a", "b"), "plate1"),
    "log_error.*error_message.*must be a single character"
  )
})

test_that("log_error rejects non-character plateID", {
  expect_error(
    log_error("msg", 123),
    "log_error.*plateID.*must be a single character"
  )
  expect_error(
    log_error("msg", c("a", "b")),
    "log_error.*plateID.*must be a single character"
  )
})

test_that("log_error rejects non-character project_directory", {
  expect_error(
    log_error("msg", "plate1", project_directory = 123),
    "log_error.*project_directory.*must be a single character"
  )
})

test_that("log_error rejects non-existent project_directory", {
  expect_error(
    log_error("msg", "plate1", project_directory = "/nonexistent/dir/xyz"),
    "log_error.*project_directory.*does not exist"
  )
})

# ============================================================================
# check_docker()
# ============================================================================

test_that("check_docker errors when Docker is not installed", {
  stub(check_docker, "system2", function(...) stop("not found"))

  expect_error(
    check_docker(),
    "Docker is not installed"
  )
})

test_that("check_docker errors when Docker daemon is not running", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) {
      # docker --version succeeds
      return("Docker version 24.0.0")
    }
    if (call_count == 2L) {
      # docker info returns non-zero exit status (authoritative signal)
      result <- character(0)
      attr(result, "status") <- 1L
      return(result)
    }
    return("")
  })

  expect_error(
    check_docker(),
    "Docker is installed but the daemon is not running"
  )
})

test_that("check_docker succeeds when image exists locally", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) return("Docker version 24.0.0")
    if (call_count == 2L) return("Server: Docker Engine")
    if (call_count == 3L) return("abc123def456")
    return("")
  })

  result <- check_docker()
  expect_true(is.null(result) || isTRUE(result))
  expect_equal(call_count, 3L)
})

test_that("check_docker pulls image when not found locally", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) return("Docker version 24.0.0")
    if (call_count == 2L) return("Server: Docker Engine")
    if (call_count == 3L) return(character(0))  # image not found
    if (call_count == 4L) {
      result <- "Pull complete"
      attr(result, "status") <- 0L
      return(result)
    }
    return("")
  })

  expect_message(check_docker(auto_pull = TRUE), "Pulling ProteoWizard Docker image")
})

test_that("check_docker errors when image pull fails", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) return("Docker version 24.0.0")
    if (call_count == 2L) return("Server: Docker Engine")
    if (call_count == 3L) return(character(0))  # image not found
    if (call_count == 4L) {
      result <- "Error: pull failed"
      attr(result, "status") <- 1L
      return(result)
    }
    return("")
  })

  expect_error(
    suppressMessages(check_docker(auto_pull = TRUE)),
    "Failed to pull Docker image"
  )
})

# ============================================================================
# validate_qcCheckR_mrm_template_list()
# ============================================================================

test_that("validate_qcCheckR_mrm_template_list errors for non-list input", {
  ml <- list(templates = list(mrm_guides = "not_a_list"))
  expect_error(
    validate_qcCheckR_mrm_template_list(ml),
    "mrm_template_list must be a list"
  )
})

test_that("validate_qcCheckR_mrm_template_list errors when version is not a list", {
  ml <- list(templates = list(mrm_guides = list(v1 = "not_a_list")))
  expect_error(
    validate_qcCheckR_mrm_template_list(ml),
    "Each version in mrm_template_list must be a list"
  )
})

test_that("validate_qcCheckR_mrm_template_list errors for missing SIL_guide", {
  ml <- list(templates = list(mrm_guides = list(
    v1 = list(conc_guide = data.frame(concentration_factor = 1, SIL_name = "A"))
  )))
  expect_error(
    validate_qcCheckR_mrm_template_list(ml),
    "Missing SIL_guide"
  )
})

test_that("validate_qcCheckR_mrm_template_list errors for missing conc_guide", {
  sil <- data.frame(
    `Molecule List Name` = "A", `Precursor Name` = "B",
    `Precursor Mz` = 100, `Precursor Charge` = 1,
    `Product Mz` = 50, `Product Charge` = 1,
    `Explicit Retention Time` = 1.0,
    `Explicit Retention Time Window` = 0.5,
    `Note` = "N1", `control_chart` = TRUE,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  ml <- list(templates = list(mrm_guides = list(v1 = list(SIL_guide = sil))))
  expect_error(
    validate_qcCheckR_mrm_template_list(ml),
    "Missing conc_guide"
  )
})

test_that("validate_qcCheckR_mrm_template_list errors for missing SIL columns", {
  sil <- data.frame(x = 1)
  cg <- data.frame(concentration_factor = 1, SIL_name = "A")
  ml <- list(templates = list(mrm_guides = list(
    v1 = list(SIL_guide = sil, conc_guide = cg)
  )))
  expect_error(
    validate_qcCheckR_mrm_template_list(ml),
    "Missing required columns"
  )
})

test_that("validate_qcCheckR_mrm_template_list errors for NA values in SIL check cols", {
  sil <- data.frame(
    `Molecule List Name` = "A", `Precursor Name` = NA,
    `Precursor Mz` = 100, `Precursor Charge` = 1,
    `Product Mz` = 50, `Product Charge` = 1,
    `Explicit Retention Time` = 1.0,
    `Explicit Retention Time Window` = 0.5,
    `Note` = "N1", `control_chart` = TRUE,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  cg <- data.frame(concentration_factor = 1, SIL_name = "N1")
  ml <- list(templates = list(mrm_guides = list(
    v1 = list(SIL_guide = sil, conc_guide = cg)
  )))
  expect_error(
    validate_qcCheckR_mrm_template_list(ml),
    "NA values found"
  )
})

test_that("validate_qcCheckR_mrm_template_list errors for non-unique transitions", {
  sil <- data.frame(
    `Molecule List Name` = c("A", "B"), `Precursor Name` = c("P1", "P2"),
    `Precursor Mz` = c(100, 100), `Precursor Charge` = c(1, 1),
    `Product Mz` = c(50, 50), `Product Charge` = c(1, 1),
    `Explicit Retention Time` = c(1.0, 1.0),
    `Explicit Retention Time Window` = c(0.5, 0.5),
    `Note` = c("N1", "N2"), `control_chart` = c(TRUE, TRUE),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  cg <- data.frame(concentration_factor = c(1, 1), SIL_name = c("N1", "N2"))
  ml <- list(templates = list(mrm_guides = list(
    v1 = list(SIL_guide = sil, conc_guide = cg)
  )))
  expect_error(
    suppressMessages(validate_qcCheckR_mrm_template_list(ml)),
    "Non-unique transitions"
  )
})

test_that("validate_qcCheckR_mrm_template_list errors for missing conc_guide columns", {
  sil <- data.frame(
    `Molecule List Name` = "A", `Precursor Name` = "P1",
    `Precursor Mz` = 100, `Precursor Charge` = 1,
    `Product Mz` = 50, `Product Charge` = 1,
    `Explicit Retention Time` = 1.0,
    `Explicit Retention Time Window` = 0.5,
    `Note` = "N1", `control_chart` = TRUE,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  cg <- data.frame(x = 1)
  ml <- list(templates = list(mrm_guides = list(
    v1 = list(SIL_guide = sil, conc_guide = cg)
  )))
  expect_error(
    suppressMessages(validate_qcCheckR_mrm_template_list(ml)),
    "Missing required columns"
  )
})

test_that("validate_qcCheckR_mrm_template_list errors for unmatched Note values", {
  sil <- data.frame(
    `Molecule List Name` = "A", `Precursor Name` = "P1",
    `Precursor Mz` = 100, `Precursor Charge` = 1,
    `Product Mz` = 50, `Product Charge` = 1,
    `Explicit Retention Time` = 1.0,
    `Explicit Retention Time Window` = 0.5,
    `Note` = "N1", `control_chart` = TRUE,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  cg <- data.frame(concentration_factor = 1, SIL_name = "UNMATCHED")
  ml <- list(templates = list(mrm_guides = list(
    v1 = list(SIL_guide = sil, conc_guide = cg)
  )))
  expect_error(
    suppressMessages(validate_qcCheckR_mrm_template_list(ml)),
    "Unmatched Note values"
  )
})

test_that("validate_qcCheckR_mrm_template_list returns TRUE for valid input", {
  sil <- data.frame(
    `Molecule List Name` = "A", `Precursor Name` = "P1",
    `Precursor Mz` = 100, `Precursor Charge` = 1,
    `Product Mz` = 50, `Product Charge` = 1,
    `Explicit Retention Time` = 1.0,
    `Explicit Retention Time Window` = 0.5,
    `Note` = "N1", `control_chart` = TRUE,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  cg <- data.frame(concentration_factor = 1, SIL_name = "N1")
  ml <- list(templates = list(mrm_guides = list(
    v1 = list(SIL_guide = sil, conc_guide = cg)
  )))
  expect_message(
    result <- validate_qcCheckR_mrm_template_list(ml),
    "Validation passed"
  )
  expect_true(result)
})

# ============================================================================
# replace_precursor_symbols() - happy path
# ============================================================================

test_that("replace_precursor_symbols replaces / with _", {
  df <- data.frame(
    `Precursor Name` = "LPC/LPE", `Note` = "test/note",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  result <- replace_precursor_symbols(df)
  expect_equal(result$`Precursor Name`, "LPC_LPE")
  expect_equal(result$Note, "test_note")
})

test_that("replace_precursor_symbols replaces backslash with _", {
  df <- data.frame(
    `Precursor Name` = "LPC\\LPE", `Note` = "test\\note",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  result <- replace_precursor_symbols(df)
  expect_equal(result$`Precursor Name`, "LPC_LPE")
  expect_equal(result$Note, "test_note")
})

test_that("replace_precursor_symbols preserves originals in original_ columns", {
  df <- data.frame(
    `Precursor Name` = "A/B", `Note` = "C/D",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  result <- replace_precursor_symbols(df)
  expect_equal(result$original_Precursor_Name, "A/B")
  expect_equal(result$original_Note, "C/D")
})

test_that("replace_precursor_symbols errors for non-character columns", {
  df <- data.frame(`Precursor Name` = "A", Note = "B",
                    check.names = FALSE, stringsAsFactors = FALSE)
  expect_error(
    replace_precursor_symbols(df, columns = 123),
    "replace_precursor_symbols.*columns.*must be a non-empty character"
  )
})

test_that("replace_precursor_symbols errors for empty columns vector", {
  df <- data.frame(`Precursor Name` = "A", Note = "B",
                    check.names = FALSE, stringsAsFactors = FALSE)
  expect_error(
    replace_precursor_symbols(df, columns = character(0)),
    "replace_precursor_symbols.*columns.*must be a non-empty character"
  )
})

# ============================================================================
# check_dir_exists() / create_dir()
# ============================================================================

test_that("check_dir_exists returns TRUE for existing directory", {
  expect_true(check_dir_exists(tempdir()))
})

test_that("check_dir_exists returns FALSE for non-existent directory", {
  expect_false(check_dir_exists(file.path(tempdir(), "nonexistent_xyz_123")))
})

test_that("create_dir creates a new directory", {
  tmp <- withr::local_tempdir()
  new_dir <- file.path(tmp, "new_subdir")
  expect_false(dir.exists(new_dir))
  create_dir(new_dir)
  expect_true(dir.exists(new_dir))
})

test_that("create_dir creates nested directories", {
  tmp <- withr::local_tempdir()
  nested <- file.path(tmp, "a", "b", "c")
  create_dir(nested)
  expect_true(dir.exists(nested))
})

# ============================================================================
# check_docker() - line 564: daemon check tryCatch error branch
# ============================================================================

test_that("check_docker stops when docker info throws an error (line 564)", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) {
      # docker --version succeeds
      return("Docker version 24.0.0")
    }
    if (call_count == 2L) {
      # docker info throws an error (daemon not reachable)
      stop("Cannot connect to the Docker daemon")
    }
    return("")
  })

  expect_error(
    check_docker(),
    "Docker is installed but the daemon is not running"
  )
})

# ============================================================================
# validate_file_types() - lines 639-641: .d directory branch
# ============================================================================

test_that("validate_file_types validates .d directories (lines 640-641)", {
  tmp <- withr::local_tempdir()
  raw_dir <- file.path(tmp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)


  # Create a .d directory (Agilent-style vendor directory)
  d_dir <- file.path(raw_dir, "sample1.d")
  dir.create(d_dir, recursive = TRUE)

  # Also create a normal supported file for comparison
  file.create(file.path(raw_dir, "sample2.raw"))

  result <- suppressMessages(validate_file_types(tmp))

  # validate_file_types returns a character vector of validated file paths
  expect_true(any(grepl("sample1\\.d$", result)))
  expect_true(any(grepl("sample2\\.raw$", result)))
})

# ============================================================================
# calculate_total_runtime() - missing start_time guard
# ============================================================================

test_that("calculate_total_runtime errors when start_time is missing", {
  ml <- list(project_details = list(
    script_log = list(
      timestamps = list(section_1 = Sys.time()),
      runtimes = list()
    )
  ))
  expect_error(
    calculate_total_runtime(ml, "section_1"),
    "start_time.*timestamp is missing"
  )
})

# ============================================================================
# validate_project_directory() - verbose=FALSE suppresses message
# ============================================================================

test_that("validate_project_directory with verbose=FALSE emits no message", {
  tmp <- tempdir()
  expect_no_message(validate_project_directory(tmp, verbose = FALSE))
})
