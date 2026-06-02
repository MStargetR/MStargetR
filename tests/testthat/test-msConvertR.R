library(mockery)
library(stringr)

# Build a minimal plate-grouping table (as returned by derive_plate_groups())
# for stubbing msConvertR's top-level flow. Defaults to flat .wiff plates,
# which are plate_level == TRUE and therefore never trigger refuse-and-prompt.
fake_groups <- function(plates = "sample1", source = "flat", plate_level = TRUE,
                        rel_dir = "", is_dir = FALSE, ext = "wiff") {
  data.frame(
    raw_path          = file.path("raw_data", rel_dir, paste0(plates, ".", ext)),
    file_name         = paste0(plates, ".", ext),
    rel_dir           = rel_dir,
    raw_plateID       = plates,
    sanitized_plateID = plates,
    is_dir            = is_dir,
    plate_level       = plate_level,
    source            = source,
    stringsAsFactors  = FALSE
  )
}

test_that("msConvertR runs successfully with valid input", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  stub(msConvertR, "derive_plate_groups",
       function(...) fake_groups(c("sample1", "sample2")))
  stub(msConvertR, "check_docker", function() TRUE)
  stub(msConvertR, "msConvertR_mzml_conversion", function(...) TRUE)

  expect_message(
    msConvertR("input_dir", "output_dir"),
    "Converted mzML files are located"
    )
  })
})

test_that("msConvertR stops when no vendor files are found", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  # derive_plate_groups() surfaces validate_file_types()'s error when raw_data
  # contains no supported vendor files.
  stub(msConvertR, "derive_plate_groups",
       function(...) stop("validate_file_types: No supported vendor files found"))

  expect_error(
    msConvertR("input_dir", "output_dir"),
    "No supported vendor files"
  )
  })
})

test_that("msConvertR refuses ambiguous flat single-sample files", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  stub(msConvertR, "check_docker", function() TRUE)
  # Two flat .raw files (single-sample, no grouping) -> ambiguous.
  stub(msConvertR, "derive_plate_groups",
       function(...) fake_groups(c("s1", "s2"), source = "flat",
                                 plate_level = FALSE, ext = "raw"))

  expect_error(
    msConvertR("input_dir", "output_dir"),
    "no plate grouping"
  )
  })
})

test_that("msConvertR handles errors during conversion gracefully", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  stub(msConvertR, "derive_plate_groups", function(...) fake_groups("sample1"))
  stub(msConvertR, "check_docker", function() TRUE)
  stub(msConvertR, "msConvertR_mzml_conversion", function(...) stop("Conversion failed"))

  expect_error(
    msConvertR("input_dir", "output_dir"),
    "msConvertR conversion failed: Conversion failed"
  )
  })
})

test_that("msConvertR gives correct message when input and output directories are the same", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  stub(msConvertR, "derive_plate_groups", function(...) fake_groups("sample1"))
  stub(msConvertR, "check_docker", function() TRUE)
  stub(msConvertR, "msConvertR_mzml_conversion", function(...) TRUE)

  expect_message(
    msConvertR("same_dir", "same_dir"),
    "Input and output directories are the same"
  )
  })
})

test_that("msConvertR gives correct message when input and output directories are different", {
  suppressMessages({
  stub(msConvertR, "validate_input_directory", function(dir) TRUE)
  stub(msConvertR, "derive_plate_groups", function(...) fake_groups("sample1"))
  stub(msConvertR, "check_docker", function() TRUE)
  stub(msConvertR, "msConvertR_mzml_conversion", function(...) TRUE)

  expect_message(
    msConvertR("input_dir", "output_dir"),
    "Input and output directories are different"
  )
  })
})

test_that("msConvertR_setup_project_directories creates correct structure", {
  suppressMessages({
  output_dir <- file.path(tempdir(), "test_project")
  plateIDs <- c("plate1", "plate2")

  msConvertR_setup_project_directories(output_dir, plateIDs)

  for (plateID in plateIDs) {
    base_path <- file.path(output_dir, plateID)
    expect_true(dir.exists(file.path(base_path, "data", "mzml")))
    expect_true(dir.exists(file.path(base_path, "data", "qs2")))
    expect_true(dir.exists(file.path(base_path, "data", "PeakForgeR")))
    expect_true(dir.exists(file.path(base_path, "data", "raw_data")))
    expect_true(dir.exists(file.path(base_path, "data", "batch_correction")))
    expect_true(dir.exists(file.path(base_path, "html_report")))
  }
  })
})

test_that("msConvertR_construct_command_for_terminal builds correct Docker command", {
  suppressMessages({
  # Use temporary directories for testing
  input_dir <- file.path(tempdir(), "input_test")
  output_dir <- file.path(tempdir(), "output_test")

  # Create dummy directories and a dummy vendor file to simulate structure.
  # Filename must equal plateID after vendor-extension stripping (MS-003
  # anchored exact match, R/msConvertR_Utils.R:210-213). Pre-remediation
  # these tests used `plate1_sample.wiff` and relied on substring matching.
  dir.create(file.path(input_dir, "raw_data"), recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(input_dir, "raw_data", "plate1.wiff"))
  dir.create(file.path(output_dir, "plate1", "data", "mzml"), recursive = TRUE, showWarnings = FALSE)

  # Membership table as produced by derive_plate_groups(): one flat .wiff plate.
  groups <- data.frame(
    raw_path = file.path(input_dir, "raw_data", "plate1.wiff"),
    file_name = "plate1.wiff", rel_dir = "", raw_plateID = "plate1",
    sanitized_plateID = "plate1", is_dir = FALSE, plate_level = TRUE,
    source = "flat", stringsAsFactors = FALSE
  )

  # Run the function (returns a list of per-plate command objects)
  commands <- msConvertR_construct_command_for_terminal(input_dir, output_dir, groups)

  # Check that the result is a list with one element
  expect_type(commands, "list")
  expect_length(commands, 1)

  command <- commands[[1]]
  docker_args <- command$docker_args
  cmd_str <- paste(docker_args, collapse = " ")

  # Check that the command contains expected Docker components
  expect_true(grepl("run", cmd_str))
  expect_true(grepl("proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses", cmd_str))
  expect_true(grepl("wine msconvert", cmd_str))
  expect_true(grepl("-o /output", cmd_str))

  # Check that normalized paths are included. MS-005 emits host paths
  # with forward slashes (gsub("\\\\", "/", path)) so Docker Desktop on
  # Windows parses the -v mount correctly; compare using that form.
  to_fwd <- function(p) gsub("\\\\", "/", p)
  expected_input_mount <- to_fwd(
    normalizePath(file.path(input_dir, "raw_data"), mustWork = FALSE, winslash = "/")
  )
  expect_true(grepl(expected_input_mount, cmd_str, fixed = TRUE))

  expected_output_mount <- to_fwd(
    normalizePath(file.path(output_dir, "plate1", "data", "mzml"),
                  mustWork = FALSE, winslash = "/")
  )
  expect_true(grepl(expected_output_mount, cmd_str, fixed = TRUE))

  })
})

test_that("msConvertR_restructure_directory moves raw files and verifies mzML", {
  suppressMessages({
  # Create a temporary directory structure
  temp_dir <- tempfile("restructure_test_")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create mock raw_data directory
  raw_data_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_data_dir, recursive = TRUE, showWarnings = FALSE)

  # Create mock raw files (a single file and a .d directory)
  raw_file <- file.path(raw_data_dir, "Plate1_sample.raw")
  dir.create(file.path(raw_data_dir, "Plate1_sample.d"), recursive = TRUE, showWarnings = FALSE)
  file.create(raw_file)

  # Docker writes mzML directly to plate/data/mzml/ — simulate that
  mzml_dest <- file.path(temp_dir, "Plate1", "data", "mzml")
  dir.create(mzml_dest, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(mzml_dest, "Plate1_sample.mzML"))

  groups <- data.frame(
    raw_path = c(file.path(raw_data_dir, "Plate1_sample.raw"),
                 file.path(raw_data_dir, "Plate1_sample.d")),
    file_name = c("Plate1_sample.raw", "Plate1_sample.d"),
    rel_dir = "", raw_plateID = "Plate1", sanitized_plateID = "Plate1",
    is_dir = c(FALSE, TRUE), plate_level = FALSE, source = "flat",
    stringsAsFactors = FALSE
  )

  # Run the function
  msConvertR_restructure_directory(temp_dir, groups)

  # Check if raw files were copied
  raw_data_dest <- file.path(temp_dir, "Plate1", "data", "raw_data")
  expect_true(file.exists(file.path(raw_data_dest, "Plate1_sample.raw")))
  expect_true(file.exists(file.path(raw_data_dest, "Plate1_sample.d")))

  # mzML files should still be in place (written by Docker, verified by restructure)
  expect_true(file.exists(file.path(mzml_dest, "Plate1_sample.mzML")))

  })
})

# ============================================================================
# Additional tests for msConvertR_Utils.R coverage
# ============================================================================

# --- validate_input_directory tests ---

test_that("validate_input_directory rejects non-character input", {
  expect_error(
    validate_input_directory(123),
    "must be a single character string"
  )
})

test_that("validate_input_directory rejects vector of length > 1", {
  expect_error(
    validate_input_directory(c("dir1", "dir2")),
    "must be a single character string"
  )
})

test_that("validate_input_directory rejects NULL input", {
  expect_error(
    validate_input_directory(NULL),
    "must be a single character string"
  )
})

test_that("validate_input_directory rejects empty string", {
  expect_error(
    validate_input_directory(""),
    "must not be an empty string"
  )
})

test_that("validate_input_directory rejects nonexistent directory", {
  expect_error(
    validate_input_directory("/nonexistent/path/xyz_99999"),
    "does not exist"
  )
})

test_that("validate_input_directory succeeds for valid directory", {
  temp_dir <- tempfile("valid_input_dir_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_message(
    validate_input_directory(temp_dir),
    "Accessing project directory"
  )
})

# --- msConvertR output_directory validation tests ---

test_that("msConvertR rejects non-character output_directory", {
  suppressMessages({
    stub(msConvertR, "validate_input_directory", function(dir) TRUE)
    expect_error(
      msConvertR("input_dir", 123),
      "output_directory.*must be a single character string"
    )
  })
})

test_that("msConvertR rejects empty string output_directory", {
  suppressMessages({
    stub(msConvertR, "validate_input_directory", function(dir) TRUE)
    expect_error(
      msConvertR("input_dir", ""),
      "output_directory.*must not be an empty string"
    )
  })
})

test_that("msConvertR rejects vector output_directory", {
  suppressMessages({
    stub(msConvertR, "validate_input_directory", function(dir) TRUE)
    expect_error(
      msConvertR("input_dir", c("a", "b")),
      "output_directory.*must be a single character string"
    )
  })
})

# --- validate_file_types tests ---

test_that("validate_file_types errors for directory with no vendor files", {
  temp_dir <- tempfile("no_vendor_")
  dir.create(file.path(temp_dir, "raw_data"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  expect_error(validate_file_types(temp_dir), "No supported vendor files")
})

test_that("validate_file_types validates .wiff files with matching .scan", {
  temp_dir <- tempfile("wiff_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create .wiff and .wiff.scan pair
  file.create(file.path(raw_dir, "sample1.wiff"))
  file.create(file.path(raw_dir, "sample1.wiff.scan"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.wiff$", result)))
})

test_that("validate_file_types flags .wiff without matching .scan as invalid", {
  temp_dir <- tempfile("wiff_noscan_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create .wiff without .scan - also add a valid file so validated_files is non-empty
  # (avoids hitting the buggy invalid_wiff_files reference in the else branch)
  file.create(file.path(raw_dir, "sample_orphan.wiff"))
  file.create(file.path(raw_dir, "sample_valid.raw"))

  expect_message(
    validate_file_types(temp_dir),
    "Missing .wiff.scan"
  )
})

test_that("validate_file_types accepts .raw files", {
  temp_dir <- tempfile("raw_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.raw"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.raw$", result)))
})

test_that("validate_file_types accepts Bruker .baf files", {
  temp_dir <- tempfile("baf_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.baf"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.baf$", result)))
})

test_that("validate_file_types accepts .tsf files", {
  temp_dir <- tempfile("tsf_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.tsf"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.tsf$", result)))
})

test_that("validate_file_types accepts .tdf files", {
  temp_dir <- tempfile("tdf_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.tdf"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.tdf$", result)))
})

test_that("validate_file_types accepts Shimadzu .lcd files", {
  temp_dir <- tempfile("lcd_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.lcd"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.lcd$", result)))
})

test_that("validate_file_types reports unsupported file types alongside valid ones", {
  temp_dir <- tempfile("unsupported_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Include a valid file so we don't hit the only-invalid branch bug
  file.create(file.path(raw_dir, "readme.txt"))
  file.create(file.path(raw_dir, "sample.raw"))

  expect_message(
    validate_file_types(temp_dir),
    "Unsupported file type found"
  )
})

test_that("validate_file_types handles mix of valid and invalid files", {
  temp_dir <- tempfile("mixed_files_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.raw"))
  file.create(file.path(raw_dir, "notes.txt"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.raw$", result)))
  expect_false(any(grepl("notes\\.txt$", result)))
})

test_that("validate_file_types accepts .fid files", {
  temp_dir <- tempfile("fid_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.fid"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.fid$", result)))
})

test_that("validate_file_types accepts .yep files", {
  temp_dir <- tempfile("yep_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.yep"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.yep$", result)))
})

test_that("validate_file_types accepts .mbi files", {
  temp_dir <- tempfile("mbi_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.mbi"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.mbi$", result)))
})

test_that("validate_file_types accepts Waters .uep and .sdf files", {
  temp_dir <- tempfile("waters_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.uep"))
  file.create(file.path(raw_dir, "sample2.sdf"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.uep$", result)))
  expect_true(any(grepl("sample2\\.sdf$", result)))
})

test_that("validate_file_types handles directory with only unsupported files (invalid_wiff_files bug fix)", {
  temp_dir <- tempfile("only_unsupported_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Only unsupported files -- no valid vendor files at all
  file.create(file.path(raw_dir, "notes.txt"))
  file.create(file.path(raw_dir, "readme.pdf"))

  # Before the fix this would crash with:
  #   "object 'invalid_wiff_files' not found"
  # Now it stops with an error after removing unsupported files
  expect_error(validate_file_types(temp_dir), "No supported vendor files")
})

test_that("validate_file_types accepts .qgd and .qgb files", {
  temp_dir <- tempfile("qgd_test_")
  raw_dir <- file.path(temp_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_dir, "sample1.qgd"))
  file.create(file.path(raw_dir, "sample2.qgb"))

  result <- suppressMessages(validate_file_types(temp_dir))
  expect_true(any(grepl("sample1\\.qgd$", result)))
  expect_true(any(grepl("sample2\\.qgb$", result)))
})

# --- msConvertR_setup_project_directories additional tests ---

test_that("msConvertR_setup_project_directories creates user_files directory", {
  temp_dir <- tempfile("proj_user_files_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  msConvertR_setup_project_directories(temp_dir, c("plate1"))

  expect_true(dir.exists(file.path(temp_dir, "user_files")))
})

test_that("msConvertR_setup_project_directories handles single plateID", {
  temp_dir <- tempfile("proj_single_plate_")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  msConvertR_setup_project_directories(temp_dir, c("SINGLE_PLATE"))

  base <- file.path(temp_dir, "SINGLE_PLATE")
  expect_true(dir.exists(file.path(base, "data", "mzml")))
  expect_true(dir.exists(file.path(base, "data", "qs2")))
  expect_true(dir.exists(file.path(base, "data", "PeakForgeR")))
  expect_true(dir.exists(file.path(base, "data", "raw_data")))
  expect_true(dir.exists(file.path(base, "data", "batch_correction")))
  expect_true(dir.exists(file.path(base, "html_report")))
})

test_that("msConvertR_setup_project_directories is idempotent", {
  temp_dir <- tempfile("proj_idempotent_")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  msConvertR_setup_project_directories(temp_dir, c("plate1"))
  # Running again should not error
  expect_no_error(
    msConvertR_setup_project_directories(temp_dir, c("plate1"))
  )
  expect_true(dir.exists(file.path(temp_dir, "plate1", "data", "mzml")))
})

# --- msConvertR_set_working_directory tests ---

test_that("msConvertR_set_working_directory changes working directory", {
  temp_dir <- tempfile("wd_test_")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  setwd_calls <- character(0)
  stub(msConvertR_set_working_directory, "setwd", function(dir) {
    setwd_calls <<- c(setwd_calls, dir)
  })

  msConvertR_set_working_directory(temp_dir)
  # The first setwd call should be to the requested directory
  expect_true(temp_dir %in% setwd_calls)
})

# --- msConvertR_construct_command_for_terminal additional tests ---

test_that("msConvertR_construct_command_for_terminal handles multiple plateIDs", {
  input_dir <- tempfile("cmd_multi_input_")
  output_dir <- tempfile("cmd_multi_output_")
  dir.create(file.path(input_dir, "raw_data"), recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(input_dir, recursive = TRUE)
    unlink(output_dir, recursive = TRUE)
  }, add = TRUE)

  # Two flat .wiff plates.
  file.create(file.path(input_dir, "raw_data", "plateA.wiff"))
  file.create(file.path(input_dir, "raw_data", "plateB.wiff"))
  dir.create(file.path(output_dir, "plateA", "data", "mzml"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "plateB", "data", "mzml"), recursive = TRUE, showWarnings = FALSE)

  groups <- data.frame(
    raw_path = file.path(input_dir, "raw_data", c("plateA.wiff", "plateB.wiff")),
    file_name = c("plateA.wiff", "plateB.wiff"), rel_dir = c("", ""),
    raw_plateID = c("plateA", "plateB"),
    sanitized_plateID = c("plateA", "plateB"),
    is_dir = FALSE, plate_level = TRUE, source = "flat",
    stringsAsFactors = FALSE
  )
  commands <- msConvertR_construct_command_for_terminal(input_dir, output_dir, groups)

  expect_type(commands, "list")
  expect_length(commands, 2)
  cmd_a <- paste(commands[[1]]$docker_args, collapse = " ")
  cmd_b <- paste(commands[[2]]$docker_args, collapse = " ")
  expect_true(grepl("plateA", cmd_a))
  expect_true(grepl("plateB", cmd_b))
})

test_that("msConvertR_construct_command_for_terminal groups subfolder members into one plate", {
  suppressMessages({
  input_dir <- tempfile("cmd_subfolder_input_")
  output_dir <- tempfile("cmd_subfolder_output_")
  dir.create(file.path(input_dir, "raw_data", "PlateX"), recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(input_dir, recursive = TRUE)
    unlink(output_dir, recursive = TRUE)
  }, add = TRUE)

  file.create(file.path(input_dir, "raw_data", "PlateX", "s1.raw"))
  file.create(file.path(input_dir, "raw_data", "PlateX", "s2.raw"))
  dir.create(file.path(output_dir, "PlateX", "data", "mzml"), recursive = TRUE, showWarnings = FALSE)

  groups <- data.frame(
    raw_path = file.path(input_dir, "raw_data", "PlateX", c("s1.raw", "s2.raw")),
    file_name = c("s1.raw", "s2.raw"), rel_dir = c("PlateX", "PlateX"),
    raw_plateID = c("PlateX", "PlateX"),
    sanitized_plateID = c("PlateX", "PlateX"),
    is_dir = FALSE, plate_level = FALSE, source = "subfolder",
    stringsAsFactors = FALSE
  )
  commands <- msConvertR_construct_command_for_terminal(input_dir, output_dir, groups)

  # One plate -> one command carrying two member invocations.
  expect_length(commands, 1)
  expect_length(commands[[1]]$invocations, 2)
  member_files <- vapply(commands[[1]]$invocations, function(x) x$file_name, character(1))
  expect_setequal(member_files, c("s1.raw", "s2.raw"))
  })
})

test_that("msConvertR_construct_command_for_terminal excludes .scan files from command", {
  input_dir <- tempfile("cmd_scan_exclude_")
  output_dir <- tempfile("cmd_scan_output_")
  dir.create(file.path(input_dir, "raw_data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "plate1", "data", "mzml"), recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(input_dir, recursive = TRUE)
    unlink(output_dir, recursive = TRUE)
  }, add = TRUE)

  # derive_plate_groups() never lists the .wiff.scan companion as a member, so
  # only the .wiff is converted; the .scan rides along during restructure.
  file.create(file.path(input_dir, "raw_data", "plate1.wiff"))
  file.create(file.path(input_dir, "raw_data", "plate1.wiff.scan"))

  groups <- data.frame(
    raw_path = file.path(input_dir, "raw_data", "plate1.wiff"),
    file_name = "plate1.wiff", rel_dir = "", raw_plateID = "plate1",
    sanitized_plateID = "plate1", is_dir = FALSE, plate_level = TRUE,
    source = "flat", stringsAsFactors = FALSE
  )
  commands <- msConvertR_construct_command_for_terminal(input_dir, output_dir, groups)
  cmd_str <- paste(commands[[1]]$docker_args, collapse = " ")

  # The .scan file should not appear in the command
  expect_false(grepl("\\.scan", cmd_str))
})

# --- msConvertR_restructure_directory additional tests ---

test_that("msConvertR_restructure_directory reports mzML files excluding COND and BLANK", {
  temp_dir <- tempfile("restructure_filter_")
  raw_data <- file.path(temp_dir, "raw_data")
  dir.create(raw_data, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # A raw file so restructure has a member to relocate
  file.create(file.path(raw_data, "PlateX_sample1.raw"))

  # Docker writes mzML directly to plate/data/mzml/ — simulate that
  mzml_dest <- file.path(temp_dir, "PlateX", "data", "mzml")
  dir.create(mzml_dest, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(mzml_dest, "PlateX_sample1.mzML"))
  file.create(file.path(mzml_dest, "PlateX-COND1_wash.mzML"))
  file.create(file.path(mzml_dest, "PlateX-BLANK1_wash.mzML"))

  groups <- data.frame(
    raw_path = file.path(raw_data, "PlateX_sample1.raw"),
    file_name = "PlateX_sample1.raw", rel_dir = "", raw_plateID = "PlateX",
    sanitized_plateID = "PlateX", is_dir = FALSE, plate_level = FALSE,
    source = "flat", stringsAsFactors = FALSE
  )

  # The restructure function now only verifies/reports mzML — COND/BLANK files
  # remain on disk but are excluded from the verification count message
  msgs <- capture.output(type = "message",
    msConvertR_restructure_directory(temp_dir, groups)
  )
  # Only 1 mzML should be counted (COND and BLANK excluded from report)
  expect_true(any(grepl("1 mzML file", msgs)))
})

test_that("msConvertR_restructure_directory handles multiple plates independently", {
  temp_dir <- tempfile("restructure_multi_")
  raw_data <- file.path(temp_dir, "raw_data")
  dir.create(raw_data, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(raw_data, "PlateA_s1.raw"))
  file.create(file.path(raw_data, "PlateB_s1.raw"))

  # Docker writes mzML directly to plate/data/mzml/ — simulate that
  dir.create(file.path(temp_dir, "PlateA", "data", "mzml"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(temp_dir, "PlateB", "data", "mzml"), recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(temp_dir, "PlateA", "data", "mzml", "PlateA_s1.mzML"))
  file.create(file.path(temp_dir, "PlateB", "data", "mzml", "PlateB_s1.mzML"))

  groups <- data.frame(
    raw_path = file.path(raw_data, c("PlateA_s1.raw", "PlateB_s1.raw")),
    file_name = c("PlateA_s1.raw", "PlateB_s1.raw"), rel_dir = "",
    raw_plateID = c("PlateA", "PlateB"),
    sanitized_plateID = c("PlateA", "PlateB"),
    is_dir = FALSE, plate_level = FALSE, source = "flat",
    stringsAsFactors = FALSE
  )

  suppressMessages(
    msConvertR_restructure_directory(temp_dir, groups)
  )

  # Raw files should be copied to plate raw_data dirs
  expect_true(file.exists(file.path(temp_dir, "PlateA", "data", "raw_data", "PlateA_s1.raw")))
  expect_true(file.exists(file.path(temp_dir, "PlateB", "data", "raw_data", "PlateB_s1.raw")))
  # mzML files already in correct location from Docker
  expect_true(file.exists(file.path(temp_dir, "PlateA", "data", "mzml", "PlateA_s1.mzML")))
  expect_true(file.exists(file.path(temp_dir, "PlateB", "data", "mzml", "PlateB_s1.mzML")))
  # Ensure no cross-contamination
  expect_false(file.exists(file.path(temp_dir, "PlateA", "data", "mzml", "PlateB_s1.mzML")))
})

# --- msConvertR_mzml_conversion orchestration test ---

test_that("msConvertR_mzml_conversion calls sub-functions in order", {
  call_log <- character(0)

  stub(msConvertR_mzml_conversion, "msConvertR_set_working_directory",
       function(...) { call_log <<- c(call_log, "set_wd") })
  stub(msConvertR_mzml_conversion, "msConvertR_setup_project_directories",
       function(...) { call_log <<- c(call_log, "setup_dirs") })
  stub(msConvertR_mzml_conversion, "msConvertR_construct_command_for_terminal",
       function(...) {
         call_log <<- c(call_log, "construct_cmd")
         cmds <- list("cmd1")
         attr(cmds, "active_plateIDs") <- "plate1"
         cmds
       })
  stub(msConvertR_mzml_conversion, "msConvertR_execute_command",
       function(...) { call_log <<- c(call_log, "execute_cmd") })
  stub(msConvertR_mzml_conversion, "msConvertR_restructure_directory",
       function(...) { call_log <<- c(call_log, "restructure") })

  msConvertR_mzml_conversion("in", "out", fake_groups("plate1"))

  expect_equal(call_log, c("set_wd", "setup_dirs", "construct_cmd", "execute_cmd", "restructure"))
})

# --- derive_plate_groups plateID extraction test ---

test_that("derive_plate_groups extracts correct plateIDs from flat vendor files", {
  suppressMessages({
    temp_dir <- tempfile("plateid_extract_")
    raw_dir <- file.path(temp_dir, "raw_data")
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    # .wiff requires its .scan companion to validate
    file.create(file.path(raw_dir, "LIPIDS_PLATE_1.wiff"))
    file.create(file.path(raw_dir, "LIPIDS_PLATE_1.wiff.scan"))
    file.create(file.path(raw_dir, "LIPIDS_PLATE_2.raw"))

    groups <- derive_plate_groups(temp_dir)

    expect_setequal(groups$sanitized_plateID,
                    c("LIPIDS_PLATE_1", "LIPIDS_PLATE_2"))
    # Flat files form their own plate
    expect_true(all(groups$source == "flat"))
  })
})

# --- derive_plate_groups subfolder + manifest tests ---

test_that("derive_plate_groups groups subfolder samples under one plate", {
  suppressMessages({
    temp_dir <- tempfile("derive_subfolder_")
    plate_dir <- file.path(temp_dir, "raw_data", "PlateX")
    dir.create(plate_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    file.create(file.path(plate_dir, "s1.raw"))
    file.create(file.path(plate_dir, "s2.raw"))
    file.create(file.path(plate_dir, "s3.raw"))

    groups <- derive_plate_groups(temp_dir)

    expect_equal(unique(groups$sanitized_plateID), "PlateX")
    expect_equal(nrow(groups), 3)
    expect_true(all(groups$source == "subfolder"))
  })
})

test_that("derive_plate_groups treats a top-level .d as its own plate, not a container", {
  suppressMessages({
    temp_dir <- tempfile("derive_dotd_")
    raw_dir <- file.path(temp_dir, "raw_data")
    dir.create(file.path(raw_dir, "Sample1.d"), recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    groups <- derive_plate_groups(temp_dir)

    expect_equal(groups$sanitized_plateID, "Sample1")
    expect_true(groups$is_dir)
    expect_equal(groups$source, "flat")
  })
})

test_that("derive_plate_groups manifest overrides grouping", {
  suppressMessages({
    temp_dir <- tempfile("derive_manifest_")
    raw_dir <- file.path(temp_dir, "raw_data")
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    file.create(file.path(raw_dir, "a.raw"))
    file.create(file.path(raw_dir, "b.raw"))

    man <- data.frame(raw_file = c("a.raw", "b.raw"),
                      plateID = c("Batch1", "Batch1"),
                      stringsAsFactors = FALSE)
    groups <- derive_plate_groups(temp_dir, manifest = man)

    expect_equal(unique(groups$sanitized_plateID), "Batch1")
    expect_true(all(groups$source == "manifest"))
  })
})

test_that("read_plate_manifest rejects unknown and duplicate raw_file entries", {
  expect_error(
    read_plate_manifest(
      data.frame(raw_file = "missing.raw", plateID = "P1"),
      known_files = c("a.raw")
    ),
    "not found in raw_data"
  )
  expect_error(
    read_plate_manifest(
      data.frame(raw_file = c("a.raw", "a.raw"), plateID = c("P1", "P2")),
      known_files = c("a.raw")
    ),
    "duplicate 'raw_file'"
  )
})

test_that("validate_file_types validates files one level deep in plate subfolders", {
  suppressMessages({
    temp_dir <- tempfile("validate_subfolder_")
    raw_dir <- file.path(temp_dir, "raw_data")
    dir.create(file.path(raw_dir, "PlateA"), recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    file.create(file.path(raw_dir, "PlateA", "s1.raw"))
    file.create(file.path(raw_dir, "PlateA", "s2.raw"))

    result <- validate_file_types(temp_dir)
    expect_equal(length(result), 2)
    expect_true(all(grepl("PlateA", result)))
  })
})

test_that("validate_file_types scopes .wiff.scan pairing per subfolder", {
  suppressMessages({
    temp_dir <- tempfile("validate_subfolder_wiff_")
    raw_dir <- file.path(temp_dir, "raw_data")
    dir.create(file.path(raw_dir, "PlateA"), recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    file.create(file.path(raw_dir, "PlateA", "s1.wiff"))  # missing .scan
    file.create(file.path(raw_dir, "PlateA", "s2.wiff"))
    file.create(file.path(raw_dir, "PlateA", "s2.wiff.scan"))

    result <- suppressMessages(validate_file_types(temp_dir))
    expect_true(any(grepl("s2\\.wiff$", result)))
    expect_false(any(grepl("s1\\.wiff$", result)))  # orphan excluded
  })
})

test_that("msConvertR errors when distinct plate IDs collapse to one sanitized name", {
  suppressMessages({
    stub(msConvertR, "validate_input_directory", function(dir) TRUE)
    stub(msConvertR, "check_docker", function() TRUE)
    # "plate*A" and "plate?A" both sanitize to "plate_A".
    stub(msConvertR, "derive_plate_groups", function(...) {
      data.frame(
        raw_path = file.path("raw_data", c("plate*A", "plate?A"), "s.raw"),
        file_name = "s.raw", rel_dir = c("plate*A", "plate?A"),
        raw_plateID = c("plate*A", "plate?A"),
        sanitized_plateID = c("plate_A", "plate_A"),
        is_dir = FALSE, plate_level = FALSE, source = "subfolder",
        stringsAsFactors = FALSE
      )
    })

    expect_error(
      msConvertR("input_dir", "output_dir"),
      "collapse to the same sanitized name"
    )
  })
})

test_that("msConvertR warns when a plate ID collides with a reserved directory name", {
  suppressMessages({
    stub(msConvertR, "validate_input_directory", function(dir) TRUE)
    stub(msConvertR, "check_docker", function() TRUE)
    stub(msConvertR, "msConvertR_mzml_conversion", function(...) TRUE)
    stub(msConvertR, "derive_plate_groups", function(...) {
      fake_groups("archive", source = "subfolder", plate_level = FALSE,
                  rel_dir = "archive", ext = "raw")
    })

    expect_warning(
      msConvertR("input_dir", "output_dir"),
      "reserved directory names"
    )
  })
})

test_that("msConvertR emits thank you message on completion", {
  suppressMessages({
    stub(msConvertR, "validate_input_directory", function(dir) TRUE)
    stub(msConvertR, "derive_plate_groups", function(...) fake_groups("s1"))
    stub(msConvertR, "check_docker", function() TRUE)
    stub(msConvertR, "msConvertR_mzml_conversion", function(...) TRUE)

    expect_message(
      msConvertR("in", "out"),
      "Thank you for using msConvertR"
    )
  })
})


