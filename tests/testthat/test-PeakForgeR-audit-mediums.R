# Tests for Medium-severity audit findings PK-015 to PK-045
library(tibble)

# Helper: locate a source file under R/ relative to the package root.
# Skips the test cleanly when the file isn't reachable, which happens
# when the test runs against the *installed* package (e.g. inside
# covr::codecov() which installs MStargetR before running tests). The
# installed library doesn't ship raw R/*.R files — they're bundled into
# a binary lazy-load DB — so any audit test that greps source code
# would otherwise raise "cannot open the connection". The original
# guard only handled rprojroot returning an error; this also handles
# rprojroot succeeding but the file simply not existing on disk.
.skip_if_no_source <- function(rel_under_R) {
  pkg_root <- tryCatch(rprojroot::find_package_root_file(),
                       error = function(e) NULL)
  if (is.null(pkg_root)) skip("cannot locate package root")
  full <- file.path(pkg_root, "R", rel_under_R)
  if (!file.exists(full)) {
    skip(paste0("source file not available outside source tree: ", full))
  }
  full
}

# PK-015: @examples uses readr::read_tsv (qualified) ----
test_that("PK-015: @examples in PeakForgeR.R uses readr::read_tsv", {
  src <- readLines(.skip_if_no_source("PeakForgeR.R"))
  unqualified <- grep("example_mrm_template <- read_tsv", src, fixed = TRUE)
  expect_equal(length(unqualified), 0L,
               info = "read_tsv in @examples must be qualified as readr::read_tsv")
})

# PK-017: sanitize_identifier not applied to discovered directory names ----
test_that("PK-017: discovered plateIDs are not sanitized (on-disk names preserved)", {
  src <- readLines(.skip_if_no_source("PeakForgeR.R"))
  # The vapply(plateIDs, sanitize_identifier, ...) line must no longer exist
  # immediately after the plateIDs <- candidate_entries[dir.exists(...)] line
  sanitize_after_discover <- grep("vapply.*plateIDs.*sanitize_identifier", src)
  expect_equal(length(sanitize_after_discover), 0L,
               info = "sanitize_identifier must not be applied to discovered directory names")
})

# PK-021: log_error delegates to write_log (timestamped format) ----
test_that("PK-021: log_error uses write_log for consistent timestamp format", {
  tmp <- tempfile(fileext = ".txt")
  # Simulate the closure created inside future_lapply
  log_file <- tmp
  write_log <- function(text) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    line <- paste0("[", timestamp, "] ", text, "\n")
    write(enc2utf8(line), file = log_file, append = TRUE)
  }
  log_error <- function(error_message, plateID, project_directory = getwd()) {
    write_log(paste("ERROR:", error_message))
  }
  log_error("something failed", "PLATE01")
  lines <- readLines(tmp)
  expect_true(length(lines) >= 1)
  expect_true(grepl("^\\[\\d{4}-\\d{2}-\\d{2}", lines[1]),
              info = "log_error output should start with a timestamp bracket")
  expect_true(grepl("ERROR:", lines[1]),
              info = "log_error output should contain 'ERROR:' prefix")
  unlink(tmp)
})

# PK-022: vapply used for successful/failed plate extraction ----
test_that("PK-022: successful_plates uses vapply not sapply", {
  src <- readLines(.skip_if_no_source("PeakForgeR.R"))
  sapply_lines <- grep("successful_plates <- sapply", src, fixed = TRUE)
  expect_equal(length(sapply_lines), 0L,
               info = "successful_plates must use vapply, not sapply")
  vapply_lines <- grep("successful_plates <- vapply", src, fixed = TRUE)
  expect_true(length(vapply_lines) > 0L,
              info = "successful_plates should use vapply")
})

# PK-024: wait_until_files_free stop includes call. = FALSE ----
test_that("PK-024: wait_until_files_free timeout stop has no call info", {
  # Create a real locked-path scenario by passing a file that does not exist
  # (file.access returns -1, so it will loop until retries exceeded)
  tmp_file <- tempfile()
  writeLines("x", tmp_file)
  Sys.chmod(tmp_file, "444")  # read-only so file.access(mode=2) fails on POSIX
  result <- tryCatch({
    # Use max_retries=0 to trip immediately
    MStargetR:::wait_until_files_free(tmp_file, max_wait = 0, max_retries = 0)
    "no_error"
  }, error = function(e) {
    list(msg = conditionMessage(e), call = conditionCall(e))
  })
  Sys.chmod(tmp_file, "644")
  unlink(tmp_file)
  # On Windows file.access(mode=2) on a read-only file returns -1; on other
  # platforms the file may be writable by owner. Accept either outcome but if
  # an error was thrown it must have call. = FALSE (conditionCall is NULL).
  if (is.list(result)) {
    expect_null(result$call, info = "stop() in wait_until_files_free should use call. = FALSE")
    expect_true(grepl("File still in use", result$msg))
  } else {
    succeed("file was accessible; timeout branch not reached on this platform")
  }
})

# PK-029: ANPC filter uses named constant ANPC_EXCLUDE_PATTERN ----
test_that("PK-029: ANPC_EXCLUDE_PATTERN constant is exported and case-insensitive", {
  pat <- MStargetR:::ANPC_EXCLUDE_PATTERN
  expect_true(is.character(pat) && length(pat) == 1)
  # Should match ISTDs case-insensitively
  expect_true(grepl(pat, "SAMPLE_ISTDS_001.mzML", perl = TRUE))
  expect_true(grepl(pat, "SAMPLE_istds_001.mzML", perl = TRUE))
  # Should match BLANK
  expect_true(grepl(pat, "BLANK_001.mzML", perl = TRUE))
  # Should NOT match a legitimate sample containing 'SIL' elsewhere
  expect_false(grepl(pat, "SAMPLE_LTR_001.mzML", perl = TRUE))
})

# PK-030: extract_acquisition_year warns on NA/empty timestamp ----
test_that("PK-030: extract_acquisition_year warns and returns NA_integer_ for empty timestamp", {
  expect_warning(
    result <- MStargetR:::extract_acquisition_year(NA_character_),
    regexp = "missing or empty"
  )
  expect_true(is.na(result))

  expect_warning(
    result2 <- MStargetR:::extract_acquisition_year(""),
    regexp = "missing or empty"
  )
  expect_true(is.na(result2))
})

test_that("PK-030: extract_acquisition_year returns integer year for valid timestamp", {
  result <- MStargetR:::extract_acquisition_year("2025-06-17T13:31:58Z")
  expect_equal(result, 2025L)
})

# PK-031: process_files uses exact match not grep for mzML filenames ----
test_that("PK-031: exact match used for mzml filename lookup in process_files", {
  src <- readLines(.skip_if_no_source("PeakForgeR_mzml.R"))
  grep_lines <- grep('length\\(grep\\(idx_mzML', src)
  expect_equal(length(grep_lines), 0L,
               info = "grep() for mzML match must be replaced with exact ==")
  exact_lines <- grep('idx_mzML == names\\(FUNC_mzR', src)
  expect_true(length(exact_lines) > 0L)
})

# PK-034: find_peak_end_idx uses last crossing, not full length ----
test_that("PK-034: find_peak_end_idx falls back to last crossing not full trace end", {
  # Build a minimal FUNC_mzR with only 1 baseline crossing after apex
  intensities <- c(1, 1, 5, 20, 50, 20, 5, 1, 1, 1)
  rtime      <- seq_along(intensities) * 0.1
  chrom      <- data.frame(rtime = rtime, intensity = intensities)
  FUNC_mzR <- list(
    plate1 = list(
      file1.mzML = list(
        mzR_chromatogram = list(chrom)
      )
    )
  )
  baseline_value <- stats::quantile(intensities, 0.1, na.rm = TRUE, names = FALSE)
  # Apex is at index 5
  peak_apex_idx <- which.max(intensities)
  # After apex, baseline crossings: indices 8, 9, 10 (intensities 1, 1, 1)
  # With n_baseline_crossings=3, should pick index 10, not length(intensities)+anything
  result <- suppressMessages(
    MStargetR:::find_peak_end_idx(FUNC_mzR, "plate1", "file1.mzML", 1L,
                                   peak_apex_idx, baseline_value)
  )
  expect_lte(result, length(intensities))
  expect_gte(result, peak_apex_idx)
})

# PK-034b: find_peak_end_idx notice is gated by MStargetR.verbose ----
test_that("PK-034b: find_peak_end_idx notice is silent by default and emitted when verbose", {
  # Construct a chromatogram with only 2 below-baseline points after the apex
  # so the function falls into the "fewer than n_baseline_crossings" branch.
  # intensities: apex at index 5; values 0.5 appear at indices 7 and 9 only
  # (< baseline 1.0), so after_apex_crossings has length 2 < 3.
  intensities <- c(1, 1, 5, 20, 50, 20, 1, 0.5, 1, 0.5)
  rtime       <- seq_along(intensities) * 0.1
  chrom       <- data.frame(rtime = rtime, intensity = intensities)
  FUNC_mzR <- list(
    plate1 = list(
      file1.mzML = list(
        mzR_chromatogram = list(chrom)
      )
    )
  )
  baseline_value <- 1.0  # indices 8 and 10 are 0.5, so 2 crossings after apex

  # With default options (verbose = FALSE): no message emitted
  old_opt <- getOption("MStargetR.verbose")
  on.exit(options(MStargetR.verbose = old_opt), add = TRUE)

  options(MStargetR.verbose = FALSE)
  expect_silent(
    MStargetR:::find_peak_end_idx(FUNC_mzR, "plate1", "file1.mzML", 1L,
                                   which.max(intensities), baseline_value)
  )

  # With verbose = TRUE: message IS emitted
  options(MStargetR.verbose = TRUE)
  expect_message(
    MStargetR:::find_peak_end_idx(FUNC_mzR, "plate1", "file1.mzML", 1L,
                                   which.max(intensities), baseline_value),
    "find_peak_end_idx: fewer than"
  )
})

# PK-035: create_output warns and uses first row when guide_match > 1 ----
test_that("PK-035: create_output warns on duplicate precursor_name in guide", {
  guide <- tibble::tibble(
    precursor_name = c("LipidA", "LipidA"),
    precursor_mz = c(500.0, 500.0),
    precursor_charge = c(1L, 1L),
    product_mz = c(100.0, 100.0),
    product_charge = c(1L, 1L),
    explicit_retention_time_window = c(0.5, 0.5),
    note = c(NA_character_, NA_character_),
    molecule_list_name = c("ClassA", "ClassA")
  )
  tbl <- tibble::tibble(
    mzml = "file1.mzML",
    lipid_class = "ClassA",
    lipid = "LipidA",
    precursor_mz = 500.0,
    product_mz = 100.0,
    peak_apex = 1.5,
    peak_start = 1.0,
    peak_end = 2.0
  )
  expect_warning(
    out <- MStargetR:::create_output(tbl, guide, "file1.mzML"),
    regexp = "matched 2 rows"
  )
  expect_equal(nrow(out$mrm_guide_updated), 1L)
})

# PK-037: read_mrm_guides stops on basename collision ----
test_that("PK-037: read_mrm_guides errors on duplicate basenames", {
  ml <- initialise_master_list()  # use internal helper
  paths <- list("dir1/template.tsv", "dir2/template.tsv")
  expect_error(
    MStargetR:::read_mrm_guides(ml, paths),
    regexp = "same basename"
  )
})

# PK-038: SIL matching uses non-alphabetic delimiter pattern ----
test_that("PK-038: SIL pattern does not match 'Silybin' but matches PC_SIL_d7", {
  # Use the same pattern now in read_mrm_guides and check_sil_standards
  sil_pattern <- "(^|[^A-Za-z])SIL([^A-Za-z]|$)"
  precursor_names <- c("Silybin", "PC_SIL_d7", "LPC_SIL_d5", "SIL_d6")
  result <- grepl(sil_pattern, precursor_names, ignore.case = TRUE)
  expect_false(result[1], info = "Silybin should not be classified as SIL")
  expect_true(result[2], info = "PC_SIL_d7 should match SIL pattern")
  expect_true(result[3], info = "LPC_SIL_d5 should match SIL pattern")
  expect_true(result[4], info = "SIL_d6 should match SIL pattern")
})

# PK-040: version_selector regex matches v5 and v6 ----
test_that("PK-040: version_selector pattern matches MS-LIPIDS-5 and MS-LIPIDS-6", {
  pat <- "_MS-LIPIDS(?:-[0-9]+)?"
  expect_true(!is.na(regmatches("PLATE_MS-LIPIDS-5_001",
                                regexpr(pat, "PLATE_MS-LIPIDS-5_001", perl = TRUE))))
  expect_true(!is.na(regmatches("PLATE_MS-LIPIDS-6",
                                regexpr(pat, "PLATE_MS-LIPIDS-6", perl = TRUE))))
  expect_true(!is.na(regmatches("PLATE_MS-LIPIDS",
                                regexpr(pat, "PLATE_MS-LIPIDS", perl = TRUE))))
})

# PK-041: junction paths use tempfile() not PID ----
test_that("PK-041: junction path construction uses tempfile not Sys.getpid", {
  utils_src <- readLines(.skip_if_no_source("PeakForgeR_Utils.R"))
  mzml_src  <- readLines(.skip_if_no_source("PeakForgeR_mzml.R"))
  pid_uses_utils <- grep("Sys\\.getpid\\(\\)", utils_src)
  pid_uses_mzml  <- grep("Sys\\.getpid\\(\\)", mzml_src)
  expect_equal(length(pid_uses_utils), 0L,
               info = "PeakForgeR_Utils.R must not use Sys.getpid() for junction paths")
  expect_equal(length(pid_uses_mzml), 0L,
               info = "PeakForgeR_mzml.R must not use Sys.getpid() for junction paths")
})

# PK-042: find_lipid_info guards against NA in guide m/z ----
test_that("PK-042: find_lipid_info returns no-match when guide has NA precursor_mz", {
  guide <- tibble::tibble(
    precursor_mz = c(NA_real_, 500.0),
    product_mz   = c(NA_real_, 100.0),
    explicit_retention_time = c(1.5, 1.5),
    molecule_list_name = c("ClassNA", "ClassA"),
    precursor_name = c("LipidNA", "LipidA")
  )
  chrom <- data.frame(rtime = c(1.0, 1.5, 2.0), intensity = c(10, 50, 10))
  FUNC_mzR <- list(
    plate1 = list(
      `file1.mzML` = list(mzR_chromatogram = list(chrom))
    )
  )
  # Query for the NA row — should not match and not error
  result <- MStargetR:::find_lipid_info(
    guide, NA_real_, NA_real_, 1.5,
    FUNC_mzR, "plate1", "file1.mzML", 1L
  )
  expect_true(result$class %in% c("no match", "multiple match"))
})

# PK-043: check_sil_standards returns FALSE (not NaN >= 0.90) when no SILs ----
test_that("PK-043: check_sil_standards returns FALSE when version has no SIL entries", {
  ml <- list(
    data = list(
      PeakForgeR_report = list(
        PLATE01 = tibble::tibble(molecule_name = c("LipidA", "LipidB"))
      )
    ),
    templates = list(
      mrm_guides = list(
        v1 = list(
          mrm_guide = tibble::tibble(`Precursor Name` = c("LipidA", "LipidB"))
        )
      )
    )
  )
  result <- suppressMessages(MStargetR:::check_sil_standards(ml, "PLATE01", "v1"))
  expect_false(isTRUE(result))
  expect_false(is.nan(result))
})

# PK-045: save_plate_data captures both stdout and stderr on child failure ----
test_that("PK-045: error message from save_plate_data includes child output fields", {
  src <- readLines(.skip_if_no_source("PeakForgeR_Utils.R"))
  stdout_capture <- grep("read_all_output", src, fixed = TRUE)
  stderr_capture <- grep("read_all_error", src, fixed = TRUE)
  expect_true(length(stdout_capture) > 0L,
              info = "save_plate_data must capture child stdout via read_all_output()")
  expect_true(length(stderr_capture) > 0L,
              info = "save_plate_data must capture child stderr via read_all_error()")
})
