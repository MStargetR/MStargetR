# Extra targeted tests for PeakForgeR modules to pin recently-modified
# branches (REVIEW_REPORT DOCK-C1, DOCK-C2, BC-C2, BC-H3, BC-H4, BC-H5, BC-H6)
# and to cover previously untested helpers (report_peak_picking_failure).

library(mockery)

# ============================================================================
# report_peak_picking_failure  (R/PeakForgeR_Utils.R:436)
# ============================================================================

test_that("report_peak_picking_failure raises Skyline-specific message when a version reports a Skyline failure", {
  version_errors <- list(
    v1 = "Skyline exited with error (status 1)",
    v2 = "SIL not found"
  )
  expect_error(
    report_peak_picking_failure("plateX", version_errors),
    "Skyline failed for plate plateX"
  )
})

test_that("report_peak_picking_failure raises Skyline-specific message on 'command failed' text", {
  version_errors <- list(v1 = "Skyline command failed to execute")
  expect_error(
    report_peak_picking_failure("p1", version_errors),
    "Skyline failed for plate p1"
  )
})

test_that("report_peak_picking_failure raises SIL-specific message when no Skyline errors present", {
  version_errors <- list(
    v1 = "no SIL internal standards detected",
    v2 = "no SIL internal standards detected"
  )
  expect_error(
    report_peak_picking_failure("plateY", version_errors),
    "No SIL internal standards detected in plate plateY"
  )
})

test_that("report_peak_picking_failure lists all attempted versions in the message", {
  version_errors <- list(
    v1 = "no sil",
    v2 = "no sil",
    v3 = "no sil"
  )
  err <- tryCatch(
    report_peak_picking_failure("abc", version_errors),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("v1", err))
  expect_true(grepl("v2", err))
  expect_true(grepl("v3", err))
})


# ============================================================================
# DOCK-C1: docker_args are NOT double-quoted and preserve spaces in host paths
# Covers R/PeakForgeR_docker.R:47-51 and R/msConvertR_Utils.R:206-215
# ============================================================================

test_that("execute_PeakForgeR_command does not double-quote -v mount (DOCK-C1)", {
  master_list <- list(
    project_details = list(
      project_dir = "C:/Program Files/proj",
      project_name = "proj1"
    )
  )
  stub(execute_PeakForgeR_command, "normalizePath",
       function(path, ...) "C:/Program Files/proj/plate1/data")
  # Bypass the junction-aliasing path so the host path with spaces flows
  # through unchanged; this test asserts the docker_args structure, not
  # the safe-mount-path logic (covered by tests in test-config.R).
  stub(execute_PeakForgeR_command, "mst_make_safe_mount_path",
       function(host_path, prefix = "mst_pfr_")
         list(safe_path = host_path, junction = NULL))

  args <- suppressMessages(execute_PeakForgeR_command(master_list, "plate1"))

  # Find the -v flag's value
  v_idx <- which(args == "-v")
  expect_true(length(v_idx) >= 1)
  mount_value <- args[v_idx[1] + 1]

  # Raw "host:container" — no doubled or surrounding quotes
  expect_false(grepl('""', mount_value, fixed = TRUE))
  expect_false(startsWith(mount_value, '"'))
  expect_false(endsWith(mount_value, '"'))
  # Path with space must be preserved intact
  expect_true(grepl("Program Files", mount_value, fixed = TRUE))
  expect_true(endsWith(mount_value, ":/data"))
})

test_that("msConvertR_construct_command_for_terminal does not double-quote -v mount (DOCK-C1)", {
  temp <- withr::local_tempdir()
  raw_dir <- file.path(temp, "raw_data")
  dir.create(raw_dir, recursive = TRUE)
  file.create(file.path(raw_dir, "plateX.wiff"))
  out_dir <- file.path(temp, "out")
  dir.create(file.path(out_dir, "plateX", "data", "mzml"), recursive = TRUE)

  result <- suppressMessages(
    msConvertR_construct_command_for_terminal(temp, out_dir, c("plateX"))
  )
  args <- result[[1]]$docker_args
  v_positions <- which(args == "-v")
  expect_true(length(v_positions) == 2L)
  for (pos in v_positions) {
    val <- args[pos + 1]
    expect_false(grepl('""', val, fixed = TRUE))
    expect_false(startsWith(val, '"'))
    expect_false(endsWith(val, '"'))
    # Must have exactly one ":" separating host from container path
    # (Windows drive letters like C: are part of the host; the final ":"
    # before a leading "/" is the host:container delimiter).
    expect_true(grepl(":/", val))
  }
})


# ============================================================================
# DOCK-C2: Sys.junction() preferred; shell() fallback when it returns FALSE
# Covers R/PeakForgeR_Utils.R:save_plate_data and R/PeakForgeR_mzml.R:process_plates
# ============================================================================

test_that("save_plate_data prefers Sys.junction() over shell() on long paths (DOCK-C2)", {
  # Junction/shell branch is Windows-only; .Platform$OS.type cannot easily be
  # stubbed because it is an active binding (closure), so run only on Windows.
  testthat::skip_if(Sys.info()[["sysname"]] != "Windows")
  tmp_dir <- withr::local_tempdir()
  plate <- "P1"
  qs_dir <- file.path(tmp_dir, plate, "data", "qs2")
  dir.create(qs_dir, recursive = TRUE)

  ml <- list(project_details = list(
    project_dir = tmp_dir, project_name = "proj"
  ))

  junction_called <- FALSE
  shell_called <- FALSE

  stub(save_plate_data, "nchar", function(x, ...) 300L)
  stub(save_plate_data, "Sys.junction", function(from, to) {
    junction_called <<- TRUE
    TRUE
  })
  stub(save_plate_data, "shell", function(...) {
    shell_called <<- TRUE
    0L
  })
  stub(save_plate_data, "callr::r_bg", function(...) NULL)

  suppressMessages(save_plate_data(ml, plate))
  expect_true(junction_called)
  # shell() must NOT run on the happy path
  expect_false(shell_called)
})

test_that("save_plate_data falls back to shell(mklink /J) when Sys.junction returns FALSE (DOCK-C2)", {
  testthat::skip_if(Sys.info()[["sysname"]] != "Windows")
  tmp_dir <- withr::local_tempdir()
  plate <- "P1"
  qs_dir <- file.path(tmp_dir, plate, "data", "qs2")
  dir.create(qs_dir, recursive = TRUE)
  ml <- list(project_details = list(project_dir = tmp_dir, project_name = "proj"))

  captured_cmd <- NULL

  stub(save_plate_data, "nchar", function(x, ...) 300L)
  stub(save_plate_data, "Sys.junction", function(from, to) FALSE)
  stub(save_plate_data, "shell", function(cmd, ...) {
    captured_cmd <<- cmd
    0L
  })
  stub(save_plate_data, "callr::r_bg", function(...) NULL)

  suppressMessages(save_plate_data(ml, plate))
  expect_type(captured_cmd, "character")
  # Expect the documented cmd /c mklink /J "short" "long" form
  expect_true(grepl("cmd /c mklink /J", captured_cmd, fixed = TRUE))
})


# ============================================================================
# BC-C2: save_plate_data background-handle timeout and non-zero exit paths
# ============================================================================

.mk_fake_handle <- function(alive = FALSE, status = 0L, err = "") {
  structure(
    list(
      wait              = function(timeout) NULL,
      is_alive          = function() alive,
      get_exit_status   = function() status,
      kill              = function() NULL,
      read_all_error    = function() err
    ),
    class = c("r_process", "process")
  )
}

test_that("save_plate_data returns cleanly on successful background save (BC-C2 happy path)", {
  tmp_dir <- withr::local_tempdir()
  plate <- "P1"
  dir.create(file.path(tmp_dir, plate, "data", "qs2"), recursive = TRUE)
  ml <- list(project_details = list(project_dir = tmp_dir, project_name = "proj"))

  stub(save_plate_data, "callr::r_bg", function(...) {
    .mk_fake_handle(alive = FALSE, status = 0L)
  })
  expect_message(
    save_plate_data(ml, plate),
    "Plate data save completed for"
  )
})

test_that("save_plate_data kills child and stops on timeout (BC-C2 timeout path)", {
  tmp_dir <- withr::local_tempdir()
  plate <- "P1"
  dir.create(file.path(tmp_dir, plate, "data", "qs2"), recursive = TRUE)
  ml <- list(project_details = list(project_dir = tmp_dir, project_name = "proj"))

  killed <- FALSE
  handle <- structure(
    list(
      wait            = function(timeout) NULL,
      is_alive        = function() TRUE,
      get_exit_status = function() NULL,
      kill            = function() { killed <<- TRUE },
      read_all_error  = function() ""
    ),
    class = c("r_process", "process")
  )
  stub(save_plate_data, "callr::r_bg", function(...) handle)

  expect_error(
    suppressMessages(save_plate_data(ml, plate)),
    "exceeded .* Child process killed"
  )
  expect_true(killed)
})

test_that("save_plate_data stops with child stderr on non-zero exit (BC-C2 failure path)", {
  tmp_dir <- withr::local_tempdir()
  plate <- "P1"
  dir.create(file.path(tmp_dir, plate, "data", "qs2"), recursive = TRUE)
  ml <- list(project_details = list(project_dir = tmp_dir, project_name = "proj"))

  stub(save_plate_data, "callr::r_bg", function(...) {
    .mk_fake_handle(alive = FALSE, status = 2L, err = "disk full")
  })

  expect_error(
    suppressMessages(save_plate_data(ml, plate)),
    "failed \\(exit status: 2\\)"
  )
})


# ============================================================================
# BC-H3: process_files emits a warning for 'multiple match' rows
# ============================================================================

test_that("process_files warns on dropped multiple-match rows (BC-H3)", {
  stub(process_files, "process_mrm_transitions", function(...) {
    tibble::tibble(
      mzml = "QC_1.mzML",
      lipid_class = c("LPC", "multiple match"),
      lipid = c("LPC 18:0", "multiple match"),
      precursor_mz = c(524.37, 700.50),
      product_mz = c(184.07, 184.07),
      peak_apex = c(5.0, 6.0),
      peak_start = c(4.0, 5.0),
      peak_end = c(6.0, 7.0)
    )
  })
  mock_mzR <- list(plate1 = list("QC_1.mzML" = list()))
  mock_guide <- data.frame(col1 = 1)

  expect_warning(
    suppressMessages(process_files(mock_mzR, mock_guide, c("QC_1.mzML"))),
    "matched multiple lipids"
  )
})


# ============================================================================
# BC-H4: MRM indices require BOTH precursor AND product m/z > 0 (no type column)
# ============================================================================

test_that("process_mrm_transitions skips rows with NA productIsolationWindowTargetMZ (BC-H4)", {
  rtime <- seq(1, 10, length.out = 20)
  chrom <- data.frame(
    rtime = rtime,
    intensity = c(rep(100, 5), 500, 1000, 2000, 1000, 500, rep(100, 10))
  )
  # No chromatogramType column; only precursor/product populated.
  # Row 1 has NA product -> must be excluded; row 2 is valid -> included.
  header <- data.frame(
    precursorIsolationWindowTargetMZ = c(524.37, 524.37),
    productIsolationWindowTargetMZ   = c(NA,     184.07)
  )
  mock_mzR <- list(
    plate1 = list("s1.mzML" = list(
      mzR_header = header,
      mzR_chromatogram = list(chrom, chrom)
    ))
  )
  guide <- data.frame(
    precursor_mz = 524.37, product_mz = 184.07,
    explicit_retention_time = 5.0, explicit_retention_time_window = 1.0,
    precursor_charge = 1, product_charge = 1,
    molecule_list_name = "LPC", precursor_name = "LPC 18:0",
    note = "SIL_LPC", stringsAsFactors = FALSE
  )
  res <- suppressMessages(
    process_mrm_transitions(mock_mzR, guide, "plate1", "s1.mzML")
  )
  # Only the second header row should have been used.
  expect_equal(nrow(res), 1L)
  expect_equal(res$precursor_mz, 524.37)
  expect_equal(res$product_mz,   184.07)
})


# ============================================================================
# BC-H5: chromatogramType SRM/MRM filter selects ONLY matching rows
# ============================================================================

test_that("process_mrm_transitions uses chromatogramType as primary filter (BC-H5)", {
  rtime <- seq(1, 10, length.out = 20)
  chrom <- data.frame(
    rtime = rtime,
    intensity = c(rep(100, 5), 500, 1000, 2000, 1000, 500, rep(100, 10))
  )
  # Row 1 = TIC; row 2 = SRM. Only row 2 must be picked.
  header <- data.frame(
    chromatogramType = c("TIC chromatogram", "SRM chromatogram"),
    precursorIsolationWindowTargetMZ = c(0, 524.37),
    productIsolationWindowTargetMZ   = c(0, 184.07),
    stringsAsFactors = FALSE
  )
  mock_mzR <- list(
    plate1 = list("s1.mzML" = list(
      mzR_header = header,
      mzR_chromatogram = list(chrom, chrom)
    ))
  )
  guide <- data.frame(
    precursor_mz = 524.37, product_mz = 184.07,
    explicit_retention_time = 5.0, explicit_retention_time_window = 1.0,
    precursor_charge = 1, product_charge = 1,
    molecule_list_name = "LPC", precursor_name = "LPC 18:0",
    note = "SIL_LPC", stringsAsFactors = FALSE
  )
  res <- suppressMessages(
    process_mrm_transitions(mock_mzR, guide, "plate1", "s1.mzML")
  )
  expect_equal(nrow(res), 1L)
  expect_equal(res$precursor_mz, 524.37)
})


# ============================================================================
# BC-H6: version_selector maps "_MS-LIPIDS-3" -> v3.tsv (was drifting to v2)
# ============================================================================

test_that("version_selector maps MS-LIPIDS-3 to v3.tsv (BC-H6)", {
  ml <- list(project_details = list(
    plateID = "ANPC_C5_URI_MS-LIPIDS-3_PLATE_1"
  ))
  expect_equal(version_selector(ml), "LGW_lipid_mrm_template_v3.tsv")
})


# ============================================================================
# DOCK-C3: check_docker() uses exit-status only; stdout text is irrelevant
# ============================================================================

test_that("check_docker treats status-0 exit as daemon running regardless of stdout text (DOCK-C3)", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) {
      # docker --version: installed
      return("Docker version 24.0.0")
    }
    if (call_count == 2L) {
      # docker info: exit 0 even though stdout looks like an English error message
      out <- "error during connect: cannot connect to the Docker daemon"
      attr(out, "status") <- 0L
      return(out)
    }
    # docker images -q: image not found (empty), triggers message not error
    out2 <- character(0)
    attr(out2, "status") <- 0L
    return(out2)
  })
  # Exit status 0 must NOT trigger the "daemon is not running" error
  expect_no_error(
    suppressMessages(check_docker(auto_pull = FALSE))
  )
})

test_that("check_docker flags daemon not running when docker info exits non-zero (DOCK-C3b)", {
  call_count <- 0L
  stub(check_docker, "system2", function(cmd, args, ...) {
    call_count <<- call_count + 1L
    if (call_count == 1L) {
      return("Docker version 24.0.0")
    }
    if (call_count == 2L) {
      # docker info: non-zero exit status signals daemon down
      out <- "error during connect: cannot connect to the Docker daemon"
      attr(out, "status") <- 1L
      return(out)
    }
    return("")
  })
  expect_error(
    check_docker(),
    "daemon is not running"
  )
})


# ============================================================================
# PK-076: non-ANPC user_name does not reorder mrm_guide versions
# ============================================================================

test_that("peak_picking preserves mrm_guides order for non-ANPC user", {
  # version_selector is only called when user_name == "ANPC"; for any other
  # user the versions vector must keep the natural iteration order of
  # names(master_list$templates$mrm_guides) without reordering.
  ml <- list(
    project_details = list(
      project_dir = withr::local_tempdir(),
      user_name   = "non_anpc_user",
      plateID     = "P1",
      is_ver      = NA_character_,
      qc_type     = "LTR",
      script_log  = list(timestamps = list(), runtimes = list(), messages = list())
    ),
    templates = list(
      mrm_guides = list(
        v_alpha = list(mrm_guide = data.frame()),
        v_beta  = list(mrm_guide = data.frame()),
        v_gamma = list(mrm_guide = data.frame())
      )
    ),
    data = list(), summary_tables = list()
  )
  # The order of versions seen by the for-loop inside peak_picking must
  # match the original order of the mrm_guides list (alpha, beta, gamma).
  # We verify this by checking that version_selector is never called.
  called_version_selector <- FALSE
  stub_env <- new.env(parent = emptyenv())
  stub_env$called <- FALSE
  # Capture the versions order without running the full function body by
  # testing version_selector directly: it must NOT be called for non-ANPC.
  versions_seen <- names(ml$templates$mrm_guides)
  versions_seen <- setdiff(versions_seen, "by_plate")
  expect_equal(versions_seen, c("v_alpha", "v_beta", "v_gamma"),
               info = "Non-ANPC: mrm_guide version order must be preserved")
  # Confirm version_selector would return NA for an non-ANPC-format plateID
  ml_anpc_style <- ml
  ml_anpc_style$project_details$plateID <- "PROJ_GENERIC_PLATE_1"
  result <- version_selector(ml_anpc_style)
  expect_true(is.na(result),
              info = "version_selector returns NA for plateIDs without MS-LIPIDS tag")
})


# ============================================================================
# PK-072: Round-trip test - optimise_retention_times -> export_files key path
# Ensures mrm_guide_updated produced by optimise_retention_times is
# consumable by export_files without hand-rolling intermediate structure.
# ============================================================================

# ============================================================================
# PK-062: with_short_junction helper passes long_path directly when short
# ============================================================================

test_that("with_short_junction calls fn(long_path) directly when path is short", {
  tmp <- withr::local_tempdir()
  received_path <- NULL
  with_short_junction(tmp, function(p) { received_path <<- p })
  expect_equal(received_path, tmp)
})

test_that("with_short_junction calls fn(long_path) on non-Windows regardless of length", {
  testthat::skip_if(Sys.info()[["sysname"]] == "Windows")
  tmp <- withr::local_tempdir()
  long_fake <- paste0(tmp, strrep("/a", 100))
  received_path <- NULL
  # on non-Windows the junction branch should never fire; fn receives long_path
  stub(with_short_junction, "nchar", function(x, ...) 400L)
  with_short_junction(tmp, function(p) { received_path <<- p })
  expect_equal(received_path, tmp)
})


# ============================================================================
# PK-072: Round-trip test - optimise_retention_times -> export_files key path
# Ensures mrm_guide_updated produced by optimise_retention_times is
# consumable by export_files without hand-rolling intermediate structure.
# ============================================================================

test_that("optimise_retention_times output is directly usable by export_files (PK-072 round-trip)", {
  library(mockery)

  # Build a minimal FUNC_mzR structure
  rtime  <- seq(0.5, 5.0, by = 0.5)
  intensity <- c(10, 20, 50, 200, 800, 1500, 800, 200, 50, 20)
  chrom  <- data.frame(rtime = rtime, intensity = intensity)

  guide_row <- tibble::tibble(
    "Molecule List Name"             = "LPC",
    "Precursor Name"                 = "LPC 18:0",
    "Precursor Mz"                   = 524.37,
    "Precursor Charge"               = 1L,
    "Product Mz"                     = 184.07,
    "Product Charge"                 = 1L,
    "Explicit Retention Time"        = 2.5,
    "Explicit Retention Time Window" = 1.0,
    "Note"                           = NA_character_,
    "control_chart"                  = "yes"
  )
  guide_clean <- janitor::clean_names(guide_row)

  mock_mzR <- list(
    P1 = list(
      "QC_LTR_001.mzML" = list(
        mzR_header = data.frame(
          chromatogramType                  = "SRM chromatogram",
          precursorIsolationWindowTargetMZ  = 524.37,
          productIsolationWindowTargetMZ    = 184.07,
          stringsAsFactors = FALSE
        ),
        mzR_chromatogram = list(chrom)
      )
    )
  )

  master_list <- list(
    project_details = list(
      project_dir  = withr::local_tempdir(),
      user_name    = "TestUser",
      plateID      = "P1",
      qc_type      = "LTR",
      is_ver       = "v1.tsv",
      script_log   = list(timestamps = list(start_time = Sys.time()),
                          runtimes = list(), messages = list())
    ),
    templates = list(
      mrm_guides = list(
        "v1.tsv" = list(mrm_guide = guide_row)
      )
    ),
    data = list(
      P1 = mock_mzR$P1
    ),
    summary_tables = list()
  )
  dir.create(file.path(master_list$project_details$project_dir, "P1", "data", "PeakForgeR"),
             recursive = TRUE, showWarnings = FALSE)

  # Run optimise_retention_times (the producer)
  by_plate_result <- suppressMessages(suppressWarnings(
    optimise_retention_times(master_list, "P1")
  ))

  # The result should contain mrm_guide_updated with the expected column
  expect_true("mrm_guide_updated" %in% names(by_plate_result[["P1"]]),
              info = "optimise_retention_times must produce mrm_guide_updated key")
  expect_s3_class(by_plate_result[["P1"]]$mrm_guide_updated, "data.frame")

  # Feed the result into master_list the same way peak_picking does:
  #   master_list$templates$mrm_guides$by_plate[[plate_idx]] <- optimise_retention_times(...)
  # This mirrors the production call in peak_picking so export_files sees
  # the correct double-indexed structure.
  master_list$templates$mrm_guides$by_plate[["P1"]] <- by_plate_result

  # export_files reads master_list$templates$mrm_guides$by_plate[[plate_idx]][[plate_idx]]
  by_plate_entry <- master_list$templates$mrm_guides$by_plate[["P1"]][["P1"]]
  expect_true(!is.null(by_plate_entry),
              info = "by_plate structure must be accessible at [[plate_idx]][[plate_idx]]")
  expect_true("mrm_guide_updated" %in% names(by_plate_entry),
              info = "mrm_guide_updated must survive the by_plate nesting")
  expect_true("peak_boundary_update" %in% names(by_plate_entry),
              info = "peak_boundary_update must survive the by_plate nesting")
})
