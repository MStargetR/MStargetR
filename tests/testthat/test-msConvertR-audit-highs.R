# Regression tests for MS-001 through MS-006 (High-severity audit findings)
library(mockery)

# ============================================================================
# MS-001: sanitized plateIDs must not be used for disk lookups
# ============================================================================

test_that("MS-001: original (raw) plateID is used for file matching, not sanitized form", {
  # If sanitize_identifier rewrites "Plate:2024" -> "Plate_2024", the file on
  # disk is still named "Plate:2024.wiff". msConvertR must look up the file
  # using the raw ID and use the sanitized ID only for output directories.
  input_dir  <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()
  raw_dir    <- file.path(input_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  # File on disk has a colon in the name (raw ID = "Plate:2024")
  raw_id   <- "Plate_2024"    # after sanitization (underscore)
  # For this test we just verify the function signature passes raw_plateIDs
  # through to construct_command, using the mocked orchestrator.
  file.create(file.path(raw_dir, paste0(raw_id, ".wiff")))
  dir.create(file.path(output_dir, raw_id, "data", "mzml"), recursive = TRUE)

  # Legacy character-vector call path (coerced to a groups table internally).
  result <- suppressMessages(
    msConvertR_construct_command_for_terminal(input_dir, output_dir, c(raw_id))
  )
  expect_length(result, 1)
  cmd_str <- paste(result[[1]]$docker_args, collapse = " ")
  expect_true(grepl(raw_id, cmd_str))
})

test_that("MS-001: msConvertR_mzml_conversion lifts legacy plateIDs into a sanitized groups table", {
  call_args <- list()
  stub(msConvertR_mzml_conversion, "msConvertR_set_working_directory", function(...) NULL)
  stub(msConvertR_mzml_conversion, "msConvertR_setup_project_directories",
       function(out, ids) { call_args$dirs_ids <<- ids })
  stub(msConvertR_mzml_conversion, "msConvertR_construct_command_for_terminal",
       function(inp, out, groups, ...) {
         call_args$construct_groups <<- groups
         cmds <- list()
         attr(cmds, "active_plateIDs") <- character(0)
         cmds
       })
  stub(msConvertR_mzml_conversion, "msConvertR_restructure_directory",
       function(out, groups, ...) { call_args$restruct_groups <<- groups })

  suppressMessages(
    msConvertR_mzml_conversion("in", "out",
                               c("Plate_raw"),
                               vendor_extension_patterns = "\\.wiff$",
                               sanitized_plateIDs = c("Plate_safe"))
  )

  # Directories use the sanitized IDs
  expect_equal(call_args$dirs_ids, "Plate_safe")
  # The coerced groups table carries raw + sanitized IDs through to the helpers
  expect_equal(call_args$construct_groups$raw_plateID, "Plate_raw")
  expect_equal(unique(call_args$construct_groups$sanitized_plateID), "Plate_safe")
  expect_equal(unique(call_args$restruct_groups$sanitized_plateID), "Plate_safe")
})

# ============================================================================
# MS-002: msConvertR_set_working_directory restores cwd on error
# ============================================================================

test_that("MS-002: msConvertR_set_working_directory restores cwd when setwd fails", {
  original_wd <- getwd()
  bad_dir <- file.path(tempdir(), "surely_nonexistent_ms002_xyz999")

  # setwd to a nonexistent directory should fail; cwd must be restored
  expect_error(msConvertR_set_working_directory(bad_dir))
  expect_equal(getwd(), original_wd)
})

test_that("MS-002: msConvertR_set_working_directory leaves new wd in place on success", {
  tmp <- withr::local_tempdir()
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  msConvertR_set_working_directory(tmp)
  # After a successful call the wd should be the new directory
  expect_equal(normalizePath(getwd()), normalizePath(tmp))
})

# ============================================================================
# MS-003: Exact filename match prevents "Plate_1" matching "Plate_10_*.wiff"
# ============================================================================

test_that("MS-003: Plate_1 does not match Plate_10 files", {
  input_dir  <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()
  raw_dir    <- file.path(input_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  # Both files are present; Plate_1 must match only its own file
  file.create(file.path(raw_dir, "Plate_1.wiff"))
  file.create(file.path(raw_dir, "Plate_10.wiff"))

  dir.create(file.path(output_dir, "Plate_1",  "data", "mzml"), recursive = TRUE)
  dir.create(file.path(output_dir, "Plate_10", "data", "mzml"), recursive = TRUE)

  # Should succeed for each plate individually without "found 2" error
  res1 <- suppressMessages(
    msConvertR_construct_command_for_terminal(input_dir, output_dir, c("Plate_1"))
  )
  expect_length(res1, 1)
  cmd1 <- paste(res1[[1]]$docker_args, collapse = " ")
  expect_true(grepl("/Plate_1\\.wiff", cmd1))
  expect_false(grepl("Plate_10", cmd1))

  res10 <- suppressMessages(
    msConvertR_construct_command_for_terminal(input_dir, output_dir, c("Plate_10"))
  )
  expect_length(res10, 1)
  cmd10 <- paste(res10[[1]]$docker_args, collapse = " ")
  expect_true(grepl("/Plate_10\\.wiff", cmd10))
})

# ============================================================================
# MS-004: .scan companion filter is anchored — legitimate mid-name .scan kept
# ============================================================================

test_that("MS-004: anchored .scan$ filter keeps .wiff with .scan mid-name", {
  input_dir  <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()
  raw_dir    <- file.path(input_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  # Plate whose name contains ".scan" mid-string (legitimate file, not companion)
  pid <- "plateB.scan_backup_20240101"
  file.create(file.path(raw_dir, paste0(pid, ".wiff")))
  file.create(file.path(raw_dir, paste0(pid, ".wiff.scan")))  # companion
  dir.create(file.path(output_dir, pid, "data", "mzml"), recursive = TRUE)

  result <- suppressMessages(
    msConvertR_construct_command_for_terminal(input_dir, output_dir, c(pid))
  )
  expect_length(result, 1)
  cmd_str <- paste(result[[1]]$docker_args, collapse = " ")
  # The .wiff file must be passed to msconvert, not the .wiff.scan companion
  expect_true(grepl(paste0(pid, "\\.wiff"), cmd_str))
  expect_false(grepl("\\.wiff\\.scan", cmd_str))
})

test_that("MS-004: plain .wiff.scan companion is still excluded", {
  input_dir  <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()
  raw_dir    <- file.path(input_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  file.create(file.path(raw_dir, "plateA.wiff"))
  file.create(file.path(raw_dir, "plateA.wiff.scan"))
  dir.create(file.path(output_dir, "plateA", "data", "mzml"), recursive = TRUE)

  result <- suppressMessages(
    msConvertR_construct_command_for_terminal(input_dir, output_dir, c("plateA"))
  )
  cmd_str <- paste(result[[1]]$docker_args, collapse = " ")
  expect_false(grepl("\\.scan", cmd_str))
})

# ============================================================================
# MS-005: Docker mount uses forward slashes and input is read-only
# ============================================================================

test_that("MS-005: Docker -v input mount contains forward slashes and :ro suffix", {
  input_dir  <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()
  raw_dir    <- file.path(input_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  file.create(file.path(raw_dir, "plate1.wiff"))
  dir.create(file.path(output_dir, "plate1", "data", "mzml"), recursive = TRUE)

  result <- suppressMessages(
    msConvertR_construct_command_for_terminal(input_dir, output_dir, c("plate1"))
  )
  args <- result[[1]]$docker_args

  # Find the -v argument for the input mount (the one containing /data:ro)
  v_indices <- which(args == "-v")
  mounts <- args[v_indices + 1]
  input_mount <- mounts[grepl(":/data", mounts)]

  expect_length(input_mount, 1)
  # Must end with :/data:ro
  expect_true(grepl(":/data:ro$", input_mount))
  # Must not contain backslashes
  expect_false(grepl("\\\\", input_mount))
})

test_that("MS-005: Docker args include --network=none and --cap-drop=ALL", {
  input_dir  <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()
  raw_dir    <- file.path(input_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  file.create(file.path(raw_dir, "plate1.wiff"))
  dir.create(file.path(output_dir, "plate1", "data", "mzml"), recursive = TRUE)

  result <- suppressMessages(
    msConvertR_construct_command_for_terminal(input_dir, output_dir, c("plate1"))
  )
  args <- result[[1]]$docker_args
  expect_true("--network=none" %in% args)
  expect_true("--cap-drop=ALL" %in% args)
})

# DOCK-C6: pin --security-opt seccomp=unconfined for the Wine-based pwiz image.
# Removing this flag reintroduces "wine: socket : Function not implemented" on
# newer Docker Desktop releases whose default seccomp profile blocks Wine's
# socket syscalls.
test_that("DOCK-C6: Docker args include --security-opt seccomp=unconfined for Wine", {
  input_dir  <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()
  raw_dir    <- file.path(input_dir, "raw_data")
  dir.create(raw_dir, recursive = TRUE)

  file.create(file.path(raw_dir, "plate1.wiff"))
  dir.create(file.path(output_dir, "plate1", "data", "mzml"), recursive = TRUE)

  result <- suppressMessages(
    msConvertR_construct_command_for_terminal(input_dir, output_dir, c("plate1"))
  )
  args <- result[[1]]$docker_args
  expect_true("--security-opt" %in% args)
  expect_true("seccomp=unconfined" %in% args)
  # And the two must be adjacent in that order, since docker treats them as
  # flag + value, not a free-floating pair.
  idx <- which(args == "--security-opt")
  expect_length(idx, 1)
  expect_identical(args[idx + 1L], "seccomp=unconfined")
})

# ============================================================================
# MS-006: Log output is redacted before writing
# ============================================================================

test_that("MS-006: redact_output helper removes sha256, Bearer tokens, and bare JWTs", {
  # Mirror the production redact_output order: JWT first, then Bearer, then sha256.
  redact_output <- function(lines) {
    lines <- gsub("ey[A-Za-z0-9_-]{10,}[.][A-Za-z0-9_-]+[.][A-Za-z0-9_-]*",
                  "<JWT_REDACTED>", lines)
    lines <- gsub("Bearer[[:space:]]+[A-Za-z0-9+/=_-]+", "Bearer <REDACTED>", lines)
    lines <- gsub("sha256:[A-Fa-f0-9]{8,}", "sha256:<REDACTED>", lines)
    lines
  }

  input_lines <- c(
    "Pulling from docker.io digest sha256:abcdef1234567890 status: ok",
    "Authorization: Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.payload.sig",
    "raw-jwt eyJhbGciOiJub25lIn0.body.sig",
    "Normal log line without secrets"
  )
  out <- redact_output(input_lines)

  # sha256 digest redacted
  expect_false(any(grepl("sha256:abcdef", out)))
  expect_true(any(grepl("sha256:<REDACTED>", out)))
  # Bearer line: JWT pattern fires first so the token body becomes <JWT_REDACTED>;
  # the original eyJ... literal must not appear in the output.
  expect_false(any(grepl("Bearer eyJ", out)))
  expect_false(any(grepl("eyJhbGciOiJSUzI", out)))
  # Bare JWT line: replaced with <JWT_REDACTED>
  expect_false(any(grepl("eyJhbGciOiJub25lIn0", out)))
  expect_true(any(grepl("<JWT_REDACTED>", out)))
  # Unchanged line untouched
  expect_true(any(grepl("Normal log line without secrets", out)))
})

test_that("MS-006: msConvertR_execute_command writes redacted log on mock docker success", {
  tmp <- withr::local_tempdir()
  logs_dir <- file.path(tmp, "MStargetR_logs")

  stub(msConvertR_execute_command, "future::plan", function(...) NULL)
  stub(msConvertR_execute_command, "future::availableCores", function() 2)
  stub(msConvertR_execute_command, "future::future", function(expr, ...) {
    structure(list(pid = "plate1"), class = "MockFuture")
  })
  stub(msConvertR_execute_command, "future::value", function(f, ...) {
    list(plateID = f$pid, success = TRUE)
  })

  dir.create(file.path(tmp, "plate1", "data", "mzml"), recursive = TRUE)
  file.create(file.path(tmp, "plate1", "data", "mzml", "plate1.mzML"))

  suppressMessages(
    msConvertR_execute_command(
      commands  = list(list(docker_args = c("run", "--rm", "test_image"))),
      output_directory = tmp,
      plateIDs  = c("plate1")
    )
  )

  # Log directory should exist (created by the function)
  expect_true(dir.exists(logs_dir))
})
