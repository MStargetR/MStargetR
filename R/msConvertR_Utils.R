# msConvertR_Utils ----
# sub functions for msConvertR function


#' Import specific functions from packages
#' @keywords internal
#' @name msConvertR_import_external_functions
#' @importFrom future future value

NULL
#'
#mzML File Conversion Functions ----
#' Validate input Directory
#'
#' This function checks if the `input_directory` parameter is a single string and if the specified directory exists.
#' @keywords internal
#' @param input_directory A character string representing the path to the project directory.
#' @return TRUE if the validation is successful, otherwise an error is thrown.
#' @examples
#' \dontrun{
#' validate_project_directory("input_directory")
#' }
validate_input_directory <- function(input_directory) {
  # Check if input_directory is a single string
  if (!is.character(input_directory) ||
      length(input_directory) != 1) {
    stop("validate_input_directory: 'input_directory' must be a single character string. Got: ",
         paste(class(input_directory), collapse = ", "),
         " of length ", length(input_directory), ".", call. = FALSE)
  }

  if (nchar(input_directory) == 0) {
    stop("validate_input_directory: 'input_directory' must not be an empty string.",
         call. = FALSE)
  }

  # Check if the specified directory exists
  if (!dir.exists(input_directory)) {
    stop("validate_input_directory: The specified input directory does not exist: '",
         input_directory, "'.", call. = FALSE)
  }

  # Return TRUE if validation is successful
  message(paste("Accessing project directory ", input_directory))
}

###Primary Function----
#' msConvertR_mzml_conversion
#'
#' This function converts raw vendor files to mzML format using ProteoWizard's msconvert tool, restructures directories, and updates the script log.
#' @keywords internal
#' @param input_directory Directory path for project folder
#' @param output_directory Directory path for project folder if different from
#' input directory.
#' @param plateIDs vector of vendor files names to be converted
#' @param vendor_extension_patterns character string of vendor file extensions.
#' @return Converted mzml files.
#' @examples
#' \dontrun{
#' msConvertR_mzml_conversion(input_directory, output_directory, plateIDs)
#' }
msConvertR_mzml_conversion <- function(input_directory,
                                       output_directory,
                                       plateIDs,
                                       vendor_extension_patterns,
                                       sanitized_plateIDs = plateIDs,
                                       enable_HPC = getOption("MStargetR.enable_HPC", FALSE)) {
  # Restore wd even on error / interrupt (equivalent to withr::with_dir;
  # withr is Suggests-only). See REVIEW_REPORT BC-H10.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  message("msConvertR: Starting mzML conversion for ",
          length(plateIDs), " plate(s)...")
  msConvertR_set_working_directory(input_directory)
  # Use sanitized IDs for directory names; raw IDs for disk file matching
  msConvertR_setup_project_directories(output_directory, sanitized_plateIDs)
  commands <- msConvertR_construct_command_for_terminal(
    input_directory, output_directory, plateIDs, sanitized_plateIDs,
    enable_HPC = enable_HPC
  )
  active_plateIDs <- attr(commands, "active_plateIDs")
  if (length(commands) > 0) {
    msConvertR_execute_command(commands, output_directory, active_plateIDs,
                               enable_HPC = enable_HPC)
  } else {
    message("msConvertR: All plates already have mzML output -- nothing to convert.")
  }
  message("msConvertR: Restructuring output directories...")
  msConvertR_restructure_directory(output_directory, sanitized_plateIDs, vendor_extension_patterns)
  message("msConvertR: mzML conversion pipeline complete.")
}

###Sub Functions----

#' msConvertR_setup_project_directories
#'
#' This function sets up project directories for each plate ID.
#' @keywords internal
#' @param output_directory Output directory for mzml files.
#' @param plateIDs vector of vendor files names being converted
#' @return None. The function sets up directories.
#' @examples
#' \dontrun{
#' msConvertR_setup_project_directories(output_directory, plateIDs)
#' }
msConvertR_setup_project_directories <- function(output_directory, plateIDs) {
  message("  Setting up project directories for ", length(plateIDs), " plate(s)...")
  dir.create(file.path(output_directory, "user_files"), showWarnings = FALSE)
  for (plateID in plateIDs) {
    base_path <- file.path(output_directory, plateID)
    dir.create(base_path, showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(base_path, "data"),
               showWarnings = FALSE,
               recursive = TRUE)
    dir.create(
      file.path(base_path, "data", "mzml"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    dir.create(
      file.path(base_path, "data", "qs2"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    dir.create(
      file.path(base_path, "data", "PeakForgeR"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    dir.create(
      file.path(base_path, "data", "raw_data"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    dir.create(
      file.path(base_path, "data", "batch_correction"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    dir.create(
      file.path(base_path, "html_report"),
      showWarnings = FALSE,
      recursive = TRUE
    )
  }
}


#' msConvertR_set_working_directory
#'
#' This function sets the working directory to the project directory.
#' The caller is responsible for restoring the previous wd via
#' \code{on.exit(setwd(old_wd))} (see \code{msConvertR_mzml_conversion}).
#' withr::with_dir would be preferable but withr is only in
#' DESCRIPTION Suggests (see REVIEW_REPORT BC-H10).
#' @keywords internal
#' @param directory Directory path for the project folder.
#' @return None. The function sets the working directory.
#' @examples
#' \dontrun{
#' msConvertR_set_working_directory("path/to/output_directory")
#' }
msConvertR_set_working_directory <- function(directory) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(directory)
  on.exit(NULL)  # setwd succeeded -- cancel the safety restore so the new wd persists
}

#' msConvertR_construct_commands_for_terminal
#'
#' This function constructs the command for terminal to convert files to mzML format.
#' @keywords internal
#' @param input_directory path to input directory containing vendor files
#' @param output_directory path to output directory.
#' @param plateIDs The names of vendor files to convert
#' @return The constructed command string.
#' @examples
#' \dontrun{
#' command <- msConvertR_construct_command_for_terminal(path/to/input/directory,
#'                                                      "path/to/output_directory")
#' }
msConvertR_construct_command_for_terminal <- function(input_directory, output_directory,
                                                     plateIDs,
                                                     sanitized_plateIDs = plateIDs,
                                                     enable_HPC = getOption("MStargetR.enable_HPC", FALSE)) {
  message("  Constructing msconvert commands (runtime: ",
          if (isTRUE(enable_HPC)) "Apptainer" else "Docker", ")...")

  # Track which plates are skipped vs need conversion
  skip_env <- new.env(parent = emptyenv())
  skip_env$skipped <- character(0)

  commands <- lapply(seq_along(plateIDs), function(idx) {
    plateID <- plateIDs[idx]
    saneID  <- sanitized_plateIDs[idx]
    input_path <- normalizePath(file.path(input_directory, "raw_data"), mustWork = FALSE)
    output_dir <- normalizePath(file.path(output_directory, saneID, "data", "mzml"), mustWork = FALSE)

    # Skip plates that already have mzML output from a previous run
    existing_mzml <- list.files(output_dir, pattern = "\\.mzML$", ignore.case = TRUE)
    if (length(existing_mzml) > 0) {
      message("    [", saneID, "] Already has ", length(existing_mzml),
              " mzML file(s) -- skipping conversion")
      skip_env$skipped <- c(skip_env$skipped, saneID)
      return(NULL)
    }

    container_data_path <- "/data"
    output_data_path <- "/output"

    # MS-003: Use exact match after stripping vendor extensions to prevent
    # "Plate_1" from matching "Plate_10_sample.wiff".
    vendor_ext_pat <- "\\.(d|baf|fid|yep|tsf|tdf|mbi|wiff|wiff\\.scan|scan|wiff2|qgd|qgb|qgm|lcd|lcdproj|raw|uep|sdf|dat|wcf|wproj|wdata)$"
    files <- list.files(input_path)
    stripped <- sub(vendor_ext_pat, "", files, ignore.case = FALSE)
    file_name <- files[stripped == plateID]
    # MS-004: Anchor the companion-file filter so only trailing ".scan" is removed,
    # not legitimate vendor filenames containing ".scan" mid-string.
    file_name <- file_name[!grepl("\\.scan$", file_name)]

    if (length(file_name) != 1) {
      stop("msConvertR: Expected exactly one file matching plateID '", plateID,
           "' in '", input_path, "', found ", length(file_name), ".",
           call. = FALSE)
    }

    # MS-005: Normalise host path to forward slashes so Docker Desktop parses
    # the "-v host:container" mount correctly on Windows, and prevents any
    # mid-path colon from being mistaken for the separator.  Add ":ro" to the
    # input bind-mount and harden the container with network isolation,
    # capability drop, and resource limits.
    # IMPORTANT: do NOT shQuote() individual args here. system2() already
    # quotes each element of `args` before invoking the process. See
    # REVIEW_REPORT DOCK-C1.
    host_input  <- gsub("\\\\", "/", input_path)
    host_output <- gsub("\\\\", "/", output_dir)

    # DOCK-C3: On Windows, paths containing spaces (e.g. "OneDrive - Org",
    # "Program Files") cause docker to error with "invalid reference format"
    # because cmd.exe / docker.exe drops the shQuote-added quotes during the
    # command-line handoff, splitting the -v value on the spaces. Route the
    # host paths through Sys.junction-based aliases under a no-spaces tempdir
    # whenever spaces are present. Junctions are recorded so the caller can
    # clean them up after the future-based execution finishes.
    # mst_make_safe_mount_path() is a no-op on non-Windows, so it is safe to
    # call unconditionally even when enable_HPC = TRUE (Linux/HPC).
    input_mount  <- mst_make_safe_mount_path(host_input,  prefix = "mst_in_")
    output_mount <- mst_make_safe_mount_path(host_output, prefix = "mst_out_")

    # DOCK-C4: --user=1000:1000 was part of the DOCK-C1 hardening pass, but
    # on Windows it adds no real privilege drop (the docker daemon runs in a
    # WSL2 VM, not on the Windows host) AND frequently breaks writes to
    # bind-mounted output directories, since Windows host files have no
    # Linux UID/GID to map to. Apply it only on POSIX hosts. Apptainer runs
    # as the invoking user automatically, so the flag is omitted there.
    docker_extra_args <- if (.Platform$OS.type == "windows") NULL else "--user=1000:1000"

    # DOCK-C5/DOCK-C6 notes: standard Docker hardening flags (--rm,
    # --cap-drop=ALL, --network=none, --security-opt seccomp=unconfined) are
    # emitted by run_container() for the Docker path. Apptainer runs as the
    # invoking user with no extra hardening flags by default.
    image_command <- c(
      "wine", "msconvert",
      file.path(container_data_path, file_name),
      "-o", output_data_path
    )
    binds <- list(
      list(host = input_mount$safe_path,  container = container_data_path, ro = TRUE),
      list(host = output_mount$safe_path, container = output_data_path,    ro = FALSE)
    )

    # Legacy docker argv shape kept alongside the new image_command/binds so
    # tests that inspect `commands[[i]]$docker_args` still work unchanged.
    # msConvertR_execute_command() prefers image_command + binds when present
    # and falls back to docker_args for legacy fakes.
    docker_image <- mstargetr_image_ref()
    docker_args <- c(
      "run", "--rm",
      "--network=none",
      "--cap-drop=ALL",
      "--security-opt", "seccomp=unconfined",
      docker_extra_args,
      "-v", paste0(input_mount$safe_path,  ":", container_data_path, ":ro"),
      "-v", paste0(output_mount$safe_path, ":", output_data_path),
      docker_image,
      image_command
    )

    list(
      docker_args = docker_args,
      image_command = image_command,
      binds = binds,
      docker_extra_args = docker_extra_args,
      saneID = saneID,
      junctions = c(input_mount$junction, output_mount$junction)
    )
  })

  # Remove NULL entries (skipped plates) and carry the sanitized display IDs
  non_null <- !vapply(commands, is.null, logical(1))
  commands <- commands[non_null]
  skipped_sane <- sanitized_plateIDs[!non_null]
  active_ids   <- vapply(commands, function(x) x$saneID, character(1))

  if (length(skipped_sane) > 0) {
    message("  Skipped ", length(skipped_sane),
            " plate(s) with existing mzML output: ",
            paste(skipped_sane, collapse = ", "))
  }

  # In verbose mode show the full docker argv (always built, even when
  # enable_HPC = TRUE) so users can see exactly what would be invoked.
  # When enable_HPC = TRUE the dispatcher logs the actual apptainer argv
  # at run_container() dispatch time.
  if (isTRUE(getOption("MStargetR.verbose", FALSE))) {
    for (i in seq_along(commands)) {
      message("    [", active_ids[i], "] docker ",
              paste(commands[[i]]$docker_args, collapse = " "))
    }
  } else {
    for (i in seq_along(commands)) {
      message("    [", active_ids[i], "] queued for conversion")
    }
  }

  attr(commands, "active_plateIDs") <- active_ids
  return(commands)
}

#' msConvertR_execute_command
#'
#' This function executes the command to convert files to mzML format.
#' @keywords internal
#' @param commands List of lists, each containing \code{docker_args} (character
#'   vector passed to \code{system2}) and \code{saneID} (sanitized plate ID).
#' @param output_directory Character string. Root output directory where a
#'   \code{MStargetR_logs} sub-directory will be created.
#' @param plateIDs Character vector of sanitized plate IDs corresponding to
#'   each element of \code{commands}.
#' @return Named logical list; \code{TRUE} for each plate that converted
#'   successfully, \code{FALSE} otherwise.
#' @examples
#' \dontrun{
#' msConvertR_execute_command(commands, output_directory, plateIDs)
#' }
msConvertR_execute_command <- function(commands, output_directory, plateIDs,
                                       enable_HPC = getOption("MStargetR.enable_HPC", FALSE)) {
  message("Converting vendor files:\n", paste(plateIDs,collapse = "\n"))

  # Guard against path-traversal in output_directory (e.g. from Shiny input).
  # Inspect the raw path for `..` components BEFORE normalising, because on
  # Windows normalizePath() resolves `..` when the parent exists and we
  # would silently accept an escape into the parent directory. Split on
  # either separator so `foo\..\bar` and `foo/../bar` are both rejected.
  raw_components <- unlist(strsplit(output_directory, "[/\\\\]"))
  if (any(raw_components == "..")) {
    stop("msConvertR_execute_command: 'output_directory' must not contain '..' components: '",
         output_directory, "'.", call. = FALSE)
  }
  norm_out <- normalizePath(output_directory, mustWork = FALSE, winslash = "/")
  lnk <- Sys.readlink(output_directory)
  if (!is.na(lnk) && nzchar(lnk)) {
    stop("msConvertR_execute_command: 'output_directory' must not be a symbolic link: '",
         output_directory, "'.", call. = FALSE)
  }

  logs_dir <- file.path(output_directory, "MStargetR_logs")
  dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)

  # DOCK-C3 cleanup: gather any safe-mount junctions that
  # msConvertR_construct_command_for_terminal created and remove them when
  # this function exits (success, error, or interrupt). recursive = FALSE
  # inside the helper is critical -- a recursive unlink on a junction
  # would traverse INTO the user's real data and delete it.
  all_junctions <- unique(unlist(
    lapply(commands, function(cmd) cmd$junctions),
    use.names = FALSE
  ))
  on.exit(mst_cleanup_mount_junctions(all_junctions), add = TRUE)

  # Use workers= argument so the concurrency cap is honoured correctly.
  total_cores <- future::availableCores()
  available_cores <- if (total_cores <= 2) 1 else total_cores - 2
  future::plan(future::multisession, workers = available_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  # Resolve the SIF up-front on the main session when running on HPC. This
  # ensures the (potentially slow / network-bound) apptainer pull happens
  # exactly once instead of being raced by every future, and surfaces any
  # pull failure as a clear error before workers spin up.
  if (isTRUE(enable_HPC)) resolve_sif()

  # Start conversion tasks as futures; pass only the per-plate data to avoid
  # serialising the full commands list and plateIDs vector to every worker.
  futures <- lapply(seq_along(commands), function(i) {
    image_cmd <- commands[[i]]$image_command
    bnds      <- commands[[i]]$binds
    dx_args   <- commands[[i]]$docker_extra_args
    legacy_args <- commands[[i]]$docker_args
    pid       <- plateIDs[i]
    ldir      <- logs_dir
    hpc_flag  <- enable_HPC
    future::future({
      plateID <- pid
      log_file <- file.path(ldir, paste0(plateID, "_MStargetR_log.txt"))

      start_time <- Sys.time()

      # Prefer the runtime-aware dispatcher when image_command + binds are
      # present. Fall back to a direct system2("docker", ...) on the legacy
      # docker_args field for synthetic command lists used in unit tests.
      if (!is.null(image_cmd) && !is.null(bnds)) {
        output <- run_container(
          image_command     = image_cmd,
          binds             = bnds,
          enable_HPC        = hpc_flag,
          docker_extra_args = dx_args,
          stdout            = TRUE,
          stderr            = TRUE
        )
        cmd_for_log <- paste(image_cmd, collapse = " ")
      } else if (!is.null(legacy_args)) {
        output <- system2("docker", args = legacy_args,
                          stdout = TRUE, stderr = TRUE)
        cmd_for_log <- paste("docker", paste(legacy_args, collapse = " "))
      } else {
        stop("msConvertR_execute_command: each command must provide either ",
             "image_command/binds or docker_args.", call. = FALSE)
      }
      exit_status <- attr(output, "status")
      success <- is.null(exit_status) || exit_status == 0

      # Redact credential-helper tokens, Bearer auth headers, and base64 JWT
      # fragments that Docker Desktop or ProteoWizard may emit to stderr/stdout.
      # JWT pattern must run first so it fires before the Bearer consumer
      # absorbs the token body.
      redact_output <- function(lines) {
        lines <- gsub("ey[A-Za-z0-9_-]{10,}[.][A-Za-z0-9_-]+[.][A-Za-z0-9_-]*",
                      "<JWT_REDACTED>", lines)
        lines <- gsub("Bearer[[:space:]]+[A-Za-z0-9+/=_-]+", "Bearer <REDACTED>", lines)
        lines <- gsub("sha256:[A-Fa-f0-9]{8,}", "sha256:<REDACTED>", lines)
        lines
      }
      safe_output <- redact_output(output)

      writeLines(enc2utf8(c(
        paste("Start time:", start_time),
        paste("Runtime:", if (isTRUE(hpc_flag)) "Apptainer" else "Docker"),
        paste("Command:", cmd_for_log),
        "Output:",
        safe_output,
        paste("End time:", Sys.time()),
        paste("Status:", if (success) "SUCCESS" else "FAILURE")
      )), con = log_file)

      # Return a list so the collector can print the status message outside the
      # future (with future::multisession, messages emitted inside a future are
      # batched and only flushed when future::value() collects the result).
      list(plateID = plateID, success = success)
    })
  })

  # Collect results (blocks until all futures complete) then print per-plate
  # status in the main session so messages appear as each plate finishes.
  message("  Waiting for ", length(futures), " conversion(s) to finish...")
  raw_results <- lapply(futures, future::value)
  for (res in raw_results) {
    message(sprintf("Finished conversion for %s - %s",
                    res$plateID, if (res$success) "SUCCESS" else "FAILURE"))
  }
  results <- stats::setNames(
    lapply(raw_results, function(r) r$success),
    vapply(raw_results, function(r) r$plateID, character(1))
  )

  # Post-run verification: confirm that at least one new .mzML file appeared in
  # each plate's output directory.  Docker reports the *docker* exit code (0 for
  # a clean container exit), not msconvert's internal status, so a zero exit
  # code alone is insufficient evidence of successful conversion.
  for (pid in names(results)) {
    if (isTRUE(results[[pid]])) {
      mzml_dir <- file.path(output_directory, pid, "data", "mzml")
      new_mzml <- list.files(mzml_dir, pattern = "\\.mzML$",
                              full.names = FALSE, recursive = FALSE)
      if (length(new_mzml) == 0L) {
        results[[pid]] <- FALSE
        message("msConvertR: No .mzML files found for '", pid,
                "' in '", mzml_dir, "' - marking as FAILURE.")
      }
    }
  }

  n_success <- sum(unlist(results))
  n_fail <- length(results) - n_success
  message("msConvertR conversion summary: ", n_success, " succeeded, ",
          n_fail, " failed out of ", length(results), " plate(s).")
  if (n_fail > 0) {
    failed_plates <- names(results)[!unlist(results)]
    message("  Failed plate(s): ", paste(failed_plates, collapse = ", "))
    stop(
      sprintf("msConvertR: %d of %d plate(s) failed conversion: %s",
              length(failed_plates), length(results),
              paste(failed_plates, collapse = ", ")),
      call. = FALSE
    )
  }

  return(results)
}

#' msConvertR_restructure_directory
#'
#' This function restructures the directory by moving raw_data and mzML files to correct locations.
#' @keywords internal
#' @param output_directory Output directory where the mzML files will be stored.
#' @param plateIDs filenames for plates being converted with no extension.
#' @param vendor_extension_patterns vector of file extensions for vendor files
#' @return \code{invisible(NULL)}. Called for its side effects.
#' @examples
#' \dontrun{
#' master_list <- msConvertR_restructure_directory(output_directory,
#'                                                 plateIDs,
#'                                                 vendor_extension_patterns)
#' }
msConvertR_restructure_directory <- function(output_directory,
                                             plateIDs,
                                             vendor_extension_patterns) {
  for (plateID in plateIDs) {
    # Define key paths
    raw_data_dir <- file.path(output_directory, "raw_data")
    plate_data_dir <- file.path(output_directory, plateID, "data")
    raw_data_dest <- file.path(plate_data_dir, "raw_data")
    mzml_dest <- file.path(plate_data_dir, "mzml")

    # Create destination directories if they don't exist
    dir.create(raw_data_dest,
               recursive = TRUE,
               showWarnings = FALSE)
    dir.create(mzml_dest, recursive = TRUE, showWarnings = FALSE)

    # Find raw data files matching plateID.
    # Match against basename only to avoid spurious hits on parent-directory
    # components that happen to contain the plateID substring.
    raw_files <- list.files(path = raw_data_dir,
                            pattern = vendor_extension_patterns,
                            full.names = TRUE)
    matched_raw_files <- raw_files[grepl(plateID, basename(raw_files), fixed = TRUE)]

    # Copy individual raw files (non-directories)
    file_info <- file.info(matched_raw_files)
    raw_file_paths <- matched_raw_files[!file_info$isdir]
    message("    [", plateID, "] Copying ", length(raw_file_paths),
            " raw file(s) to: ", raw_data_dest)
    copy_ok <- file.copy(from = raw_file_paths,
                         to = raw_data_dest,
                         recursive = FALSE)
    if (!all(copy_ok)) {
      failed_files <- raw_file_paths[!copy_ok]
      stop("msConvertR: Failed to copy ", length(failed_files),
           " raw file(s) for plate '", plateID,
           "'. Check disk space and permissions. Files: ",
           paste(basename(failed_files), collapse = ", "),
           call. = FALSE)
    }

    # Copy entire .d directories
    d_dirs <- matched_raw_files[file_info$isdir]
    for (dir in d_dirs) {
      copy_ok_d <- file.copy(from = dir,
                            to = raw_data_dest,
                            recursive = TRUE)
      if (!all(copy_ok_d)) {
        stop("msConvertR: Failed to copy .d directory '", basename(dir),
             "' for plate '", plateID,
             "'. Check disk space and permissions.",
             call. = FALSE)
      }
    }

    # Verify mzML files produced by Docker (written directly to mzml_dest).
    # Exclude ANPC conditioning/blank/ISTDs files using the shared helper so
    # the pattern is defined in one place (config.R) and is unit-testable.
    existing_mzml <- list.files(path = mzml_dest,
                                pattern = "\\.mzML$",
                                full.names = TRUE)
    existing_mzml <- existing_mzml[!is_qc_support_file(existing_mzml)]
    message("    [", plateID, "] ", length(existing_mzml),
            " mzML file(s) in: ", mzml_dest)

  }
}
