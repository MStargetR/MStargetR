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

#' derive_plate_groups
#'
#' Resolves which samples belong to which plate, producing a single membership
#' table consumed by every downstream msConvertR helper. Resolution priority:
#' \enumerate{
#'   \item \strong{Manifest} (\code{manifest=}) — explicit \code{raw_file ->
#'     plateID} mapping; overrides everything else.
#'   \item \strong{Subfolder} — a vendor file in \code{raw_data/<plateID>/}
#'     belongs to plate \code{<plateID>} (the subfolder name).
#'   \item \strong{Flat} — a vendor file directly in \code{raw_data/} forms its
#'     own plate from the filename (legacy behaviour; keeps \code{.wiff} working
#'     unchanged, as one \code{.wiff} is one multi-sample plate).
#' }
#' The returned \code{source}/\code{plate_level} columns let the caller apply the
#' refuse-and-prompt policy for ambiguous flat single-sample inputs.
#'
#' @keywords internal
#' @param input_directory Project directory containing a \code{raw_data/} folder.
#' @param manifest Optional CSV path or \code{data.frame} (see
#'   \code{read_plate_manifest}).
#' @return A \code{data.frame}, one row per vendor file, with columns
#'   \code{raw_path}, \code{file_name}, \code{rel_dir}, \code{raw_plateID},
#'   \code{sanitized_plateID}, \code{is_dir}, \code{plate_level}, \code{source}.
derive_plate_groups <- function(input_directory, manifest = NULL) {
  validated <- validate_file_types(input_directory)
  raw_root <- normalizePath(file.path(input_directory, "raw_data"),
                            mustWork = FALSE)

  file_name <- basename(validated)
  parent    <- normalizePath(dirname(validated), mustWork = FALSE)
  rel_dir   <- ifelse(parent == raw_root, "", basename(parent))
  is_dir    <- dir.exists(validated)
  # Plate-level (inherently multi-sample) vendor formats: one file == one plate.
  plate_level <- tolower(tools::file_ext(file_name)) %in% c("wiff", "wiff2")

  man <- if (!is.null(manifest)) {
    read_plate_manifest(manifest, known_files = file_name)
  } else {
    NULL
  }

  n <- length(validated)
  raw_plateID <- character(n)
  source      <- character(n)
  for (i in seq_len(n)) {
    if (!is.null(man) && file_name[i] %in% man$raw_file) {
      raw_plateID[i] <- man$plateID[match(file_name[i], man$raw_file)]
      source[i]      <- "manifest"
    } else if (nzchar(rel_dir[i])) {
      raw_plateID[i] <- rel_dir[i]
      source[i]      <- "subfolder"
    } else {
      raw_plateID[i] <- sub(MSTARGETR_VENDOR_EXT_PATTERN, "", file_name[i],
                            ignore.case = TRUE)
      source[i]      <- "flat"
    }
  }

  sanitized_plateID <- vapply(raw_plateID, function(pid) {
    sanitize_identifier(pid, context = "plateID")
  }, character(1), USE.NAMES = FALSE)

  data.frame(
    raw_path          = validated,
    file_name         = file_name,
    rel_dir           = rel_dir,
    raw_plateID       = raw_plateID,
    sanitized_plateID = sanitized_plateID,
    is_dir            = is_dir,
    plate_level       = plate_level,
    source            = source,
    stringsAsFactors  = FALSE
  )
}

#' mst_groups_from_plateIDs
#'
#' Backward-compatibility shim: builds a grouping table from a character vector
#' of plate IDs by exact-matching one vendor file per plate in
#' \code{<input_directory>/raw_data} (the pre-grouping contract). Internal
#' callers now pass a groups data.frame directly; this exists so helpers that
#' historically accepted a plateID vector keep working.
#' @keywords internal
#' @param input_directory Project directory containing \code{raw_data/}.
#' @param plateIDs Character vector of (already sanitized) plate IDs.
#' @return A grouping data.frame with the same columns as
#'   \code{derive_plate_groups()}.
mst_groups_from_plateIDs <- function(input_directory, plateIDs) {
  input_path <- normalizePath(file.path(input_directory, "raw_data"),
                              mustWork = FALSE)
  files <- list.files(input_path)
  stripped <- sub(MSTARGETR_VENDOR_EXT_PATTERN, "", files, ignore.case = FALSE)

  rows <- lapply(plateIDs, function(pid) {
    fn <- files[stripped == pid]
    # MS-004: only strip a trailing ".scan" companion, never mid-string.
    fn <- fn[!grepl("\\.scan$", fn)]
    if (length(fn) != 1) {
      stop("msConvertR: Expected exactly one file matching plateID '", pid,
           "' in '", input_path, "', found ", length(fn), ".", call. = FALSE)
    }
    full <- file.path(input_path, fn)
    data.frame(
      raw_path = full, file_name = fn, rel_dir = "",
      raw_plateID = pid, sanitized_plateID = pid,
      is_dir = dir.exists(full),
      plate_level = tolower(tools::file_ext(fn)) %in% c("wiff", "wiff2"),
      source = "flat", stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

###Primary Function----
#' msConvertR_mzml_conversion
#'
#' This function converts raw vendor files to mzML format using ProteoWizard's msconvert tool, restructures directories, and updates the script log.
#' @keywords internal
#' @param input_directory Directory path for project folder
#' @param output_directory Directory path for project folder if different from
#' input directory.
#' @param groups Plate membership table from \code{derive_plate_groups()}.
#' @return Converted mzml files.
#' @examples
#' \dontrun{
#' msConvertR_mzml_conversion(input_directory, output_directory, groups)
#' }
msConvertR_mzml_conversion <- function(input_directory,
                                       output_directory,
                                       groups,
                                       vendor_extension_patterns = MSTARGETR_VENDOR_EXT_PATTERN,
                                       sanitized_plateIDs = NULL,
                                       enable_HPC = getOption("MStargetR.enable_HPC", FALSE)) {
  # Restore wd even on error / interrupt (equivalent to withr::with_dir;
  # withr is Suggests-only). See REVIEW_REPORT BC-H10.
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  # Backward compatibility: accept a legacy character vector of plate IDs (plus
  # an optional parallel sanitized_plateIDs vector) and lift it into a minimal
  # grouping table. New callers pass the data.frame from derive_plate_groups().
  if (!is.data.frame(groups)) {
    pids <- groups
    sane <- if (!is.null(sanitized_plateIDs)) sanitized_plateIDs else pids
    n <- length(pids)
    groups <- data.frame(
      raw_path = rep(NA_character_, n), file_name = rep(NA_character_, n),
      rel_dir = rep("", n), raw_plateID = pids, sanitized_plateID = sane,
      is_dir = rep(FALSE, n), plate_level = rep(FALSE, n),
      source = rep("flat", n), stringsAsFactors = FALSE
    )
  }
  plates <- unique(groups$sanitized_plateID)
  message("msConvertR: Starting mzML conversion for ",
          length(plates), " plate(s) from ", nrow(groups), " vendor file(s)...")
  msConvertR_set_working_directory(input_directory)
  msConvertR_setup_project_directories(output_directory, plates)
  commands <- msConvertR_construct_command_for_terminal(
    input_directory, output_directory, groups,
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
  msConvertR_restructure_directory(output_directory, groups)
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
#' @param groups Plate membership table from \code{derive_plate_groups()}; one
#'   command is produced per plate, with one msconvert invocation per member.
#' @return A list of per-plate command objects, each with an \code{invocations}
#'   list. Carries an \code{active_plateIDs} attribute.
#' @examples
#' \dontrun{
#' command <- msConvertR_construct_command_for_terminal(path/to/input/directory,
#'                                                      "path/to/output_directory")
#' }
msConvertR_construct_command_for_terminal <- function(input_directory, output_directory,
                                                     groups,
                                                     enable_HPC = getOption("MStargetR.enable_HPC", FALSE)) {
  message("  Constructing msconvert commands (runtime: ",
          if (isTRUE(enable_HPC)) "Apptainer" else "Docker", ")...")

  # Backward compatibility: accept a legacy character vector of plate IDs.
  if (!is.data.frame(groups)) {
    groups <- mst_groups_from_plateIDs(input_directory, groups)
  }

  container_data_path <- "/data"
  output_data_path    <- "/output"

  # Track which plates are skipped vs need conversion
  skip_env <- new.env(parent = emptyenv())
  skip_env$skipped <- character(0)

  plates <- unique(groups$sanitized_plateID)

  # One command per plate; each command carries one msconvert invocation per
  # member vendor file, all writing into the same <plate>/data/mzml folder.
  commands <- lapply(plates, function(saneID) {
    rows <- groups[groups$sanitized_plateID == saneID, , drop = FALSE]
    output_dir <- normalizePath(file.path(output_directory, saneID, "data", "mzml"),
                                mustWork = FALSE)
    is_single <- nrow(rows) == 1L

    # Per-member skip check. A single-member plate (e.g. one .wiff, one .raw)
    # is skipped if ANY mzML already exists (preserves legacy/wiff behaviour,
    # since msconvert may split a multi-sample .wiff into many mzML). A
    # multi-member plate skips only the individual members whose own
    # <stem>.mzML already exists, so a partially-converted plate resumes.
    need <- logical(nrow(rows))
    for (j in seq_len(nrow(rows))) {
      stem <- sub(MSTARGETR_VENDOR_EXT_PATTERN, "", rows$file_name[j],
                  ignore.case = TRUE)
      already <- if (is_single) {
        length(list.files(output_dir, pattern = "\\.mzML$", ignore.case = TRUE)) > 0
      } else {
        file.exists(file.path(output_dir, paste0(stem, ".mzML")))
      }
      need[j] <- !already
      if (already) {
        message("    [", saneID, "] '", rows$file_name[j],
                "' already converted -- skipping")
      }
    }
    if (!any(need)) {
      skip_env$skipped <- c(skip_env$skipped, saneID)
      return(NULL)
    }

    # MS-005: Normalise host paths to forward slashes so Docker Desktop parses
    # the "-v host:container" mount correctly on Windows.
    # DOCK-C3: route paths containing spaces through Sys.junction aliases under
    # a no-spaces tempdir. mst_make_safe_mount_path() is a no-op on non-Windows.
    # IMPORTANT: do NOT shQuote() individual args; system2() quotes each arg.
    host_output  <- gsub("\\\\", "/", output_dir)
    output_mount <- mst_make_safe_mount_path(host_output, prefix = "mst_out_")
    junctions    <- output_mount$junction

    # Cache input mounts per unique source directory so subfolder plates (whose
    # members share one raw_data/<plate> directory) create a single junction.
    input_mounts <- list()
    get_input_mount <- function(input_path) {
      if (is.null(input_mounts[[input_path]])) {
        host_input <- gsub("\\\\", "/", input_path)
        input_mounts[[input_path]] <<- mst_make_safe_mount_path(host_input,
                                                                prefix = "mst_in_")
      }
      input_mounts[[input_path]]
    }

    docker_extra_args <- if (.Platform$OS.type == "windows") NULL else "--user=1000:1000"
    docker_image <- mstargetr_image_ref()

    invocations <- lapply(which(need), function(j) {
      file_name <- rows$file_name[j]
      rel_dir   <- rows$rel_dir[j]
      input_path <- normalizePath(file.path(input_directory, "raw_data", rel_dir),
                                  mustWork = FALSE)
      input_mount <- get_input_mount(input_path)
      junctions <<- c(junctions, input_mount$junction)

      image_command <- c(
        "wine", "msconvert",
        file.path(container_data_path, file_name),
        "-o", output_data_path
      )
      binds <- list(
        list(host = input_mount$safe_path,  container = container_data_path, ro = TRUE),
        list(host = output_mount$safe_path, container = output_data_path,    ro = FALSE)
      )
      # Legacy docker argv shape kept so tests that inspect $docker_args still work.
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
        image_command     = image_command,
        binds             = binds,
        docker_args       = docker_args,
        docker_extra_args = docker_extra_args,
        file_name         = file_name
      )
    })

    list(
      saneID            = saneID,
      invocations       = invocations,
      docker_extra_args = docker_extra_args,
      # First invocation's fields are surfaced at the top level for backward
      # compatibility with callers/tests that inspect a single command shape.
      image_command     = invocations[[1]]$image_command,
      binds             = invocations[[1]]$binds,
      docker_args       = invocations[[1]]$docker_args,
      junctions         = unique(junctions)
    )
  })

  # Remove NULL entries (plates whose every member was skipped)
  non_null <- !vapply(commands, is.null, logical(1))
  commands <- commands[non_null]
  active_ids <- vapply(commands, function(x) x$saneID, character(1))

  if (length(skip_env$skipped) > 0) {
    message("  Skipped ", length(skip_env$skipped),
            " plate(s) with existing mzML output: ",
            paste(skip_env$skipped, collapse = ", "))
  }

  # In verbose mode show the full docker argv for each member invocation
  # (always built, even when enable_HPC = TRUE) so users can see exactly what
  # would be invoked. When enable_HPC = TRUE the dispatcher logs the actual
  # apptainer argv at run_container() dispatch time.
  if (isTRUE(getOption("MStargetR.verbose", FALSE))) {
    for (i in seq_along(commands)) {
      for (inv in commands[[i]]$invocations) {
        message("    [", active_ids[i], "] docker ",
                paste(inv$docker_args, collapse = " "))
      }
    }
  } else {
    for (i in seq_along(commands)) {
      message("    [", active_ids[i], "] ",
              length(commands[[i]]$invocations), " file(s) queued for conversion")
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
    # A plate may carry several member invocations (one per vendor file); run
    # them sequentially within this plate's future so each plate keeps exactly
    # one log file and one success/failure verdict.
    invs      <- commands[[i]]$invocations
    # Backward compatibility: a synthetic command that only carries top-level
    # image_command/binds/docker_args (no $invocations) is treated as one member.
    if (is.null(invs)) {
      invs <- list(list(
        image_command     = commands[[i]]$image_command,
        binds             = commands[[i]]$binds,
        docker_args       = commands[[i]]$docker_args,
        docker_extra_args = commands[[i]]$docker_extra_args
      ))
    }
    pid       <- plateIDs[i]
    ldir      <- logs_dir
    hpc_flag  <- enable_HPC
    future::future({
      plateID <- pid
      log_file <- file.path(ldir, paste0(plateID, "_MStargetR_log.txt"))

      start_time <- Sys.time()

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

      success     <- TRUE
      all_cmds    <- character(0)
      all_output  <- character(0)
      for (inv in invs) {
        # Prefer the runtime-aware dispatcher when image_command + binds are
        # present. Fall back to a direct system2("docker", ...) on the legacy
        # docker_args field for synthetic command lists used in unit tests.
        if (!is.null(inv$image_command) && !is.null(inv$binds)) {
          output <- run_container(
            image_command     = inv$image_command,
            binds             = inv$binds,
            enable_HPC        = hpc_flag,
            docker_extra_args = inv$docker_extra_args,
            stdout            = TRUE,
            stderr            = TRUE
          )
          cmd_for_log <- paste(inv$image_command, collapse = " ")
        } else if (!is.null(inv$docker_args)) {
          output <- system2("docker", args = inv$docker_args,
                            stdout = TRUE, stderr = TRUE)
          cmd_for_log <- paste("docker", paste(inv$docker_args, collapse = " "))
        } else {
          stop("msConvertR_execute_command: each invocation must provide either ",
               "image_command/binds or docker_args.", call. = FALSE)
        }
        exit_status <- attr(output, "status")
        ok <- is.null(exit_status) || exit_status == 0
        success <- success && ok
        all_cmds <- c(all_cmds, cmd_for_log)
        all_output <- c(all_output, redact_output(output))
      }

      writeLines(enc2utf8(c(
        paste("Start time:", start_time),
        paste("Runtime:", if (isTRUE(hpc_flag)) "Apptainer" else "Docker"),
        paste("Command:", paste(all_cmds, collapse = " ; ")),
        "Output:",
        all_output,
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
#' Relocates each plate's raw vendor files (and any \code{.wiff.scan}
#' companions) into \code{<plateID>/data/raw_data}, using the explicit plate
#' membership recorded in the grouping table rather than substring matching.
#' mzML files are written directly into \code{<plateID>/data/mzml} by msconvert;
#' this function only reports their count.
#' @keywords internal
#' @param output_directory Output directory where the plate folders live.
#' @param groups Membership table from \code{derive_plate_groups()}.
#' @return \code{invisible(NULL)}. Called for its side effects.
#' @examples
#' \dontrun{
#' msConvertR_restructure_directory(output_directory, groups)
#' }
msConvertR_restructure_directory <- function(output_directory, groups,
                                             vendor_extension_patterns = MSTARGETR_VENDOR_EXT_PATTERN) {
  # Backward compatibility: a legacy character vector of plate IDs runs the
  # original substring-matched relocation inline (reads <output>/raw_data).
  # Kept inline so tests that stub file.copy() on this function still apply.
  if (!is.data.frame(groups)) {
    plateIDs <- groups
    for (plateID in plateIDs) {
      raw_data_dir <- file.path(output_directory, "raw_data")
      plate_data_dir <- file.path(output_directory, plateID, "data")
      raw_data_dest <- file.path(plate_data_dir, "raw_data")
      mzml_dest <- file.path(plate_data_dir, "mzml")
      dir.create(raw_data_dest, recursive = TRUE, showWarnings = FALSE)
      dir.create(mzml_dest, recursive = TRUE, showWarnings = FALSE)

      raw_files <- list.files(path = raw_data_dir,
                              pattern = vendor_extension_patterns,
                              full.names = TRUE)
      matched_raw_files <- raw_files[grepl(plateID, basename(raw_files), fixed = TRUE)]

      file_info <- file.info(matched_raw_files)
      raw_file_paths <- matched_raw_files[!file_info$isdir]
      message("    [", plateID, "] Copying ", length(raw_file_paths),
              " raw file(s) to: ", raw_data_dest)
      copy_ok <- file.copy(from = raw_file_paths, to = raw_data_dest,
                           recursive = FALSE)
      if (!all(copy_ok)) {
        failed_files <- raw_file_paths[!copy_ok]
        stop("msConvertR: Failed to copy ", length(failed_files),
             " raw file(s) for plate '", plateID,
             "'. Check disk space and permissions. Files: ",
             paste(basename(failed_files), collapse = ", "), call. = FALSE)
      }
      d_dirs <- matched_raw_files[file_info$isdir]
      for (dir in d_dirs) {
        copy_ok_d <- file.copy(from = dir, to = raw_data_dest, recursive = TRUE)
        if (!all(copy_ok_d)) {
          stop("msConvertR: Failed to copy .d directory '", basename(dir),
               "' for plate '", plateID,
               "'. Check disk space and permissions.", call. = FALSE)
        }
      }
      existing_mzml <- list.files(path = mzml_dest, pattern = "\\.mzML$",
                                  full.names = TRUE)
      existing_mzml <- existing_mzml[!is_qc_support_file(existing_mzml)]
      message("    [", plateID, "] ", length(existing_mzml),
              " mzML file(s) in: ", mzml_dest)
    }
    return(invisible(NULL))
  }
  for (saneID in unique(groups$sanitized_plateID)) {
    rows <- groups[groups$sanitized_plateID == saneID, , drop = FALSE]
    plate_data_dir <- file.path(output_directory, saneID, "data")
    raw_data_dest  <- file.path(plate_data_dir, "raw_data")
    mzml_dest      <- file.path(plate_data_dir, "mzml")
    dir.create(raw_data_dest, recursive = TRUE, showWarnings = FALSE)
    dir.create(mzml_dest, recursive = TRUE, showWarnings = FALSE)

    copied <- 0L
    for (j in seq_len(nrow(rows))) {
      src    <- rows$raw_path[j]
      is_dir <- isTRUE(rows$is_dir[j])

      # Include the .wiff.scan companion so a .wiff plate stays self-contained.
      to_copy <- src
      if (tolower(tools::file_ext(src)) == "wiff") {
        scan_file <- paste0(src, ".scan")
        if (file.exists(scan_file)) to_copy <- c(to_copy, scan_file)
      }

      for (item in to_copy) {
        dest_path <- file.path(raw_data_dest, basename(item))
        # Idempotent: skip items already relocated (e.g. on a re-run, or when
        # input_directory == output_directory).
        if (file.exists(dest_path) || dir.exists(dest_path)) next
        ok <- file.copy(from = item, to = raw_data_dest,
                        recursive = isTRUE(is_dir) && !grepl("\\.scan$", item))
        if (!all(ok)) {
          stop("msConvertR: Failed to copy '", basename(item),
               "' for plate '", saneID,
               "'. Check disk space and permissions.", call. = FALSE)
        }
        copied <- copied + 1L
      }
    }
    message("    [", saneID, "] Copied ", copied,
            " raw item(s) to: ", raw_data_dest)

    # Report mzML files produced by msconvert (written directly to mzml_dest).
    # Exclude ANPC conditioning/blank/ISTDs files using the shared helper so
    # the pattern is defined in one place (config.R) and is unit-testable.
    existing_mzml <- list.files(path = mzml_dest,
                                pattern = "\\.mzML$",
                                full.names = TRUE)
    existing_mzml <- existing_mzml[!is_qc_support_file(existing_mzml)]
    message("    [", saneID, "] ", length(existing_mzml),
            " mzML file(s) in: ", mzml_dest)
  }
}

