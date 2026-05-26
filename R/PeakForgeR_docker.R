# PeakForgeR_docker.R
# Docker command construction, execution, and container management
# for running Skyline via Docker.

# Skyline instrument-method m/z tolerance used when importing MRM transitions.
# 0.055 Da is intentionally wider than the mrm_guide matching tolerances
# (0.01 strict / 0.05 fallback) to give Skyline headroom for vendor-specific
# m/z centroiding offsets at the import stage; the tighter R-side tolerances
# still govern which guide entry is assigned to each chromatogram.
SKYLINE_INSTRUMENT_MZ_TOLERANCE <- 0.055

#Docker Command Functions----

#' execute_PeakForgeR_command
#'
#' This function executes a Skyline system command to process mzML files and generate various reports for a given plate.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to execute the Skyline command for.
#' @return Executes the Skyline command and generates reports and chromatogram files saving to project directory.
#' @examples
#' \dontrun{
#' execute_PeakForgeR_command(master_list, plate_idx)
#' }
execute_PeakForgeR_command <- function(master_list, plate_idx) {
  message("Building Skyline container command for plate: ", plate_idx)
  # Get absolute path to data directory; convert backslashes for Docker -v mount
  data_dir <- gsub("\\\\", "/", normalizePath(file.path(master_list$project_details$project_dir, plate_idx, "data")))

  # DOCK-C3: route the host path through a Sys.junction alias when it contains
  # spaces (OneDrive, "Program Files", etc.) so Docker's CLI parser doesn't
  # mis-split the -v value into "invalid reference format". The returned
  # junction is attached to the result vector via attr(); run_system_command
  # is responsible for cleanup after the container invocation completes.
  # mst_make_safe_mount_path() is a no-op on non-Windows so it is harmless
  # when enable_HPC = TRUE (Linux/HPC).
  data_mount <- mst_make_safe_mount_path(data_dir, prefix = "mst_pfr_")

  # Build filenames
  date_str <- Sys.Date()
  base_path <- file.path("PeakForgeR")

  in_file <- file.path(base_path, paste0(date_str, "_", plate_idx, ".sky"))
  import_transition_list <- file.path(base_path,
                                      paste0(date_str, "_RT_update_", plate_idx, ".csv"))
  mzml_path <- "mzml"
  import_peak_boundaries <- file.path(base_path,
                                      paste0(date_str, "_peak_boundary_update_", plate_idx, ".csv"))
  out_file <- file.path(base_path, paste0(date_str, "_", plate_idx, ".sky"))
  report_name <- "YYYY-MM-DD_PeakForgeR_project_name"
  report_file <- file.path(base_path,
                           paste0(date_str, "_PeakForgeR_", plate_idx, ".csv"))
  report_template <- file.path(base_path, "YYYY-MM-DD_PeakForgeR_project_name.skyr")
  chromatogram_file <- file.path(base_path,
                                 paste0(date_str, "_", plate_idx, "_chromatograms.tsv"))

  # SkylineCmd runs under Wine inside the image. image_command is the part
  # that goes *inside* the container (identical across Docker and Apptainer);
  # the runtime wrapper (run_container) is responsible for adding the
  # appropriate hardening flags and bind syntax.
  image_command <- c(
    "wine", "SkylineCmd",
    "--dir=/data",
    paste0("--in=", in_file),
    paste0("--instrument-method-mz-tolerance=", SKYLINE_INSTRUMENT_MZ_TOLERANCE),
    paste0("--import-transition-list=", import_transition_list),
    paste0("--import-all=", mzml_path),
    paste0("--import-peak-boundaries=", import_peak_boundaries),
    "--save-settings", "--overwrite",
    paste0("--out=", out_file),
    "--report-conflict-resolution=overwrite",
    paste0("--report-name=", report_name),
    paste0("--report-file=", report_file),
    paste0("--report-add=", report_template),
    "--report-format=csv",
    "--report-invariant",
    paste0("--chromatogram-file=", chromatogram_file),
    "--chromatogram-precursors",
    "--chromatogram-products",
    "--chromatogram-base-peaks",
    "--chromatogram-tics"
  )
  binds <- list(
    list(host = data_mount$safe_path, container = "/data", ro = FALSE)
  )

  # Compute the legacy Docker argv shape so tests that inspect the return
  # value as a character vector (and run_system_command's legacy single-arg
  # path) keep working unchanged. The runtime-aware dispatch in
  # run_system_command reads the mst_image_command / mst_binds attributes
  # below; the legacy argv body is only used as a fallback.
  docker_args <- c(
    "run", "--rm",
    "--cap-drop=ALL",
    "--network=none",
    "--security-opt", "seccomp=unconfined",
    "-v", paste0(data_mount$safe_path, ":/data"),
    mstargetr_image_ref(),
    image_command
  )

  message("Skyline container command constructed for plate: ", plate_idx,
          " with data directory: ", data_dir)

  # Stash the structured payload as attributes so run_system_command can
  # dispatch via run_container() (and therefore honour enable_HPC) without
  # parsing the docker argv back into pieces.
  attr(docker_args, "mst_image_command") <- image_command
  attr(docker_args, "mst_binds")         <- binds
  if (!is.null(data_mount$junction)) {
    attr(docker_args, "mst_junctions") <- data_mount$junction
  }
  docker_args
}


#' run_system_command
#'
#' This function wraps the container invocation that runs Skyline, allowing
#' for unit testing. It captures all output (stdout and stderr) and writes
#' it to a .txt file.
#' @keywords internal
#' @param PeakForgeR_command Docker argument vector returned by
#'   \code{execute_PeakForgeR_command()}. The vector carries the structured
#'   payload (\code{image_command}, \code{binds}) as attributes so the
#'   dispatcher can route the call to either Docker or Apptainer.
#' @param output_file optional path to save the command output
#' @param expected_output_files Optional character vector of host-side file paths that
#'   Skyline is expected to have produced (e.g. the report CSV and sky file).
#'   When supplied, the function checks that every path exists and has non-zero
#'   size after the command returns, providing a safety net for Skyline crash
#'   signatures that change across Skyline builds.
#' @param enable_HPC Logical. \code{FALSE} (default) -> Docker. \code{TRUE}
#'   -> Apptainer via the cached SIF.
#' @examples
#' \dontrun{
#' run_system_command(c("run", "--rm", "image", "wine", "SkylineCmd"), "output.txt")
#' }
run_system_command <- function(PeakForgeR_command, output_file,
                               expected_output_files = NULL,
                               enable_HPC = getOption("MStargetR.enable_HPC", FALSE)) {
  if (!is.character(PeakForgeR_command) || length(PeakForgeR_command) == 0) {
    stop("run_system_command: 'PeakForgeR_command' must be a non-empty character vector.",
         call. = FALSE)
  }

  # DOCK-C3: clean up any safe-mount junctions the command builder attached.
  # Done in on.exit so the junctions are released on success, error, or
  # interrupt. recursive = FALSE inside mst_cleanup_mount_junctions is
  # critical -- recursive unlink would traverse INTO the junction and
  # delete the user's real source files.
  on.exit(
    mst_cleanup_mount_junctions(attr(PeakForgeR_command, "mst_junctions")),
    add = TRUE
  )

  # Prefer the structured payload attached as attributes by
  # execute_PeakForgeR_command(); this lets run_container() honour
  # enable_HPC without re-parsing the docker argv. The legacy character-
  # vector single-string / docker-argv path remains for tests that stub
  # system2 directly.
  image_command <- attr(PeakForgeR_command, "mst_image_command")
  binds         <- attr(PeakForgeR_command, "mst_binds")
  use_dispatcher <- !is.null(image_command) && !is.null(binds)

  command_str <- paste("docker", paste(PeakForgeR_command, collapse = " "))
  preview_str <- if (nchar(command_str) > 120) {
    paste0(substr(command_str, 1, 120), "...")
  } else {
    command_str
  }
  message("Executing system command: ", preview_str)

  # Log the full command for reproducibility
  if (!is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    write(c(
      paste0("[", timestamp, "] --- Full Docker command ---"),
      command_str,
      ""
    ), file = output_file, append = TRUE)
  }

  # Require an argument vector. The single-string fallback is retained only for
  # backward compatibility but is deprecated: passing a length-1 string routes
  # through cmd.exe / sh which is shell-injection-prone if the string contains
  # user-influenced content.
  if (length(PeakForgeR_command) <= 1L) {
    warning("run_system_command: passing a single-string command is deprecated and ",
            "will be removed in a future version. Pass a character vector with ",
            "length > 1 (e.g. from execute_PeakForgeR_command()).",
            call. = FALSE, immediate. = TRUE)
  }

  result <- tryCatch({
    suppressWarnings(
      if (use_dispatcher) {
        # New runtime-aware path: dispatch via run_container() so enable_HPC
        # selects between Docker and Apptainer.
        run_container(image_command = image_command,
                      binds         = binds,
                      enable_HPC    = enable_HPC,
                      stdout        = TRUE,
                      stderr        = TRUE)
      } else if (length(PeakForgeR_command) > 1L) {
        # Legacy direct-docker path (used by tests that synthesise a raw
        # docker argv without the mst_* attributes).
        system2("docker", args = PeakForgeR_command, stdout = TRUE, stderr = TRUE)
      } else if (.Platform$OS.type == "windows") {
        system2("cmd", args = c("/c", PeakForgeR_command), stdout = TRUE, stderr = TRUE)
      } else {
        system2("sh", args = c("-c", PeakForgeR_command), stdout = TRUE, stderr = TRUE)
      }
    )
  }, error = function(e) {
    NULL
  })

  # Append Skyline output to log file
  if (!is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_lines <- c(
      paste0("[", timestamp, "] --- Skyline CMD output start ---"),
      if (!is.null(result)) result else "(no output captured)",
      paste0("[", timestamp, "] --- Skyline CMD output end ---"),
      ""
    )
    write(log_lines, file = output_file, append = TRUE)
  }

  if (is.null(result)) {
    stop("Skyline command failed to execute. Check Docker is running and accessible.\n",
         "Full command logged to: ", output_file,
         call. = FALSE)
  }

  exit_code <- attr(result, "status")
  output_text <- paste(result, collapse = "\n")

  # Check for Skyline-specific crash signatures (stack traces from pwiz.Skyline).
  # Pattern is pure ASCII, so useBytes = TRUE bypasses wide-string conversion
  # and avoids Windows non-UTF-8 locale warnings ("unable to translate" /
  # "input string ... is invalid") when Skyline output contains chemical
  # names with non-ASCII bytes (e.g. greek delta in "d18:1-D7"). It also
  # ensures grepl returns logical (not NA) for those lines, so a real crash
  # banner sitting on a non-translatable line is not silently missed.
  has_skyline_crash <- any(grepl(
    "pwiz\\.Skyline\\.Program\\.(ReportExceptionUI|Main)|System\\.AggregateException",
    result, perl = TRUE, useBytes = TRUE
  ))

  if (!is.null(exit_code) && exit_code != 0) {
    stop("Skyline exited with error (status ", exit_code, "):\n", output_text,
         "\n\nFull command and output logged to: ", output_file,
         call. = FALSE)
  } else if (has_skyline_crash) {
    stop("Skyline crashed during execution:\n", output_text,
         "\n\nFull command and output logged to: ", output_file,
         call. = FALSE)
  } else {
    message("System command completed successfully. Output saved to: ", output_file)
  }

  # Post-run output file check: verify expected outputs exist and are non-empty.
  # This supplements the crash-string heuristic for Skyline builds that alter
  # their crash banner format.
  if (!is.null(expected_output_files)) {
    missing_or_empty <- expected_output_files[
      !file.exists(expected_output_files) |
        (file.exists(expected_output_files) & file.info(expected_output_files)$size == 0)
    ]
    if (length(missing_or_empty) > 0) {
      stop("Skyline completed but expected output file(s) are missing or empty:\n",
           paste("  -", missing_or_empty, collapse = "\n"),
           "\n\nFull command and output logged to: ", output_file,
           call. = FALSE)
    }
  }

  return(result)
}
