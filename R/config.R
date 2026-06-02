# config.R
#
# This file consolidates several related but distinct concerns that are each
# too small to justify a standalone file:
#
#   * Docker image configuration  (see also: check_docker)
#   * Path helpers                (mstargetr_with_dir, sanitize_identifier,
#                                  validate_project_directory)
#   * Script-log state machine    (update_script_log and helpers)
#   * MRM template validators     (validate_mrm_template_list,
#                                  validate_qcCheckR_mrm_template_list,
#                                  default_mrm_templates)
#   * Docker availability check   (check_docker)
#   * Raw-file type validation    (validate_file_types)
#   * Symbol replacement          (replace_precursor_symbols)
#
# Each section is clearly delimited below.

# == Package-level constants ==================================================

#' Directory names excluded when discovering plate IDs from a project directory
#'
#' These names are ignored during automatic plate detection so that
#' well-known non-plate sub-folders are never treated as plate IDs.
#' @keywords internal
MSTARGETR_EXCLUDE_DIRS <- c(
  "raw_data", "msConvert_mzml_output", "all", "archive",
  "error_log.txt", "MStargetR_logs", "logs", "user_files"
)

#' Default RSD threshold used in export and summary functions
#'
#' Features whose QC RSD (%) meets or exceeds this value are excluded from the
#' pre-processed concentration outputs. Both \code{filter_concentration()} and
#' the display columns in \code{generate_plate_summary()} reference this
#' constant so the two remain in sync.
#' @keywords internal
DEFAULT_RSD_THRESHOLD <- 30L

#' Regex pattern matching all supported vendor-file extensions
#'
#' Used by \code{msConvertR()} to strip extensions when deriving plate IDs from
#' file names, and by \code{msConvertR_restructure_directory()} when scanning
#' the raw-data folder.  Having a single definition prevents the two call-sites
#' from drifting out of sync.
#' @keywords internal
MSTARGETR_VENDOR_EXT_PATTERN <- paste0(
  "\\.(d|baf|fid|yep|tsf|tdf|mbi|wiff|wiff\\.scan|scan|wiff2|",
  "qgd|qgb|qgm|lcd|lcdproj|raw|uep|sdf|dat|wcf|wproj|wdata)$"
)

#' Regex that identifies ANPC QC / support files to exclude from mzML counts
#'
#' Matches ANPC conditioning runs, blanks, and ISTDs files so they are not
#' counted as converted sample mzML files.  Used by
#' \code{msConvertR_restructure_directory()} and testable in isolation via the
#' \code{is_qc_support_file()} helper.
#' @keywords internal
MSTARGETR_QC_SUPPORT_PATTERN <- "-COND\\d+[._]|-BLANK\\d+[._]|-BLANK_\\d+|-ISTDs_\\d+"

#' Test whether a filename belongs to an ANPC QC / support file
#'
#' Wraps \code{MSTARGETR_QC_SUPPORT_PATTERN} so the predicate can be exercised
#' in isolation without constructing full paths.
#' @param fname Character vector of file names (basename or full path).
#' @return Logical vector; \code{TRUE} for each element that matches.
#' @keywords internal
is_qc_support_file <- function(fname) {
  grepl(MSTARGETR_QC_SUPPORT_PATTERN, fname, ignore.case = TRUE)
}

#' Columns selected from a non-unique transition table for the clash report
#'
#' Shared by \code{transition_checkR()} so that the set of reported columns is
#' defined in one place and can be referenced by tests without duplicating the
#' list.
#' @keywords internal
MSTARGETR_TRANSITION_SUMMARY_COLS <- c(
  "Molecule List Name", "Precursor Name", "Precursor Mz",
  "Precursor Charge", "Product Mz", "Product Charge"
)

# == Docker image configuration ===============================================

# Docker image configuration.
# Pinned to a specific build so all users of a given MStargetR release pull
# the same ProteoWizard/Skyline image. Bumping this tag is a breaking change
# (re-download required); record the bump in NEWS.md. Upstream deletion of
# the tag would make `check_docker()` fail loudly on pull, not silently.
MSTARGETR_DOCKER_IMAGE_TAG <- local({
  env_val <- Sys.getenv("MSTARGETR_DOCKER_IMAGE", unset = NA_character_)
  opt_val <- getOption("MStargetR.docker.image_tag", default = NULL)
  if (!is.null(opt_val) && nzchar(opt_val)) {
    opt_val
  } else if (!is.na(env_val) && nzchar(env_val)) {
    env_val
  } else {
    "skyline_26.1.0.057-c07debd"
  }
})

#' Evaluate an expression with a temporarily changed working directory
#'
#' Internal helper that changes the working directory, evaluates \code{expr},
#' and restores the original directory on exit. Equivalent to
#' \code{withr::with_dir} but implemented without a \code{withr} dependency at
#' this specific call site so it can be used safely during package
#' initialisation before \pkg{withr} is attached.
#'
#' Note on lazy-eval semantics: \code{expr} is a promise that is
#' \code{force()}-d *after* the directory has been changed (and the
#' \code{on.exit} restore has been registered). The caller's working directory
#' at call-time is therefore the directory in which \code{expr} is evaluated —
#' this is intentional and matches the behaviour of \code{withr::with_dir}.
#' Guarantees the original working directory is restored even if \code{expr}
#' errors or the user interrupts.
#' @keywords internal
#' @param new_dir Target working directory.
#' @param expr Expression to evaluate.
#' @return The value of \code{expr}.
mstargetr_with_dir <- function(new_dir, expr) {
  if (!is.character(new_dir) || length(new_dir) != 1 || !nzchar(new_dir)) {
    stop("mstargetr_with_dir: 'new_dir' must be a non-empty single string.",
         call. = FALSE)
  }
  if (!dir.exists(new_dir)) {
    stop("mstargetr_with_dir: directory does not exist: ", new_dir,
         call. = FALSE)
  }
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(new_dir)
  force(expr)
}

#' @keywords internal
MSTARGETR_SIL_REQUIRED_COLUMNS <- c(
  "Molecule List Name",
  "Precursor Name",
  "Precursor Mz",
  "Precursor Charge",
  "Product Mz",
  "Product Charge",
  "Explicit Retention Time",
  "Explicit Retention Time Window",
  "Note",
  "control_chart"
)

#' Sanitize an identifier for safe use in file paths
#'
#' Removes or replaces characters that could cause path traversal (`..`,
#' `/`, `\\`). Allows alphanumeric, underscore, hyphen, dot, space,
#' parentheses, and at-sign only.
#'
#' Note: this helper guards filesystem callers. External command safety
#' comes from passing arguments as a vector to \code{system2()} (which
#' never invokes a shell), not from this sanitizer — spaces and
#' parentheses are deliberately preserved here so human-readable
#' identifiers round-trip through filenames.
#'
#' @details
#' The exact allowed character set after sanitization is:
#' \code{[A-Za-z0-9_.()@ -]} (alphanumeric, underscore, hyphen, dot,
#' parentheses, at-sign, and ASCII space). Any character outside this set
#' is replaced with an underscore. Path traversal sequences (\code{..})
#' and path separators (\code{/}, \code{\\}) are rejected outright rather
#' than silently stripped. Callers that pass the result to a Windows
#' filesystem API should be aware that Windows silently strips trailing
#' spaces and that parentheses may require quoting in some shell contexts;
#' always use \code{system2(args = vector)} rather than string
#' interpolation when passing sanitized values to external commands.
#' @keywords internal
#' @param x Character string to sanitize.
#' @param context Description of what is being sanitized (for error messages).
#' @return Sanitized character string.
sanitize_identifier <- function(x, context = "identifier") {
  if (!is.character(x) || length(x) != 1 || !nzchar(x)) {
    stop(sprintf("sanitize_identifier: '%s' must be a non-empty string.", context),
         call. = FALSE)
  }
  # Reject path traversal sequences
  if (grepl("\\.\\.", x) || grepl("[/\\\\]", x)) {
    stop(sprintf("sanitize_identifier: '%s' contains invalid path characters: '%s'",
                 context, x), call. = FALSE)
  }
  # Strip any remaining unsafe characters (keep alphanumeric, _, -, ., space, parens, @)
  sanitized <- gsub("[^A-Za-z0-9_.()@ -]", "_", x)
  if (!nzchar(sanitized)) {
    stop(sprintf("sanitize_identifier: '%s' is empty after sanitization.", context),
         call. = FALSE)
  }
  sanitized
}

# == Script-log state machine =================================================

# Primary function
#' Update Script Log
#'
#' This function updates the script log in the `master_list` object by capturing the current time, calculating the runtime for the current section, and creating a message for the log.
#' @keywords internal
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#' @param previous_section_name A string representing the name of the previous section.
#' @param next_section_name A string representing the name of the next section.
#' @return The updated `master_list` object with the new log information.
#' @examples
#' \dontrun{
#' # Build a minimal master_list with start_time already recorded
#' master_list <- list(
#'   project_details = list(
#'     script_log = list(
#'       timestamps = list(start_time = Sys.time()),
#'       runtimes   = list(),
#'       messages   = list()
#'     )
#'   )
#' )
#' # Record the end of "section_1" and prepare the log entry for "section_2"
#' master_list <- update_script_log(master_list,
#'                                   section_name          = "section_1",
#'                                   previous_section_name = "start_time",
#'                                   next_section_name     = "section_2")
#' }
update_script_log <- function(master_list,
                              section_name,
                              previous_section_name,
                              next_section_name) {
  # Input validation
  if (!is.list(master_list)) {
    stop("update_script_log: 'master_list' must be a list. Got: ",
         paste(class(master_list), collapse = ", "), call. = FALSE)
  }
  if (is.null(master_list$project_details$script_log)) {
    stop("update_script_log: 'master_list' must contain ",
         "$project_details$script_log structure.", call. = FALSE)
  }
  if (!is.character(section_name) || length(section_name) != 1) {
    stop("update_script_log: 'section_name' must be a single character string. Got: ",
         paste(class(section_name), collapse = ", "), call. = FALSE)
  }
  if (!is.character(previous_section_name) || length(previous_section_name) != 1) {
    stop("update_script_log: 'previous_section_name' must be a single character string. Got: ",
         paste(class(previous_section_name), collapse = ", "), call. = FALSE)
  }
  if (!is.character(next_section_name) || length(next_section_name) != 1) {
    stop("update_script_log: 'next_section_name' must be a single character string. Got: ",
         paste(class(next_section_name), collapse = ", "), call. = FALSE)
  }

  validate_previous_section(master_list, previous_section_name)
  master_list <- capture_current_time(master_list, section_name)
  master_list <- calculate_runtime(master_list, section_name, previous_section_name)
  master_list <- calculate_total_runtime(master_list, section_name)
  master_list <- create_message(master_list, section_name, next_section_name)
  master_list <- print_message(master_list, section_name)
  return(master_list)
}

# Secondary functions
#' Validate Previous Section
#'
#' This function validates the previous section name.
#' @keywords internal
#' @param master_list A list containing project details and script log information.
#' @param previous_section_name A string representing the name of the previous section.
#'
#' @return Called for its side effect; returns \code{NULL} invisibly.

validate_previous_section <- function(master_list, previous_section_name) {
  timestamps <- master_list$project_details$script_log$timestamps
  if (!is.list(timestamps)) {
    stop("validate_previous_section: ",
         "'master_list$project_details$script_log$timestamps' must be a list. Got: ",
         paste(class(timestamps), collapse = ", "), call. = FALSE)
  }
  if (!previous_section_name %in% names(timestamps)) {
    stop("validate_previous_section: 'previous_section_name' ('",
         previous_section_name, "') not found in script_log timestamps. ",
         "Available sections: ",
         paste(names(timestamps), collapse = ", "),
         call. = FALSE)
  }
}

#' Capture Current Time
#'
#' This function captures the current time for the given section.
#' @keywords internal
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#' @param overwrite Logical. If \code{FALSE} (the default) a warning is emitted
#'   when a timestamp already exists for \code{section_name}. Set to
#'   \code{TRUE} to overwrite silently.
#'
#' @return updated master list
capture_current_time <- function(master_list, section_name, overwrite = FALSE) {
  if (!isTRUE(overwrite) &&
      !is.null(master_list$project_details$script_log$timestamps[[section_name]])) {
    warning("capture_current_time: timestamp for section '", section_name,
            "' already exists and will be overwritten. ",
            "Pass overwrite = TRUE to suppress this warning.",
            call. = FALSE)
  }
  master_list$project_details$script_log$timestamps[[section_name]] <- Sys.time()
  return(master_list)
}

#' Calculate Runtime
#'
#' This function calculates the runtime for the given section.
#' @keywords internal
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#' @param previous_section_name A string representing the name of the previous section.
#'
#' @return The updated `master_list` with the calculated runtime for the section.

calculate_runtime <- function(master_list,
                              section_name,
                              previous_section_name) {
  if (is.null(master_list$project_details$script_log$timestamps[[section_name]])) {
    stop("calculate_runtime: Timestamp for 'section_name' ('", section_name,
         "') not found in script_log.", call. = FALSE)
  }
  if (is.null(master_list$project_details$script_log$timestamps[[previous_section_name]])) {
    stop("calculate_runtime: Timestamp for 'previous_section_name' ('",
         previous_section_name, "') not found in script_log.", call. = FALSE)
  }
  master_list$project_details$script_log$runtimes[[section_name]] <- difftime(
    master_list$project_details$script_log$timestamps[[section_name]],
    master_list$project_details$script_log$timestamps[[previous_section_name]],
    units = "mins"
  )
  return(master_list)
}

#' Calculate Total Runtime
#'
#' This function calculates the total runtime from the start.
#' @keywords internal
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#'
#' @return The updated `master_list` with the total runtime calculated from start.
calculate_total_runtime <- function(master_list, section_name) {
  if (is.null(master_list$project_details$script_log$timestamps$start_time)) {
    stop("calculate_total_runtime: 'start_time' timestamp is missing from ",
         "master_list$project_details$script_log$timestamps. ",
         "Seed it with capture_current_time(master_list, \"start_time\") ",
         "before calling update_script_log().", call. = FALSE)
  }
  master_list$project_details$script_log$runtimes$total_runtime <- difftime(
    master_list$project_details$script_log$timestamps[[section_name]],
    master_list$project_details$script_log$timestamps$start_time,
    units = "mins"
  )
  return(master_list)
}

#' Create Message
#'
#' This function creates a message for the log.
#' @keywords internal
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#' @param next_section_name A string representing the name of the next section.
#'
#' @return The updated `master_list` with the log message for the section.

create_message <- function(master_list,
                           section_name,
                           next_section_name) {
  master_list$project_details$script_log$messages[[section_name]] <- paste0(
    "\n",
    toupper(gsub("_", " ", section_name)),
    " complete!",
    "\n\n Section runtime: ",
    signif(
      as.numeric(master_list$project_details$script_log$runtimes[[section_name]]),
      digits = 3
    ),
    " minutes",
    "\n\n Total runtime: ",
    signif(
      as.numeric(
        master_list$project_details$script_log$runtimes$total_runtime
      ),
      digits = 3
    ),
    " minutes",
    "\n",
    "\nInitialising: ",
    toupper(gsub("_", " ", next_section_name)),
    "...."
  )
  return(master_list)
}

#' Print Message
#'
#' This function prints the message for the log.
#' @keywords internal
#' @param master_list A list containing project details and script log information.
#' @param section_name A string representing the name of the current section.
#'
#' @return The updated `master_list` (unchanged, returned for piping).

print_message <- function(master_list, section_name) {
  msg <- master_list$project_details$script_log$messages[[section_name]]
  if (is.null(msg)) {
    stop("print_message: No message found for section '", section_name,
         "'. Available sections: ",
         paste(names(master_list$project_details$script_log$messages), collapse = ", "),
         call. = FALSE)
  }
  message(msg)
  return(master_list)
}



# == Parameter validators ====================================================
#' Validate Project Directory
#'
#' This function checks if the `project_directory` parameter is a single string and if the specified directory exists.
#' @keywords internal
#' @param project_directory A character string representing the path to the project directory.
#' @param verbose Logical. If \code{TRUE} (the default) a message is emitted
#'   with the resolved path. Set to \code{FALSE} to suppress the message when
#'   calling the validator in a loop or from another validator.
#' @return The normalized project directory path (invisibly), or throws an error if validation fails.
#' @examples
#' \dontrun{
#' validate_project_directory("path/to/project_directory")
#' }
validate_project_directory <- function(project_directory, verbose = TRUE) {
  # Check if project_directory is a single string
  if (!is.character(project_directory) ||
      length(project_directory) != 1) {
    stop("validate_project_directory: 'project_directory' must be a single character string. Got: ",
         paste(class(project_directory), collapse = ", "),
         " of length ", length(project_directory), ".", call. = FALSE)
  }

  if (nchar(project_directory) == 0) {
    stop("validate_project_directory: 'project_directory' must not be an empty string.",
         call. = FALSE)
  }

  # Check if the specified directory exists
  if (!dir.exists(project_directory)) {
    stop("validate_project_directory: The specified project directory does not exist: '",
         project_directory, "'.", call. = FALSE)
  }

  # Normalize path to prevent traversal via symlinks or relative paths
  project_directory <- normalizePath(project_directory, winslash = "/", mustWork = TRUE)

  # Return the normalized path so callers use a consistent canonical form
  if (isTRUE(verbose)) {
    message("Accessing project directory ", project_directory)
  }
  return(invisible(project_directory))
}

#' validate_master_list_project_directory
#' This function validates the existence of the project directory specified in the master list.
#' @keywords internal
#' @param master_list A list containing project details.
#' @return Stops execution if the project directory does not exist.
#' @examples
#' \dontrun{
#' validate_master_list_project_directory(master_list)
#' }
validate_master_list_project_directory <- function(master_list) {
  if (is.null(master_list$project_details$project_dir)) {
    stop("Project directory is not set in master_list.", call. = FALSE)
  }
  if (!dir.exists(master_list$project_details$project_dir)) {
    stop(
      paste(
        "Project directory does not exist:",
        master_list$project_details$project_dir
      ),
      call. = FALSE
    )
  }
}




#' Default ANPC MRM Templates
#'
#' Returns the default ANPC MRM template list (four versioned file paths
#' shipped with the package). Callers that need the ANPC default list should
#' call this function directly rather than relying on the side-effect branch
#' in \code{validate_mrm_template_list()}.
#' @keywords internal
#' @return A named list of file paths (v1–v4).
default_mrm_templates <- function() {
  list(
    v1 = system.file("extdata", "LGW_lipid_mrm_template_v1.tsv", package = "MStargetR"),
    v2 = system.file("extdata", "LGW_lipid_mrm_template_v2.tsv", package = "MStargetR"),
    v3 = system.file("extdata", "LGW_lipid_mrm_template_v3.tsv", package = "MStargetR"),
    v4 = system.file("extdata", "LGW_lipid_mrm_template_v4.tsv", package = "MStargetR")
  )
}

#' Validate MRM Template List
#'
#' This function checks if the `mrm_template_list` parameter is valid and
#' contains required columns. For ANPC users who pass \code{NULL}, call
#' \code{default_mrm_templates()} first to obtain the default list and then
#' pass it here for validation.
#' @keywords internal
#' @param mrm_template_list A named list of file paths (character) or data
#'   frames representing MRM templates.
#' @param user_name A character string identifying the user.
#' @return \code{invisible(TRUE)} on success; stops with an error on failure.
#'   For ANPC users with \code{NULL} templates the function returns the
#'   default template list (for backward compatibility with existing callers).
#' @examples
#' \dontrun{
#' validate_mrm_template_list(list("path/to/template1.csv", "path/to/template2.csv"), "user")
#' }
validate_mrm_template_list <- function(mrm_template_list, user_name) {
  required_columns <- MSTARGETR_SIL_REQUIRED_COLUMNS

  if (user_name == "ANPC" && is.null(mrm_template_list)) {
    # ANPC default templates. v3 added so the "_MS-LIPIDS-3" plate suffix
    # resolves to the correctly matched template (see REVIEW_REPORT BC-H6).
    mrm_template_list <- default_mrm_templates()
    message("mrm_template validation complete")
    return(mrm_template_list)
  }

  if (is.null(mrm_template_list)) {
    stop("Please provide a valid mrm_template_list.", call. = FALSE)
  }

  if (!is.list(mrm_template_list)) {
    stop("mrm_template_list must be a list.", call. = FALSE)
  }

  if (is.null(names(mrm_template_list)) || any(!nzchar(names(mrm_template_list)))) {
    stop("mrm_template_list must be a named list (e.g. list(v1 = df1, v2 = df2)). ",
         "Found unnamed or partially named entries.", call. = FALSE)
  }

  for (version in names(mrm_template_list)) {
    version_entry <- mrm_template_list[[version]]

    if (is.character(version_entry) && length(version_entry) == 1) {
      # Entry is a file path — validate that the file exists and has a valid extension
      if (!file.exists(version_entry)) {
        stop(
          paste("mrm_template_list file does not exist for", version, ":",
                version_entry),
          call. = FALSE
        )
      }
      if (!grepl("\\.(csv|tsv)$", version_entry, ignore.case = TRUE)) {
        stop(
          paste("mrm_template_list file for", version,
                "must be a .csv or .tsv file. Got:", version_entry),
          call. = FALSE
        )
      }
    } else if (is.data.frame(version_entry)) {
      # Entry is a data frame — validate required columns
      missing_columns <- setdiff(required_columns, colnames(version_entry))
      if (length(missing_columns) > 0) {
        stop(paste(
          "Missing required columns in version",
          version,
          ":\n",
          paste(missing_columns, collapse = "\n")
        ),
        call. = FALSE)
      }
    } else {
      stop(
        paste(
          "Each version in mrm_template_list must be a file path (character) or",
          "a data frame. Problem with:",
          version
        ),
        call. = FALSE
      )
    }
  }
  message("mrm_template validation complete")
  return(mrm_template_list)
}

#' Log Error to File
#'
#' This function logs error messages to a file named `error_log.txt`.
#' @keywords internal
#' @param error_message A character string representing the error message to be logged.
#' @param plateID A character string identifying the plate.
#' @param project_directory A character string for the project directory path. Defaults to \code{getwd()}.
#' @return None. The function writes the error message to the log file.
#' @examples
#' \dontrun{
#' log_error("An error occurred while processing the data.")
#' }
log_error <- function(error_message, plateID, project_directory = getwd()) {
  if (!is.character(error_message) || length(error_message) != 1) {
    stop("log_error: 'error_message' must be a single character string. Got: ",
         paste(class(error_message), collapse = ", "), call. = FALSE)
  }
  if (!is.character(plateID) || length(plateID) != 1) {
    stop("log_error: 'plateID' must be a single character string. Got: ",
         paste(class(plateID), collapse = ", "), call. = FALSE)
  }
  if (!is.character(project_directory) || length(project_directory) != 1) {
    stop("log_error: 'project_directory' must be a single character string. Got: ",
         paste(class(project_directory), collapse = ", "), call. = FALSE)
  }
  if (!dir.exists(project_directory)) {
    stop("log_error: 'project_directory' does not exist: '",
         project_directory, "'.", call. = FALSE)
  }
  log_dir <- file.path(project_directory, "MStargetR_logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  log_file <- file.path(log_dir, paste0(plateID, "_MStargetR_log.txt"))
  write(error_message, file = log_file, append = TRUE)
}


#' Validate qcCheckR mrm template list
#'
#' This function validates the mrm_template_list list by checking the column headers and ensuring there are no NA or NULL values in the SIL_guide and conc_guide files.
#' @keywords internal
#' @param master_list A list containing all project details and data
#' @return TRUE if validation passes. Stops execution if validation fails.
#' @examples
#' \dontrun{
#' validate_qcCheckR_mrm_template_list(master_list)
#' }
validate_qcCheckR_mrm_template_list <- function(master_list) {
  mrm_template_list <- master_list$templates$mrm_guides

  if (!is.list(mrm_template_list)) {
    stop("mrm_template_list must be a list.", call. = FALSE)
  }

  for (version in names(mrm_template_list)) {
    version_list <- mrm_template_list[[version]]

    # Check that version is a list
    if (!is.list(version_list)) {
      stop(paste(
        "Each version in mrm_template_list must be a list. Problem with:",
        version
      ), call. = FALSE)
    }

    for (guide in c("SIL_guide", "conc_guide")) {
      if (!guide %in% names(version_list)) {
        stop(paste("Missing", guide, "in version", version), call. = FALSE)
      }

      # Check required columns
      if (guide == "SIL_guide") {
        required_columns <- MSTARGETR_SIL_REQUIRED_COLUMNS

        data <- mrm_template_list[[version]][[guide]]

        if (!all(required_columns %in% colnames(data))) {
          #store missing columns
          missing_columns <- setdiff(required_columns, colnames(data))
          stop(paste(
            guide,
            "for version",
            version,
            "\n Missing required columns: ",
            paste(missing_columns, collapse = "\n")
          ), call. = FALSE)
        }


        # Define the columns you want to check
        check_cols <- c(
          "Molecule List Name",
          "Precursor Name",
          "Precursor Mz",
          "Precursor Charge",
          "Product Mz",
          "Product Charge",
          "Explicit Retention Time",
          "Explicit Retention Time Window",
          "control_chart"
        )

        # Subset the data to only include the filtered check columns
        data_to_check <- data[, check_cols, drop = FALSE]

        # Check for NA or NULL values
        if (any(is.na(data_to_check))) {
          stop(paste("NA values found in", guide, "for version", version),
               call. = FALSE)
        }

        transition_result <- transition_checkR(data)
        if (is.data.frame(transition_result)) {
          stop(paste("Non-unique transitions found in version", version, "\n",
                     paste(capture.output(print(transition_result)), collapse = "\n")),
               call. = FALSE)
        }

      } else if (guide == "conc_guide") {
        required_columns <- c("concentration_factor", "SIL_name")

        data <- mrm_template_list[[version]][[guide]]

        if (!all(required_columns %in% colnames(data))) {
          missing_columns <- setdiff(required_columns, colnames(data))
          stop(paste(
            guide,"for version",version,
            "\n Missing required columns: ",
            paste(missing_columns, collapse = "\n")
          ), call. = FALSE)
        }
        sil_guide <- version_list[["SIL_guide"]]
        compare_result <- compare_mrm_template_with_guide(sil_guide, data)
        if (is.character(compare_result)) {
          stop(paste("Unmatched Note values in version", version, "\n",
                     paste(compare_result, collapse = "\n")),
               call. = FALSE)
        }
      }
    }
  }

  message("Validation passed: mrm_template_list structure and contents are valid.")
  return(TRUE)
}

# == Docker availability check ===============================================
#'
#' Function to check Docker installation, daemon, and containers
#' @keywords internal
#' @return Returns current status of docker
#' @examples
#' \dontrun{
#' check_docker()
#' }
# Function to check Docker installation, daemon status, and container execution
check_docker <- function(auto_pull = interactive()) {
  # Check if Docker is installed using system2 (safer than system).
  # A 10-second timeout prevents indefinite blocking on a hung socket.
  docker_installed <- tryCatch({
    result <- system2("docker", "--version",
                      stdout = TRUE, stderr = TRUE, timeout = 10)
    length(result) > 0 && any(grepl("Docker", result, ignore.case = TRUE))
  }, error = function(e) {
    FALSE
  })

  if (!docker_installed) {
    stop(
      "Docker is not installed. Please install Docker Desktop from:\n",
      "  https://www.docker.com/products/docker-desktop/\n",
      "Then restart R and try again.",
      call. = FALSE
    )
  }

  # Check daemon is running via 'docker info' (lightweight, no image pull).
  # Exit status is the authoritative signal; it is locale-independent.
  # A timeout prevents indefinite blocking when the daemon socket is hung.
  daemon_running <- tryCatch({
    info <- suppressWarnings(
      system2("docker", "info", stdout = TRUE, stderr = TRUE, timeout = 10)
    )
    exit_status <- attr(info, "status")
    is.null(exit_status) || exit_status == 0L
  }, error = function(e) {
    FALSE
  })

  if (!daemon_running) {
    stop(
      "Docker is installed but the daemon is not running.\n",
      "Please start the Docker Desktop application and try again.",
      call. = FALSE
    )
  }

  # Check if the image exists locally before pulling
  image_name <- paste0(
    "proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses:",
    MSTARGETR_DOCKER_IMAGE_TAG
  )
  image_exists <- tryCatch({
    images <- system2("docker", c("images", "-q", image_name),
                      stdout = TRUE, stderr = TRUE, timeout = 10)
    length(images) > 0 && nzchar(images[1])
  }, error = function(e) FALSE)

  if (!image_exists) {
    if (!auto_pull) {
      message(
        "ProteoWizard Docker image not found locally: ", image_name, "\n",
        "Run check_docker(auto_pull = TRUE) to download it (~GB), ",
        "or pull it manually with:\n",
        "  docker pull ", image_name
      )
      return(invisible(NULL))
    }
    message("Pulling ProteoWizard Docker image (first run only)...")
    pull_result <- system2("docker", c("pull", image_name),
                           stdout = TRUE, stderr = TRUE)
    pull_exit <- attr(pull_result, "status")
    if (!is.null(pull_exit) && pull_exit != 0L) {
      stop("Failed to pull Docker image: ", image_name, "\n",
           "Check your internet connection and Docker configuration.",
           call. = FALSE)
    }
    message("Successfully pulled ProteoWizard Docker image.")
  }
}

# == Raw-file type validation ================================================
#' Validate Raw files
#'
#' This function checks project directories contains vendor files.
#' @keywords internal
#' @param input_directory directory path for vendor file locations
#' @return validated paths and returns message on outcome of check.
#' @examples
#' \dontrun{
#' all_file_paths <- validate_file_types(input_directory)
#' }
validate_file_types <- function(input_directory) {
  raw_root <- file.path(input_directory, "raw_data")

  # Supported single-file extensions (data-driven lookup)
  supported_exts <- c("raw", "baf", "fid", "yep", "tsf", "tdf", "mbi",
                       "qgd", "qgb", "qgm", "lcd", "lcdproj", "uep",
                       "sdf", "dat", "wcf", "wproj", "wdata")

  # Validate the vendor files within a single directory, scoping the
  # .wiff/.wiff.scan companion pairing (and orphan-scan detection) to that
  # directory so that per-plate subfolders are each validated independently.
  # Plain sub-directories (plate containers) are left for the caller to recurse
  # into; only .d directories are accepted as vendor data here.
  validate_one_dir <- function(dir) {
    entries <- list.files(path = dir, full.names = TRUE)
    valid <- character(0)
    invalid <- character(0)
    for (file in entries) {
      # Skip .wiff.scan companion files — validated alongside their .wiff
      if (grepl("\\.wiff\\.scan$", file)) next
      ext <- tolower(tools::file_ext(file))
      if (ext == "wiff") {
        if (file.exists(paste0(file, ".scan"))) {
          valid <- c(valid, file)
        } else {
          message("Missing .wiff.scan for: ", basename(file))
          invalid <- c(invalid, file)
        }
      } else if (ext == "d" && dir.exists(file)) {
        valid <- c(valid, file)
      } else if (!dir.exists(file) && ext %in% supported_exts) {
        valid <- c(valid, file)
      } else if (dir.exists(file)) {
        # Plate-container subdirectory: recursed into by the caller, not here.
        next
      } else {
        message("Unsupported file type found: ", basename(file))
        invalid <- c(invalid, file)
      }
    }

    # Detect orphan .wiff.scan files (within this dir) whose .wiff is absent
    for (ws in entries[grepl("\\.wiff\\.scan$", entries)]) {
      base_wiff <- sub("\\.scan$", "", ws)
      if (!base_wiff %in% valid) {
        message("Orphan .wiff.scan (no matching valid .wiff): ", basename(ws))
        invalid <- c(invalid, ws)
      }
    }
    list(valid = valid, invalid = invalid)
  }

  top_entries <- list.files(path = raw_root, full.names = TRUE)
  # A directory is a plate container (recurse into it) unless its name matches a
  # vendor extension (e.g. "Sample.d" is vendor data, not a plate folder).
  is_plate_subdir <- vapply(top_entries, function(p) {
    dir.exists(p) &&
      !grepl(MSTARGETR_VENDOR_EXT_PATTERN, basename(p), ignore.case = TRUE)
  }, logical(1))

  # Validate top-level files (+ .d dirs), then one level into each plate folder.
  res <- validate_one_dir(raw_root)
  validated_files <- res$valid
  invalid_files <- res$invalid
  for (sub in top_entries[is_plate_subdir]) {
    sub_res <- validate_one_dir(sub)
    if (length(sub_res$valid) == 0) {
      message("No valid vendor files in plate folder: ", basename(sub))
    }
    validated_files <- c(validated_files, sub_res$valid)
    invalid_files <- c(invalid_files, sub_res$invalid)
  }

  file_path <- raw_root  # preserve variable name used in messages below

  if (length(validated_files) == 0) {
    stop("validate_file_types: No supported vendor files found in '",
         file_path, "'. Supported formats: .wiff, .raw, .d, .baf, .fid, .yep, ",
         ".tsf, .tdf, .mbi, .qgd, .qgb, .qgm, .lcd, .lcdproj, .uep, ",
         ".sdf, .dat, .wcf, .wproj, .wdata",
         call. = FALSE)
  }

  if (length(invalid_files) > 0) {
    message(
      "Removed following unsupported files:\n",
      paste(invalid_files, collapse = "\n")
    )
  }

  message("Returning validated files for processing:\n",
          paste(basename(validated_files), collapse = "\n"))
  return(validated_files)
}

# == Plate manifest ==========================================================
#' Read and validate a plate-grouping manifest
#'
#' Parses an optional user-supplied manifest that maps each raw vendor file to a
#' plate. This lets labs whose plate membership lives in an instrument worklist,
#' LIMS export, or filename convention express that mapping explicitly rather
#' than relying on subfolder layout. Accepts a CSV path or a pre-read
#' \code{data.frame} with (case-insensitive) columns \code{raw_file} and
#' \code{plateID} (a \code{sample_name} column, if present, is currently
#' ignored and reserved for future renaming support).
#'
#' @keywords internal
#' @param manifest Path to a CSV file, or a \code{data.frame}.
#' @param known_files Character vector of validated vendor file basenames; every
#'   \code{raw_file} in the manifest must be present here.
#' @return \code{data.frame} with columns \code{raw_file} (basename) and
#'   \code{plateID}.
read_plate_manifest <- function(manifest, known_files) {
  if (is.data.frame(manifest)) {
    df <- manifest
  } else if (is.character(manifest) && length(manifest) == 1L &&
             file.exists(manifest)) {
    # fileEncoding = "UTF-8-BOM" strips an Excel-written BOM if present and is a
    # no-op otherwise, avoiding a mangled first column name on Windows.
    df <- utils::read.csv(manifest, stringsAsFactors = FALSE,
                          check.names = FALSE, fileEncoding = "UTF-8-BOM")
  } else {
    stop("msConvertR: 'manifest' must be a path to an existing CSV file or a ",
         "data.frame. Got: '",
         if (is.character(manifest)) manifest else class(manifest)[1], "'.",
         call. = FALSE)
  }

  orig_names <- names(df)
  names(df) <- tolower(trimws(orig_names))
  if ("plate_id" %in% names(df) && !"plateid" %in% names(df)) {
    names(df)[names(df) == "plate_id"] <- "plateid"
  }
  if (!all(c("raw_file", "plateid") %in% names(df))) {
    stop("msConvertR: manifest must contain columns 'raw_file' and 'plateID'. ",
         "Found: ", paste(orig_names, collapse = ", "), ".", call. = FALSE)
  }

  raw_file <- basename(trimws(as.character(df$raw_file)))
  plateID  <- trimws(as.character(df$plateid))
  if (any(!nzchar(raw_file)) || any(!nzchar(plateID))) {
    stop("msConvertR: manifest contains empty 'raw_file' or 'plateID' values.",
         call. = FALSE)
  }
  if (anyDuplicated(raw_file)) {
    dup <- unique(raw_file[duplicated(raw_file)])
    stop("msConvertR: manifest lists duplicate 'raw_file' entries: ",
         paste(dup, collapse = ", "), ".", call. = FALSE)
  }
  missing_files <- setdiff(raw_file, known_files)
  if (length(missing_files) > 0) {
    stop("msConvertR: manifest references files not found in raw_data/: ",
         paste(missing_files, collapse = ", "), ".", call. = FALSE)
  }

  data.frame(raw_file = raw_file, plateID = plateID, stringsAsFactors = FALSE)
}

# == Symbol replacement for MRM templates ====================================
#' replace_precursor_symbols
#'
#' This function replaces forward or backwards slashes in 'Precursor Name' while preserving the original naming convention
#' This is due to skyline cmd being unable to handle the special character
#' @keywords internal
#' @param mrm_template dataframe of transitions (mrm_template) for SkylineR or qcCheckR
#' @return Updated mrm_template with special characters replaced in 'Precursor Name' and 'Note', while the original names are preserved for the columns in original_col
#' @section Scope of sanitisation:
#'   This function only replaces \code{/} and \code{\\} with underscores to
#'   satisfy Skyline's path parser. It does \emph{not} provide general shell
#'   safety. If any returned value is later interpolated into a shell command,
#'   callers must use \code{system2(args = vector)} (never string
#'   interpolation) or pass the value through \code{sanitize_identifier()}
#'   first.
#' @examples
#' \dontrun{
#' replace_precursor_symbols(mrm_template, columns = c("Precursor Name", "Note"))
#' }
replace_precursor_symbols <- function(mrm_template, columns = c("Precursor Name", "Note")) {
  if (!is.data.frame(mrm_template)) {
    stop("replace_precursor_symbols: 'mrm_template' must be a data.frame. Got: ",
         paste(class(mrm_template), collapse = ", "), call. = FALSE)
  }
  if (!is.character(columns) || length(columns) == 0) {
    stop("replace_precursor_symbols: 'columns' must be a non-empty character vector. Got: ",
         paste(class(columns), collapse = ", "), call. = FALSE)
  }
  missing_cols <- setdiff(columns, colnames(mrm_template))
  if (length(missing_cols) > 0) {
    stop("replace_precursor_symbols: 'mrm_template' is missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  for (col in columns) {
    original_col <- paste0("original_", gsub(" ", "_", col))
    mrm_template[[original_col]] <- mrm_template[[col]]

    # Replace / and \ with underscores
    mrm_template[[col]] <- gsub("[/\\\\]", "_", mrm_template[[col]])
  }
  return(mrm_template)
}

# == Test helpers ============================================================

check_dir_exists <- function(path)
  dir.exists(path)
create_dir <- function(path)
  dir.create(path, recursive = TRUE)

# == Windows MAX_PATH junction helper =========================================

#' Evaluate a function with a Windows junction to shorten a long path
#'
#' On Windows, when \code{long_path} exceeds 260 characters, a directory
#' junction is created at a temporary location and \code{fn(junction_path)}
#' is called instead. The junction is removed on exit (success or error).
#' On non-Windows platforms, or when the path is short enough,
#' \code{fn(long_path)} is called directly.
#'
#' Sys.junction() is preferred over \code{shell(mklink /J)} to avoid
#' shell-quoting pitfalls. The shell fallback is retained for environments
#' where Sys.junction is unavailable.
#'
#' @param long_path The full, potentially long directory path.
#' @param fn A function that accepts a single path argument and returns a value.
#' @param pattern Prefix passed to \code{tempfile()} for the junction name.
#' @return The value returned by \code{fn}.
#' @keywords internal
with_short_junction <- function(long_path, fn, pattern = "PeakForgeR_short_") {
  if (nchar(long_path) > 260 && .Platform$OS.type == "windows") {
    short_junction <- tempfile(pattern = pattern)
    message("Path exceeds 260 chars; creating junction: ",
            short_junction, " -> ", long_path)
    ok <- tryCatch({
      Sys.junction(long_path, short_junction)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(ok)) {
      cmd <- sprintf('cmd /c mklink /J "%s" "%s"', short_junction, long_path)
      shell(cmd, intern = FALSE)
    }
    on.exit(unlink(short_junction, recursive = TRUE), add = TRUE)
    fn(short_junction)
  } else {
    fn(long_path)
  }
}

#' Make a path safe for use as a Docker bind-mount source on Windows.
#'
#' Docker on Windows fails with \code{"invalid reference format"} when the
#' host side of a \code{-v} mount contains spaces, because the
#' \code{cmd.exe -> docker.exe} command-line handoff loses the quotes that
#' \code{system2() / shQuote()} adds. This affects common locations like
#' OneDrive ("OneDrive - Org Name") and "Program Files".
#'
#' On Windows, when \code{host_path} contains a space, this helper creates
#' an NTFS junction under a no-spaces temp directory pointing at the
#' original location and returns the junction path. The caller is
#' responsible for unlinking the junction (with \code{recursive = FALSE}
#' so only the reparse point is removed, not the target's contents).
#'
#' On non-Windows platforms, or when \code{host_path} has no spaces, the
#' original path is returned unchanged and \code{junction} is \code{NULL},
#' so callers can pass the result through unconditionally.
#'
#' @param host_path Real host filesystem path to be bind-mounted.
#' @param prefix Prefix used for the junction name (passed to
#'   \code{tempfile()}).
#' @return A list with two elements:
#' \itemize{
#'   \item \code{safe_path}: the path to use in the docker \code{-v} arg
#'     (forward-slash-normalised).
#'   \item \code{junction}: the junction path that needs to be unlinked
#'     after use, or \code{NULL} if no junction was created.
#' }
#' @keywords internal
mst_make_safe_mount_path <- function(host_path, prefix = "mst_mnt_") {
  if (.Platform$OS.type != "windows" || !grepl(" ", host_path)) {
    return(list(safe_path = host_path, junction = NULL))
  }
  if (!dir.exists(host_path)) {
    stop("mst_make_safe_mount_path: target does not exist: '",
         host_path, "'", call. = FALSE)
  }
  # Pick a tempdir that itself contains no spaces; otherwise the junction
  # path inherits the problem we are trying to escape.
  tmp_root <- Sys.getenv("TEMP", unset = "")
  if (!nzchar(tmp_root) || grepl(" ", tmp_root) || !dir.exists(tmp_root)) {
    tmp_root <- if (dir.exists("C:/Windows/Temp")) "C:/Windows/Temp" else tempdir()
  }
  if (grepl(" ", tmp_root)) {
    stop("mst_make_safe_mount_path: every candidate tempdir contains spaces; ",
         "set TEMP to a no-spaces path (e.g. C:\\Temp).", call. = FALSE)
  }
  junction <- tempfile(pattern = prefix, tmpdir = tmp_root)
  ok <- tryCatch({
    if (exists("Sys.junction", mode = "function", envir = baseenv())) {
      # Unqualified call: Sys.junction is Windows-only and not exported
      # on Linux/macOS, so `base::Sys.junction` would trigger an R CMD
      # check WARNING ("Missing or unexported object") on cross-platform
      # CI. The exists() guard above plus the `.Platform$OS.type` check
      # in callers ensure we only reach this branch on Windows.
      Sys.junction(host_path, junction)
    } else {
      status <- shell(sprintf('cmd /c mklink /J "%s" "%s"',
                              junction, host_path), intern = FALSE)
      identical(status, 0L) || dir.exists(junction)
    }
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok) && !dir.exists(junction)) {
    stop("mst_make_safe_mount_path: failed to create junction at '",
         junction, "' -> '", host_path, "'", call. = FALSE)
  }
  list(safe_path = gsub("\\\\", "/", junction), junction = junction)
}

#' Remove junctions created by \code{mst_make_safe_mount_path()}.
#'
#' \code{recursive = FALSE} is critical: \code{unlink(j, recursive = TRUE)}
#' on a Windows NTFS junction will recurse INTO the target and delete the
#' user's real files, which is exactly the disaster the safe-mount path
#' is meant to avoid. \code{RemoveDirectory()} (what \code{unlink()} ends
#' up calling on a junction with \code{recursive = FALSE}) only removes
#' the reparse point.
#'
#' @param junctions Character vector of junction paths (any length, may
#'   include \code{NULL}-equivalents).
#' @return Invisibly, the input.
#' @keywords internal
mst_cleanup_mount_junctions <- function(junctions) {
  for (j in junctions) {
    if (length(j) == 0L) next
    if (!is.character(j) || !nzchar(j)) next
    if (!dir.exists(j)) next
    tryCatch(unlink(j, recursive = FALSE, force = TRUE),
             error = function(e) NULL, warning = function(w) NULL)
  }
  invisible(junctions)
}
