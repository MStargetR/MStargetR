#' @keywords internal
#' @name import_external_functions
#' @importFrom stringr str_remove str_extract
NULL

#' Convert Vendor Mass Spectrometry Files to mzML Format
#'
#' Converts raw vendor mass spectrometry files (e.g. \code{.wiff}, \code{.raw},
#' \code{.d}) to open \code{.mzML} format using ProteoWizard's \code{msconvert}
#' tool running inside a Docker container. The function validates inputs,
#' manages Docker execution, and organises the resulting files into a
#' standardised project directory structure.
#'
#' @param input_directory A character string specifying the path to the
#'   directory containing vendor files to convert.
#' @param output_directory A character string specifying the path to the
#'   directory where the converted \code{.mzML} files and project structure
#'   will be created.
#' @param enable_HPC Logical. When \code{TRUE}, the ProteoWizard container is
#'   invoked via Apptainer (Singularity) instead of Docker. This is the
#'   intended runtime on HPC clusters where Docker is typically forbidden.
#'   The default is \code{getOption("MStargetR.enable_HPC", FALSE)} so HPC
#'   users can set \code{options(MStargetR.enable_HPC = TRUE)} once in their
#'   \code{.Rprofile} and never pass the argument explicitly. See the
#'   "Running on HPC" section of the README for SIF setup instructions.
#' @param ... Reserved for forward compatibility. Any unrecognised named
#'   arguments trigger a warning and are otherwise ignored.
#' @return Called for its side effects. The function creates a project
#'   directory structure containing converted \code{.mzML} files organised
#'   by plate. Invisibly returns \code{NULL}.
#' @export
#' @examples
#' \dontrun{
#' # Default (Docker) on a workstation
#' msConvertR(input_directory  = "path/to/input_directory",
#'            output_directory = "path/to/output_directory")
#'
#' # HPC (Apptainer): pre-pull the SIF on a login node, then run a job:
#' #   apptainer pull mstargetr-pwiz.sif \
#' #     docker://proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses:<tag>
#' options(
#'   MStargetR.enable_HPC = TRUE,
#'   MStargetR.sif_path   = "/scratch/me/mstargetr-pwiz.sif"
#' )
#' msConvertR(input_directory  = "/scratch/me/MyProject/raw_data",
#'            output_directory = "/scratch/me/MyProject")
#'
#' # Or enable HPC mode for a single call without setting the option:
#' msConvertR(input_directory  = "/scratch/me/MyProject/raw_data",
#'            output_directory = "/scratch/me/MyProject",
#'            enable_HPC       = TRUE)
#'  }
#'
#' @details
#' \itemize{
#'  \item \strong{Input Validation:}
#'   \itemize{
#'    \item Validate input_directory
#'    \item Validate presence of supported vendor file types
#'   }
#'  \item \strong{Plate Identification:}
#'   \itemize{
#'    \item Extract plateIDs from vendor file names
#'    \item Remove vendor-specific extensions
#'   }
#'  \item \strong{Docker Setup:}
#'   \itemize{
#'    \item Check Docker installation and running status
#'   }
#'  \item \strong{File Conversion:}
#'   \itemize{
#'    \item Convert vendor files to mzML format using ProteoWizard's msconvert
#'    \item Handle errors gracefully with tryCatch
#'   }
#'  \item \strong{Directory Structuring:}
#'   \itemize{
#'    \item Create project structure for converted files
#'    \item Relocate vendor files based on input/output directory configuration
#'   }
#'  \item \strong{User Messaging:}
#'   \itemize{
#'    \item Notify user of conversion status and file locations
#'    \item Provide guidance on directory structure
#'   }
#' }


msConvertR <- function (input_directory, output_directory,
                        enable_HPC = getOption("MStargetR.enable_HPC", FALSE),
                        ...) {
  # Warn callers who supply unexpected named arguments (forward-compat guard).
  dots <- list(...)
  if (length(dots) > 0) {
    warning("msConvertR: unknown argument(s) ignored: ",
            paste(names(dots), collapse = ", "), call. = FALSE)
  }

  if (!is.logical(enable_HPC) || length(enable_HPC) != 1L || is.na(enable_HPC)) {
    stop("msConvertR: 'enable_HPC' must be a single logical (TRUE or FALSE).",
         call. = FALSE)
  }

  # Validate input_directory
  validate_input_directory(input_directory)

  # Validate output_directory
  if (!is.character(output_directory) || length(output_directory) != 1) {
    stop("msConvertR: 'output_directory' must be a single character string. Got: ",
         paste(class(output_directory), collapse = ", "), call. = FALSE)
  }
  if (nchar(output_directory) == 0) {
    stop("msConvertR: 'output_directory' must not be an empty string.", call. = FALSE)
  }

  # Ensure output_directory exists and is writable before any work begins.
  dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)
  if (!dir.exists(output_directory)) {
    stop("msConvertR: 'output_directory' could not be created: '",
         output_directory, "'.", call. = FALSE)
  }
  if (file.access(output_directory, mode = 2) != 0) {
    stop("msConvertR: 'output_directory' is not writable: '",
         output_directory, "'.", call. = FALSE)
  }

  # Validate wiff files
  file_paths <- validate_file_types(input_directory)

  # Check if wiff files are found
  if (length(file_paths) == 0) {
    stop(
      "No supported files found in the specified input directory for processing. ",
      "Checked: '", input_directory, "'. ",
      "Please ensure the directory contains vendor files (.wiff, .raw, .d, etc.) and try again.",
      call. = FALSE
    )
  }

  # Vendor file extensions — single authoritative pattern from config.R
  vendor_extension_patterns <- MSTARGETR_VENDOR_EXT_PATTERN

  # Set plateIDs
  plateIDs <- str_remove(basename(file_paths), vendor_extension_patterns)

  # Deduplicate plateIDs to prevent running conversion multiple times for the

  # same plate when multiple vendor files resolve to the same name
  if (anyDuplicated(plateIDs)) {
    dupes <- plateIDs[duplicated(plateIDs)]
    message("msConvertR: Removing ", length(dupes),
            " duplicate plateID(s): ", paste(unique(dupes), collapse = ", "))
    plateIDs <- unique(plateIDs)
  }

  # Keep original (filename-derived) IDs for disk lookups; produce a parallel
  # vector of sanitized IDs used only in output paths / directory names.
  raw_plateIDs <- plateIDs
  sanitized_plateIDs <- vapply(plateIDs, function(pid) {
    sanitize_identifier(pid, context = "plateID")
  }, character(1), USE.NAMES = FALSE)

  # Deduplication must also be checked AFTER sanitization because two distinct
  # raw names can collapse to the same sanitized name (e.g. "plate/A" and
  # "plate_A" both become "plate_A"), which would cause log-file collisions.
  if (anyDuplicated(sanitized_plateIDs)) {
    dup_sane <- sanitized_plateIDs[duplicated(sanitized_plateIDs)]
    dup_raw  <- raw_plateIDs[sanitized_plateIDs %in% dup_sane]
    stop(
      "msConvertR: the following vendor file names produce the same sanitized plateID ",
      "after sanitization, which would cause output collisions. ",
      "Rename the source files to make them unique: ",
      paste(dup_raw, collapse = ", "),
      call. = FALSE
    )
  }

  # Check runtime availability: Docker for the default path, Apptainer for
  # enable_HPC = TRUE. resolve_sif() is called inside msConvertR_execute_command()
  # so the (potentially slow) one-time pull happens once on the main session
  # rather than racing across workers.
  if (isTRUE(enable_HPC)) {
    assert_runtime_available("apptainer")
  } else {
    check_docker()
  }

  # Process vendor files
  tryCatch({
    msConvertR_mzml_conversion(input_directory,
                               output_directory,
                               raw_plateIDs,
                               vendor_extension_patterns,
                               sanitized_plateIDs,
                               enable_HPC = enable_HPC)

  }, error = function(e) {
    stop("msConvertR conversion failed: ", conditionMessage(e), call. = FALSE)
  })

  # Notify user about converted files
  message(
    "\nConverted mzML files are located in ",
    file.path(output_directory, "plate_id", "data", "mzml")
  )

  # Directory structure messages
  # Use forward-slash normalisation so different drive-letter representations
  # of the same physical directory compare equal on Windows.
  if (normalizePath(input_directory,  mustWork = FALSE, winslash = "/") ==
      normalizePath(output_directory, mustWork = FALSE, winslash = "/")) {
    message(
      "\nNote: Input and output directories are the same.\n",
      "Vendor files have been relocated to ",
      file.path(output_directory, "plateID", "data", "raw_data")
    )
  } else {
    message(
      "\nNote: Input and output directories are different.\n",
      "The project structure has been created in ",
      output_directory,
      ".\n",
      "Vendor files are located in ",
      file.path(input_directory, "plateID", "data", "raw_data")
    )
  }

  message("Thank you for using msConvertR")

}
