#' @keywords internal
#' @name import_external_functions
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
#' @param manifest Optional. Either a path to a CSV file or a
#'   \code{data.frame} mapping each raw vendor file to a plate, with
#'   (case-insensitive) columns \code{raw_file} and \code{plateID}. Use this
#'   last-resort override when plate membership for one-file-per-sample formats
#'   (e.g. \code{.d}, \code{.raw}) cannot be recovered automatically. When
#'   \code{NULL} (default), plate membership is resolved automatically in
#'   priority order: a remembered \code{plate_grouping.csv} at the project root;
#'   per-plate subfolders under \code{raw_data/} (a file in
#'   \code{raw_data/<plateID>/} belongs to that plate); filename-based
#'   auto-discovery, which infers the plate from the filename token structure
#'   for sample-level files left flat in \code{raw_data/} and reports the
#'   inference (persisting it to an editable \code{plate_grouping.csv} for
#'   confirmation); and finally the bare filename. Sample-level files that even
#'   auto-discovery cannot group are converted with a warning (each becomes its
#'   own plate) rather than rejected. A multi-sample \code{.wiff} is always one
#'   plate.
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
                        manifest = NULL,
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

  # Resolve plate membership from (priority order) the manifest / remembered
  # plate_grouping.csv, per-plate subfolders under raw_data/, filename-based
  # auto-discovery, or the flat filename. validate_file_types() is called
  # inside and errors if no supported vendor files are present.
  groups <- derive_plate_groups(input_directory, manifest = manifest)

  # Report-and-proceed: any sample-level vendor files that survive as "flat"
  # could not be grouped by a manifest, a subfolder, or filename auto-discovery
  # (no shared filename structure at all). Per the grouping policy we proceed
  # rather than stop - but warn loudly, because each becomes its own plate,
  # which weakens per-plate QC and batch correction. Multi-sample .wiff/.wiff2
  # files are exempt: each is legitimately its own plate.
  ungrouped <- groups$source == "flat" & !groups$plate_level
  if (sum(ungrouped) >= 2L) {
    warning(
      "msConvertR: ", sum(ungrouped), " single-sample vendor file(s) could not ",
      "be grouped into plates from their filenames:\n  ",
      paste(groups$file_name[ungrouped], collapse = "\n  "),
      "\n\nEach will be treated as its own plate. To group them, either:\n",
      "  (a) place each plate's files in a 'raw_data/<plateID>/' subfolder,\n",
      "  (b) pass manifest = a CSV with columns 'raw_file,plateID', or\n",
      "  (c) edit the generated 'plate_grouping.csv' in the project directory.\n",
      "(Multi-sample .wiff files are exempt - each is treated as its own plate.)",
      call. = FALSE
    )
  }

  # Post-sanitization collision guard: two DISTINCT plate IDs must not collapse
  # to the same sanitized directory name (e.g. "plate/A" and "plate_A" both
  # sanitize to "plate_A"), which would merge unrelated plates / clobber logs.
  plate_map <- unique(groups[, c("raw_plateID", "sanitized_plateID")])
  if (anyDuplicated(plate_map$sanitized_plateID)) {
    dup_sane <- plate_map$sanitized_plateID[duplicated(plate_map$sanitized_plateID)]
    dup_raw  <- plate_map$raw_plateID[plate_map$sanitized_plateID %in% dup_sane]
    stop(
      "msConvertR: the following plate IDs collapse to the same sanitized name ",
      "after sanitization, which would merge unrelated plates. ",
      "Rename the plate folder(s) / manifest entries to make them unique: ",
      paste(dup_raw, collapse = ", "),
      call. = FALSE
    )
  }

  # Warn if a derived plateID collides with a reserved directory name, since
  # downstream plate discovery (PeakForgeR) excludes those names and would
  # silently skip the plate.
  reserved <- unique(plate_map$sanitized_plateID[
    plate_map$sanitized_plateID %in% MSTARGETR_EXCLUDE_DIRS])
  if (length(reserved) > 0) {
    warning(
      "msConvertR: plate ID(s) collide with reserved directory names and will be ",
      "skipped by downstream plate discovery: ", paste(reserved, collapse = ", "),
      ". Rename the plate folder(s) / manifest entries.", call. = FALSE
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
                               groups,
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
