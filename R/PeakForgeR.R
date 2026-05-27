#' Peak Picking and Integration via Skyline in Docker
#'
#' @description This function performs peak picking and integration via Skyline
#' in a Docker image. Allowing for usage across all major OS systems.
#'
#' We strongly recommend checking your mrm transition list
#' using MStargetR::transition_checkR prior to using it in PeakForgeR
#'
#' If the user has not used MStargetR::msConvertR to convert vendor files
#' please ensure each plate folder exists under the project directory with
#' mzML files located at \code{<project_directory>/<plateID>/data/mzml/*.mzML}.
#' The \code{plateID_outputs} parameter must be supplied to identify the plate
#' folders.
#' @param user_name A character string to identify user.
#' @param project_directory A path to project directory
#' @param mrm_template_list Path to MRM transition list,
#' must be in specified format. See examples and run load example
#' mrm_guide for structure. May contain more than one template for
#' multi-method projects.
#' @param QC_sample_label User specified tag to filter QC samples.
#' Character case is not sensitive
#'
#' E.g. "JANE_C5_URI_MS-LIPIDS_PLIP01_PLATE_3-PLASMA LTR_19.mzML"
#'
#' QC_sample_label = "LTR" to target files containing LTR for QC.
#' @param plateID_outputs A vector of character strings specifying plateIDs for
#' project. This parameter must only be specified by users who have not used
#' MStargetR::msConvertR..... Default is NULL
#'
#' These must match mzml files.
#' e.g. If you have two plates:
#'  - JANE_DOE_C5_URI_MS-LIPIDS_PLATE_1-PLASMA_sample_1.mzML
#'  - JANE_DOE_C5_URI_MS-LIPIDS_PLATE_2-PLASMA_sample_1.mzML
#'
#' An appropriate input would be:
#'  - plateID_outputs = c("JANE_DOE_C5_URI_MS-LIPIDS_PLATE_1",
#'                        "JANE_DOE_C5_URI_MS-LIPIDS_PLATE_2")
#' @param enable_HPC Logical. When \code{TRUE}, Skyline is run via Apptainer
#'   (Singularity) instead of Docker. This is the intended runtime on HPC
#'   clusters where Docker is typically forbidden. The default is
#'   \code{getOption("MStargetR.enable_HPC", FALSE)} so HPC users can set
#'   \code{options(MStargetR.enable_HPC = TRUE)} once in their \code{.Rprofile}
#'   and never pass the argument explicitly. See the "Running on HPC" section
#'   of the README for SIF setup instructions.
#' @return A curated project directory with sub folders for each plate containing Skyline exports.
#' @export
#' @examples
#' \dontrun{
#' #Load example mrm_guide
#'   file_path <- system.file("extdata", "LGW_lipid_mrm_template_v1.tsv", package = "MStargetR")
#'   example_mrm_template <- readr::read_tsv(file_path)
#'
#' #Default (Docker) on a workstation
#' PeakForgeR(user_name = "Mad_max",
#'            project_directory = "USER/PATH/TO/PROJECT/DIRECTORY",
#'            mrm_template_list = list("User/path/to/user_mrm_guide_v1.tsv",
#'                                     "user/path/to/user_mrm_guide_v2.tsv"),
#'            QC_sample_label = "LTR",
#'            plateID_outputs = NULL
#'           )
#'
#' # HPC (Apptainer): pre-pull the SIF on a login node, then run a job:
#' #   apptainer pull mstargetr-pwiz.sif \
#' #     docker://proteowizard/pwiz-skyline-i-agree-to-the-vendor-licenses:<tag>
#' options(
#'   MStargetR.enable_HPC = TRUE,
#'   MStargetR.sif_path   = "/scratch/me/mstargetr-pwiz.sif"
#' )
#' PeakForgeR(user_name         = "Mad_max",
#'            project_directory = "/scratch/me/MyProject",
#'            mrm_template_list = list("/scratch/me/templates/lipid_mrm_v1.tsv"),
#'            QC_sample_label   = "LTR")
#'}
#'
#' @details
#' \itemize{
#'  \item \strong{Input Validation:}
#'   \itemize{
#'    \item Validate project_directory
#'    \item Validate mrm_template_list
#'   }
#'  \item \strong{File Handling:}
#'   \itemize{
#'    \item Set plateIDs from either plate MStargetR::msConvertR or
#'          user specified plateID_outputs
#'   }
#'  \item \strong{Processing Plates:}
#'   \itemize{
#'    \item For each plateID:
#'     \itemize{
#'      \item Setup project structure
#'      \item Import mzml files
#'      \item QC optimised retention times
#'      \item QC optimised peak boundaries
#'      \item Peak picking/integration with Skyline MS through docker
#'     }
#'   }
#'  \item \strong{Final Cleanup:}
#'   \itemize{
#'    \item Archive raw files
#'    \item Message about availability of chromatograms and reports
#'   }
#' }
#'
#' @importFrom future plan sequential multisession availableCores
#' @importFrom future.apply future_lapply
PeakForgeR <- function(user_name,
                     project_directory,
                     mrm_template_list = NULL,
                     QC_sample_label = NULL,
                     plateID_outputs = NULL,
                     enable_HPC = getOption("MStargetR.enable_HPC", FALSE)) {
  #Validate user
  if (!is.character(user_name) || length(user_name) != 1 || nchar(user_name) == 0) {
    stop("PeakForgeR: 'user_name' must be a non-empty single character string. Got: ",
         paste(class(user_name), collapse = ", "), call. = FALSE)
  }
  if (!is.logical(enable_HPC) || length(enable_HPC) != 1L || is.na(enable_HPC)) {
    stop("PeakForgeR: 'enable_HPC' must be a single logical (TRUE or FALSE).",
         call. = FALSE)
  }
  sanitized_user_name <- sanitize_identifier(user_name, context = "user_name")
  if (sanitized_user_name != user_name) {
    message("PeakForgeR: 'user_name' was sanitized from '", user_name,
            "' to '", sanitized_user_name, "' for safe use in filenames.")
  }
  user_name <- sanitized_user_name

  # Validate project_directory and use the normalized path
  project_directory <- validate_project_directory(project_directory)

  # Scope the wd change: restore on.exit so an early error / interrupt
  # cannot leave the user in the project dir. This is the manual
  # equivalent of withr::with_dir (withr is Suggests-only; see
  # REVIEW_REPORT BC-H10).
  old_wd <- getwd()
  setwd(project_directory)
  on.exit(setwd(old_wd), add = TRUE)

  # Validate mrm_template_list
  validated_list <-  validate_mrm_template_list(mrm_template_list, user_name)
  ## If validate_mrm_template_list returned something, use it
  if (!is.null(validated_list)) {
    mrm_template_list <- validated_list
  }

  # Validate QC_sample_label
  if (is.null(QC_sample_label) || !is.character(QC_sample_label) ||
      length(QC_sample_label) != 1 || nchar(QC_sample_label) == 0) {
    stop("PeakForgeR: 'QC_sample_label' must be a non-empty single character string. Got: ",
         if (is.null(QC_sample_label)) "NULL" else paste(class(QC_sample_label), collapse = ", "),
         call. = FALSE)
  }

  # Validate plateID_outputs (optional)
  if (!is.null(plateID_outputs)) {
    if (!is.character(plateID_outputs) || length(plateID_outputs) == 0) {
      stop("PeakForgeR: 'plateID_outputs' must be a character vector of length >= 1 when provided. Got: ",
           paste(class(plateID_outputs), collapse = ", "), " of length ", length(plateID_outputs),
           call. = FALSE)
    }
    if (any(!nzchar(plateID_outputs))) {
      stop("PeakForgeR: 'plateID_outputs' must not contain empty strings.",
           call. = FALSE)
    }
  }

  # Check runtime availability: Docker for the default path, Apptainer for
  # enable_HPC = TRUE. The (potentially slow) SIF pull is deferred until the
  # first run_container() call inside the per-plate future.
  if (isTRUE(enable_HPC)) {
    assert_runtime_available("apptainer")
    # Force the SIF resolve once on the main session so the pull (if any) is
    # not raced by the per-plate futures.
    resolve_sif()
  } else {
    check_docker()
  }

  # Set plateIDs — only consider directories (not stray files)
  all_entries <- list.files(project_directory)
  candidate_entries <- all_entries[!all_entries %in% MSTARGETR_EXCLUDE_DIRS]
  # Filter to only directories to avoid treating stray files as plate IDs.
  # Directory names are kept as-is so that file.path lookups remain correct;
  # sanitize_identifier is applied only to user-supplied identifiers embedded
  # in output filenames, not to on-disk directory names.
  plateIDs <- candidate_entries[dir.exists(file.path(project_directory, candidate_entries))]


  if (is.null(plateIDs) || length(plateIDs) == 0) {
    if (is.null(plateID_outputs) || length(plateID_outputs) == 0) {
      stop("PeakForgeR: No plate directories found in '", project_directory,
           "' and no 'plateID_outputs' provided. ",
           "Please run msConvertR first or specify plateID_outputs.",
           call. = FALSE)
    }
    count_data <- list()
    for (plate in plateID_outputs) {
      mzml_dir <- file.path(project_directory, plate, "data", "mzml")
      if (!dir.exists(mzml_dir)) {
        stop("PeakForgeR: mzML directory not found for plate '", plate,
             "': ", mzml_dir, call. = FALSE)
      }
      mzml_files <- list.files(mzml_dir, pattern = "\\.mzML$", ignore.case = TRUE)
      count_data[[plate]] <- length(mzml_files)
    }

    zero_plates <- names(count_data)[unlist(count_data) == 0]
    if (length(zero_plates) > 0) {
      stop("PeakForgeR: Zero mzML files for plateID(s): ",
           paste(zero_plates, collapse = ", "),
           ". Please check your input for the 'plateID_outputs' parameter.",
           call. = FALSE)
    }

    message("Valid plateID_outputs provided:")
    for (p in names(count_data)) {
      message("  ", p, ": ", count_data[[p]], " mzML file(s)")
    }
    plateIDs <- plateID_outputs
  } else {
    message(
      "plateIDs gathered from existing project directory\n",
      "Plates for processing:\n ",
      paste(plateIDs, collapse = "\n")
    )
  }

  #Set failed/successful plates
  failed_plates <- c()
  successful_plates <- c()

  # Process each plate in parallel
  logs_dir <- file.path(project_directory, "MStargetR_logs")
  if (file.exists(logs_dir) && !dir.exists(logs_dir)) {
    stop("PeakForgeR: '", logs_dir, "' exists but is not a directory. ",
         "Please remove or rename this file before proceeding.",
         call. = FALSE)
  }
  dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)

  #Parallel settings
  total_cores <- future::availableCores()
  available_cores <- if (total_cores <= 2) 1 else total_cores - 2
  future::plan(future::multisession, workers = available_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  # future.seed = TRUE forces L'Ecuyer-CMRG streams per worker so any RNG
  # consumed downstream (tempfile fallbacks, samplers in dependencies) is
  # parallel-safe. Without it, future warns "UNRELIABLE VALUE" and results
  # may not be reproducible.
  results <- future.apply::future_lapply(plateIDs, function(plateID) {
    start_time <- Sys.time()
    log_file <- file.path(logs_dir, paste0(plateID, "_MStargetR_log.txt"))
    # Helper to write a line to the log
    write_log <- function(text) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      line <- paste0("[", timestamp, "] ", text, "\n")
      write(enc2utf8(line), file = log_file, append = TRUE)
    }
    #Helper to write error to log using same timestamped format as write_log
    log_error <- function(error_message, plateID, project_directory = getwd()) {
      write_log(paste("ERROR:", error_message))
    }

    tryCatch({
      write_log(paste("Starting processing for plate:", plateID))

      write_log("Step 1: Setting up project...")
      master_list <- PeakForgeR_setup_project(
        user_name,
        project_directory,
        plateID,
        mrm_template_list,
        QC_sample_label
      )
      write_log("Project setup complete.")

      write_log("Step 2: Importing mzML files...")
      master_list <- import_mzml(plateID, master_list)
      write_log("mzML import complete.")

      write_log("Step 3: Performing peak picking...")
      master_list <- peak_picking(plateID, master_list,
                                  enable_HPC = enable_HPC)
      write_log("Peak picking complete.")

      write_log(paste("Finished processing plate:", plateID))
      write_log("Status: SUCCESS")

      list(success = TRUE, plateID = plateID)

    }, error = function(e) {
      write_log(paste("Error during processing:", e$message))
      write_log("Status: FAILURE")

      log_error(paste("Error processing plate", plateID, ":", e$message), plateID, project_directory)
      list(success = FALSE, plateID = plateID, error = e$message)
    })
  }, future.seed = TRUE)

  # Extract successful and failed plate IDs
  successful_plates <- vapply(results, function(x) isTRUE(x$success) && !is.null(x$plateID), logical(1))
  failed_plates <- vapply(results, function(x) !isTRUE(x$success) && !is.null(x$plateID), logical(1))

  # Display summary
  message("\nProcessing complete.")

  if (any(successful_plates)) {
    message("\nPlates processed successfully:\n",
            paste(sapply(results[successful_plates], `[[`, "plateID"), collapse = "\n"))
  }

  if (any(failed_plates)) {
    message("\nPlates that FAILED to process:")
    for (r in results[failed_plates]) {
      message("  ", r$plateID, ": ", r$error)
    }
  }

  if (!any(successful_plates)) {
    stop("PeakForgeR: All plates failed to process. Check MStargetR_logs for details.",
         call. = FALSE)
  }

  # Final cleanup and archiving
  message("Starting archive of raw files...")
  tryCatch(
    {
      archive_raw_files(project_directory)
      message("Archive of raw files complete.")
    },
    error = function(e) {
      warning("Archiving failed: ", e$message,
              "\nPlate results above are still valid.",
              call. = FALSE, immediate. = TRUE)
    }
  )
  message(
    "\n Chromatograms and reports are now available per plate in ",
    paste(project_directory),
    ".\n Please run qcCheckR to calculate concentrations and QC the data"
  )

  invisible(list(
    success = TRUE,
    plates_processed = sapply(results[successful_plates], `[[`, "plateID"),
    plates_failed = if (any(failed_plates)) sapply(results[failed_plates], `[[`, "plateID") else character(0),
    project_directory = project_directory
  ))
}
