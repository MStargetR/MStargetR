#' Utils_Global.R

#' Import specific functions from packages
#' @keywords internal
#' @name PeakForgeR_import_external_functions
#' @importFrom utils installed.packages sessionInfo str flush.console
#' @importFrom readr read_csv write_csv read_tsv write_tsv
#' @importFrom dplyr bind_rows bind_cols filter select rename arrange contains intersect
#' @importFrom rlang .data
#' @importFrom tibble tibble add_column
#' @importFrom stringr str_extract str_sub
#' @importFrom mzR openMSfile chromatogramHeader chromatograms
#' @importFrom janitor clean_names
#' @importFrom magrittr %>%
#' @importFrom stats median setNames
NULL

#PeakForgeR internal functions----

#.----
#Setup Project Functions----

###Primary Function----
#' PeakForgeR_setup_project
#'
#' This function sets up the project by initialising the master list,
#' setting up project directories, and updating the script log.
#' @keywords internal
#' @param user_name string identifying user
#' @param project_directory Directory path for the project folder containing
#' the wiff folder and wiff files.
#' @param plateID Plate ID for the current plate.
#' @param mrm_template_list List of MRM guides.
#' @param QC_sample_label Key for filtering QC samples from sample list.
#' @return The updated `master_list` object with the project setup details.
#' @examples
#' \dontrun{
#' PeakForgeR_setup_project("path/to/project_directory", "plateID",
#'mrm_template_list, "QC_sample_label")
#' }
PeakForgeR_setup_project <- function(user_name,
                                  project_directory,
                                  plateID,
                                  mrm_template_list,
                                  QC_sample_label) {
  master_list <- initialise_master_list()
  master_list <- store_environment_details(master_list)
  master_list <- set_project_details(master_list, user_name, project_directory, plateID, QC_sample_label)
  master_list <- read_mrm_guides(master_list, mrm_template_list)
  setup_project_directories(master_list)
  master_list <- update_script_log(master_list, "project_setup", "start_time", "mzR_mzml_import")
  return(master_list)
}

###Sub Functions----
#' initialise_master_list
#'
#' This function initialises the master list with default values.
#' @keywords internal
#' @return A list representing the initialised master list.
#' @examples
#' \dontrun{
#' master_list <- initialise_master_list()
#' }
initialise_master_list <- function() {
  master_list <- list()
  master_list$environment <- list()
  master_list$environment$user_functions <- list()
  master_list$templates <- list()
  master_list$templates$mrm_guides <- list()
  master_list$project_details <- list()
  master_list$data <- list()
  master_list$summary_tables <- list()
  master_list$process_lists <- list()
  return(master_list)
}

#' store_environment_details
#'
#' This function stores environment details in the master list.
#' @keywords internal
#' @param master_list The master list object.
#' @return The updated master list object with environment details.
#' @examples
#' \dontrun{
#' master_list <- store_environment_details(master_list)
#' }
store_environment_details <- function(master_list) {

  si <- utils::sessionInfo()
  master_list$environment$r_version <- si$R.version$version.string
  master_list$environment$base_packages <- si$basePkgs

  installed <- utils::installed.packages()
  attached <- names(si$otherPkgs)
  valid_pkgs <- attached[attached %in% rownames(installed)]
  versions <- installed[valid_pkgs, "Version"]
  master_list$environment$user_packages <- paste0(valid_pkgs, ": ", versions)

  return(master_list)
}

#' set_project_details
#'
#' This function sets project details in the master list.
#' @keywords internal
#' @param master_list The master list object.
#' @param user_name string specifying user
#' @param project_directory Directory path for the project folder.
#' @param plateID Plate ID for the current plate.
#' @param QC_sample_label Key for filtering QC samples from sample list.
#' @return The updated master list object with project details.
#' @examples
#' \dontrun{
#' master_list <- set_project_details(master_list,
#'                                    "John Smith"
#'                                    "path/to/project_directory",
#'                                    "plateID",
#'                                    "QC_sample_label")
#' }
set_project_details <- function(master_list,
                                user_name,
                                project_directory,
                                plateID,
                                QC_sample_label) {
  master_list$project_details$project_dir <- project_directory
  master_list$project_details$PeakForgeR_version <- as.character(utils::packageVersion("MStargetR"))
  master_list$project_details$user_name <- user_name
  master_list$project_details$project_name <- basename(master_list$project_details$project_dir)
  master_list$project_details$plateID <- plateID
  master_list$project_details$qc_type <- QC_sample_label
  master_list$project_details$script_log$timestamps$start_time <- Sys.time()
  return(master_list)
}

#' read_mrm_guides
#'
#' This function reads MRM guides from user-supplied paths in the mrm_template_list.
#' Capable of reading .tsv or .csv files
#' @keywords internal
#' @param master_list The master list object.
#' @param mrm_template_list List of MRM guide file paths.
#' @return The updated \code{master_list} with each validated MRM guide stored
#'   under \code{master_list$templates$mrm_guides[[version]]$mrm_guide}, where
#'   \code{version} is the basename of the corresponding template file.
#' @examples
#' \dontrun{
#' master_list <- read_mrm_guides(master_list, mrm_template_list)
#' }

read_mrm_guides <- function(master_list, mrm_template_list) {
  basenames <- vapply(mrm_template_list, basename, character(1))
  duplicated_names <- basenames[duplicated(basenames)]
  if (length(duplicated_names) > 0) {
    conflicting_paths <- mrm_template_list[basenames %in% duplicated_names]
    stop("read_mrm_guides: two or more mrm_template files share the same basename. ",
         "Each template must have a unique filename.\n",
         "Conflicting paths:\n",
         paste("  -", conflicting_paths, collapse = "\n"),
         call. = FALSE)
  }
  names(mrm_template_list) <- basenames

  for (version in names(mrm_template_list)) {
    file_path <- mrm_template_list[[version]]

    # Determine file extension and read accordingly
    if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
      guide_data <- readr::read_csv(file_path,
                                    show_col_types = FALSE,
                                    name_repair = "minimal")
    } else if (grepl("\\.tsv$", file_path, ignore.case = TRUE)) {
      guide_data <- readr::read_tsv(file_path,
                                    show_col_types = FALSE ,
                                    name_repair = "minimal")
    } else {
      stop(
        paste(
          "Unsupported file format for:",
          file_path,
          ". \n Please ensure mrm_templates are .csv or .tsv"
        )
      )
    }

    # validate mrm_template_list
    ##Check column  names and length
    mandatory_cols <- c(
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
    guide_cols <- colnames(guide_data)
    matching_cols  <- dplyr::intersect(mandatory_cols, guide_cols)

    # report if all cols are matching
    if (length(matching_cols) != length(mandatory_cols)) {
      missing_cols <- setdiff(mandatory_cols, guide_cols)
      stop(paste(
        version,
        ": Missing mandatory columns: ",
        paste(missing_cols, collapse = ", ")
      ))
    }

    ## Check there are no NA in all columns
    ## Except "Note" if "Precursor Name" contains "SIL" then expect an NA
    # Restrict "SIL" detection to names where SIL appears as a distinct token
    # delimited by a non-alphabetic character (e.g. PC_SIL_d7, SIL_d6) to
    # avoid false positives from legitimate metabolite names such as "Silybin".
    sil_rows <- grepl("(^|[^A-Za-z])SIL([^A-Za-z]|$)", guide_data[["Precursor Name"]],
                      ignore.case = TRUE)
    cols_to_check <- setdiff(mandatory_cols, "Note")
    for (col in cols_to_check) {
      if (any(is.na(guide_data[[col]]))) {
        stop(paste(
          version,
          ": Column",
          col,
          "contains NA values, which are not allowed."
        ))
      }
    }
    note_na <- is.na(guide_data[["Note"]])
    invalid_note_na <- note_na & !sil_rows
    if (any(invalid_note_na)) {
      stop(version,
          ": NA values in 'Note' column
        are only allowed for rows where 'Precursor Name' contains 'SIL'."
      )
    }

    full_message <- paste(version,
                          ": All required columns are validated and contain no unexpected NA values.")
    message(full_message)

    guide_data <- replace_precursor_symbols(guide_data, columns = c("Precursor Name", "Note"))

    transition_check_result <- transition_checkR(guide_data)
    if (!is.null(transition_check_result)) {
      dup_summary <- tryCatch(
        paste(utils::capture.output(print(transition_check_result)), collapse = "\n"),
        error = function(e) "(could not format duplicate details)"
      )
      stop(paste0(version, ": Non-unique transitions detected. Please amend mrm_template prior rerunning.\n",
                  "Duplicate transitions:\n", dup_summary))
    }

    #Store valdiated mrm_template in master_list
    master_list$templates$mrm_guides[[version]]$mrm_guide <- guide_data
  }

  return(master_list)
}

#' setup_project_directories
#'
#' This function sets up project directories for each plate ID.
#' @keywords internal
#' @param master_list The master list object.
#' @return None. The function sets up directories.
#' @examples
#' \dontrun{
#' setup_project_directories(master_list)
#' }
setup_project_directories <- function(master_list) {
  for (plate_ID in master_list$project_details$plateID) {
    base_path <- file.path(master_list$project_details$project_dir, plate_ID)
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
      file.path(base_path, "data", "rda"),
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

#.----
#Peak Picking Functions ----

###Primary Function----
#' peak_picking
#'
#' This function processes mzML files for each plate, optimizes retention times, updates peak boundaries, and checks for SIL internal standards.
#' @keywords internal
#' @param plateID Plate ID for the current plate.
#' @param master_list Master list generated internally.
#' @return The updated `master_list` object with peak picking details.
#' @examples
#' \dontrun{
#' peak_picking(plateID, master_list)
#' }
peak_picking <- function(plateID, master_list) {
  validate_master_list_project_directory(master_list)
  plate_idx <- plateID
  message(paste("Processing plate:", plateID))

  sil_found <- FALSE
  version_errors <- list()

  versions <- names(master_list$templates$mrm_guides)
  versions <- setdiff(versions, "by_plate")

  if(master_list$project_details$user_name == "ANPC"){
  likely_version <- version_selector(master_list)
    if (!is.na(likely_version) && likely_version %in% versions) {
      versions <- c(likely_version, setdiff(versions, likely_version))
    }
  }

  for (i in seq_along(versions)) {
    version <- versions[i]

    if (sil_found)
      break

    full_msg <- paste("Starting peak picking and integration using version:",
                      version)
    border <- paste(rep("=", nchar(full_msg) + 4), collapse = "")
    message("\n", border)
    message("= ", full_msg, " =")
    message(border, "\n")

    version_success <- tryCatch({
      master_list$project_details$is_ver <- version
      message("Creating summary table for plate: ", plate_idx)
      master_list$summary_tables$project_summary <- create_summary_table(master_list, plate_idx)
      message("Optimising retention times for plate: ", plate_idx)
      master_list$templates$mrm_guides$by_plate[[plate_idx]] <- optimise_retention_times(master_list, plate_idx)

      message("Exporting files for plate: ", plate_idx)
      export_files(master_list, plate_idx)
      message("File export complete for plate: ", plate_idx)
      PeakForgeR_command <- execute_PeakForgeR_command(master_list, plate_idx)

      system_success <- tryCatch({
        output_file <- file.path(
          master_list$project_details$project_dir,
          "MStargetR_logs",
          paste0(plate_idx, "_MStargetR_log.txt")
        )

        run_system_command(PeakForgeR_command, output_file)
        TRUE

      }, error = function(e) {
        message("System command failed in version ",
                version,
                ": ",
                e$message)
        FALSE
      })

      if (!system_success) {
        message("Skipping version due to Skyline failure.\n")
        version_errors[[version]] <- "Skyline command failed"
        stop("Skyline command failed for plate: ", plate_idx,
             " with version: ", version, call. = FALSE)
      }

      message("Reimporting PeakForgeR output for plate: ", plate_idx)
      master_list$data$PeakForgeR_report[[plate_idx]] <- reimport_PeakForgeR_file(master_list, plate_idx)
      message("Checking SIL internal standards for plate: ", plate_idx,
              " with version: ", version)
      sil_result <- check_sil_standards(master_list, plate_idx, version)

      if (isTRUE(sil_result)) {
        sil_found <- TRUE
        save_plate_data(master_list, plate_idx)
        message("SIL matched to version:", version, "\n")
        message("PeakForgeR data saved for plate:", plate_idx, "\n")
      } else {
        version_errors[[version]] <- "No SIL standards detected"
        message("No SIL standards detected with version: ",
                version,
                "- trying next version\n")
      }

      TRUE
    }, error = function(e) {
      version_errors[[version]] <<- e$message
      message("\nError during processing of version ",
              version,
              ":\n",
              e$message,
              "\n")
      message("--- End of error for version ", version, "---\n")
      flush.console()
      FALSE
    })

    if (!version_success)
      next
  }

  if (!sil_found) {
    report_peak_picking_failure(plate_idx, version_errors)
  }

  # Wrap update_script_log in a directory-scoped block so the wd is
  # restored even on error / interrupt. See REVIEW_REPORT BC-H10.
  master_list <- mstargetr_with_dir(
    master_list$project_details$project_dir,
    update_script_log(
      master_list,
      "peak_picking_and_integration",
      "mzR_mzml_import",
      "next_plate_for_processing"
    )
  )
  return(master_list)
}


#' Report peak picking failure with informative error message
#'
#' Called when no version produced a SIL match. Distinguishes between
#' Skyline failures and missing SIL standards.
#'
#' @param plate_idx Plate identifier string.
#' @param version_errors Named list of error messages keyed by version.
#' @keywords internal
report_peak_picking_failure <- function(plate_idx, version_errors) {
  skyline_failures <- version_errors[grepl("Skyline|command failed", version_errors)]
  if (length(skyline_failures) > 0) {
    stop("Skyline failed for plate ", plate_idx, ". ",
         "Versions attempted: ",
         paste(names(version_errors), version_errors, sep = " -> ", collapse = "; "),
         ". Check MStargetR_logs for the full Docker command and Skyline output.",
         call. = FALSE)
  } else {
    stop("No SIL internal standards detected in plate ", plate_idx,
         " after trying all method versions (",
         paste(names(version_errors), collapse = ", "),
         "). Please ensure your mrm_guide matches the transitions used in the project!",
         call. = FALSE)
  }
}

###Sub Functions----

#' version_selector
#'
#' ANPC specific helper function to reorder first version tried based on jwist naming conventions.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return Updated version list with the most probabilistic version first.
#' @examples
#' \dontrun{
#' version_selector(master_list)
#' }
version_selector <- function(master_list) {

  # Extract version from plateID
  plate_id <- master_list$project_details$plateID
  plate_indicated_version <- unique(stringr::str_extract(plate_id, "_MS-LIPIDS(?:-[0-9]+)?"))

  # Define convention key as a named list.
  # The plate suffix "_MS-LIPIDS-N" is intended to map directly to the
  # "vN.tsv" mrm_template file. Previously "_MS-LIPIDS-3" was mapped to
  # "v2.tsv" (stale / drifted) which caused the wrong template to be tried
  # first and masked real version matches. Keys now match filenames
  # literally so the first-attempt version is correct. See REVIEW_REPORT
  # BC-H6.
  convention_key <- list(
    "_MS-LIPIDS"   = "LGW_lipid_mrm_template_v1.tsv",
    "_MS-LIPIDS-2" = "LGW_lipid_mrm_template_v2.tsv",
    "_MS-LIPIDS-3" = "LGW_lipid_mrm_template_v3.tsv",
    "_MS-LIPIDS-4" = "LGW_lipid_mrm_template_v4.tsv"
  )

  matching_version <- convention_key[[plate_indicated_version]]

  if (is.null(matching_version)) {
    message("No method version tag found in plateID: ", plate_id,
            ". All available versions will be attempted sequentially.")
    return(NA)
  }

  return(matching_version)
}

#' create_summary_table
#'
#' This function creates a summary table for a given plate in the master list, including various project details.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to create the summary table for.
#' @return A tibble containing the summary table with project details and their values.
#' @examples
#' \dontrun{
#' create_summary_table(master_list, plate_idx)
#' }
create_summary_table <- function(master_list, plate_idx) {
  Temp_list <- master_list$project_details[c(
    "project_dir",
    "PeakForgeR_version",
    "user_name",
    "project_name",
    "qc_type",
    "plateID",
    "is_ver"
  )]
  Temp_list$plateID <- paste(plate_idx)
  project_summary <- tibble::tibble(unlist(Temp_list)) %>%
    tibble::add_column(
      "Project detail" = c(
        "local directory",
        "PeakForgeR version",
        "user initials",
        "project name",
        "project QC",
        "plateID",
        "int. std. version"
      ),
      .before = 1
    )
  project_summary <- stats::setNames(project_summary, c("Project detail", "value"))
  return(project_summary)
}

#' optimise_retention_times
#'
#' This function optimises retention times for each plate in the master list using the mzR_mrm_findR function and updates the MRM guide.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param plate_idx A vector of plate indices to optimise retention times for.
#' @return A list containing the optimised retention times and updated MRM guide for each plate.
#' @examples
#' \dontrun{
#' optimise_retention_times(master_list, plate_idx)
#' }
optimise_retention_times <- function(master_list, plate_idx) {
  by_plate <- list()
  idx <- plate_idx
  result <- mzR_mrm_findR(
    FUNC_mzR = master_list$data[[idx]],
    FUNC_mrm_guide = master_list$templates$mrm_guides[[master_list$project_details$is_ver]]$mrm_guide %>% janitor::clean_names(),
    FUNC_OPTION_qc_type = master_list$project_details$qc_type
  ) %>% append(master_list$templates$mrm_guides[[master_list$project_details$is_ver]])

  #Replace 0 retention times with default
  if ("Explicit Retention Time" %in% names(result$mrm_guide_updated)) {
    zero_rt_indices <- which(!is.na(result$mrm_guide_updated$`Explicit Retention Time`) &
                               result$mrm_guide_updated$`Explicit Retention Time` == 0)
    result[["mrm_guide_updated"]][["Explicit Retention Time"]][zero_rt_indices] <-
      result[["mrm_guide"]][["Explicit Retention Time"]][zero_rt_indices]
  }

  by_plate[[plate_idx]] <- result
  original_names <- names(by_plate[[plate_idx]]$mrm_guide)
  if (!is.null(original_names) &&
      length(original_names) == ncol(by_plate[[plate_idx]]$mrm_guide_updated)) {
    by_plate[[plate_idx]]$mrm_guide_updated <- stats::setNames(
      by_plate[[plate_idx]]$mrm_guide_updated, original_names
    )
  }


  message("Successfully optimised retention times!")
  return(by_plate)
}

#' export_files
#'
#' This function exports various files related to the project for a given plate, including updated MRM guides, peak boundaries, and default templates.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to export files for.
#' @return Exports CSV, SKY, and TSV files to the specified project directory.
#' @examples
#' \dontrun{
#' export_files(master_list, plate_idx)
#' }
export_files <- function(master_list, plate_idx) {
  long_path <- file.path(master_list$project_details$project_dir,
                         plate_idx,
                         "data",
                         "PeakForgeR")

  with_short_junction(long_path, function(PeakForgeR_path) {
    # Clean old PeakForgeR output to prevent stale data on re-runs
    old_files <- list.files(PeakForgeR_path,
                            pattern = "\\.(sky|csv|tsv|skyr)$",
                            full.names = TRUE)
    if (length(old_files) > 0) {
      message("  Cleaning ", length(old_files), " old file(s) from PeakForgeR directory")
      file.remove(old_files)
    }

    by_plate_entry <- master_list$templates$mrm_guides$by_plate[[plate_idx]][[plate_idx]]

    readr::write_csv(
      x = by_plate_entry$mrm_guide_updated,
      file = file.path(
        PeakForgeR_path,
        paste0(Sys.Date(), "_RT_update_", plate_idx, ".csv")
      )
    )

    readr::write_csv(
      x = by_plate_entry$peak_boundary_update,
      file = file.path(
        PeakForgeR_path,
        paste0(Sys.Date(), "_peak_boundary_update_", plate_idx, ".csv")
      )
    )

    # Helper to copy and rename template files with error checking
    copy_template <- function(template_name, dest_name) {
      src <- system.file("templates", template_name, package = "MStargetR")
      if (!nzchar(src) || !file.exists(src)) {
        stop("Template '", template_name, "' not found in MStargetR package.", call. = FALSE)
      }
      dest <- file.path(PeakForgeR_path, dest_name)
      ok <- file.copy(from = src, to = dest, overwrite = TRUE)
      if (!ok || !file.exists(dest)) {
        stop("Failed to copy template '", template_name, "' to: ", dest,
             "\n  Source exists: ", file.exists(src),
             "\n  Destination dir exists: ", dir.exists(PeakForgeR_path),
             "\n  Destination path length: ", nchar(dest), " chars",
             call. = FALSE)
      }
      message("  Copied: ", dest_name)
    }

    copy_template("default_skyline_file.sky",
                  paste0(Sys.Date(), "_", plate_idx, ".sky"))
    copy_template("default_csv.csv",
                  paste0(Sys.Date(), "_PeakForgeR_", plate_idx, ".csv"))
    copy_template("default_tsv.tsv",
                  paste0(Sys.Date(), "_", plate_idx, "_chromatograms.tsv"))
    copy_template("YYYY-MM-DD_PeakForgeR_project_name.skyr",
                  "YYYY-MM-DD_PeakForgeR_project_name.skyr")
  })
}

#' reimport_PeakForgeR_file
#'
#' This function reimports a PeakForgeR file for a given plate, converts specific columns to numeric, and cleans the column names.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to reimport the Skyline file for.
#' @return A data frame containing the reimported PeakForgeR data with cleaned column names.
#' @examples
#' \dontrun{
#' reimport_PeakForgeR_file(master_list, plate_idx)
#' }
reimport_PeakForgeR_file <- function(master_list, plate_idx) {
  message("Reimporting PeakForgeR CSV for plate: ", plate_idx)
  long_path <- file.path(master_list$project_details$project_dir,
                         plate_idx,
                         "data",
                         "PeakForgeR")

  PeakForgeR_data <- with_short_junction(long_path, function(PeakForgeR_path) {
    matched_files <- list.files(
      PeakForgeR_path,
      pattern = paste0("_PeakForgeR_", plate_idx, "\\.csv$"),
      full.names = TRUE
    )

    if (length(matched_files) == 0) {
      stop("No PeakForgeR output file found for plate '", plate_idx,
           "' in: ", PeakForgeR_path, call. = FALSE)
    }

    if (length(matched_files) > 1) {
      file_info <- file.info(matched_files)
      matched_files <- matched_files[order(file_info$mtime, decreasing = TRUE)]
      message("  Warning: ", length(matched_files),
              " PeakForgeR files match plate '", plate_idx,
              "'. Using most recent: ", basename(matched_files[1]))
    }

    # Read all columns as character so readr's row-1000 type-inference
    # cannot emit "One or more parsing issues" warnings when later rows
    # contain blanks or "#N/A" in columns it inferred as numeric. The
    # numeric coercion below (cols_to_convert) is the authoritative pass
    # and reports unconvertible values per column.
    suppressMessages(readr::read_csv(
      matched_files[1],
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    ))
  })

  cols_to_convert <- c(
    "PrecursorMz",
    "ProductMz",
    "RetentionTime",
    "StartTime",
    "EndTime",
    "Area",
    "Height"
  )
  for (col in cols_to_convert) {
    original <- PeakForgeR_data[[col]]
    converted <- suppressWarnings(as.numeric(original))
    n_failed <- sum(is.na(converted) & !is.na(original))
    if (n_failed > 0) {
      bad_values <- unique(original[is.na(converted) & !is.na(original)])
      message("  Warning: Column '", col, "' in plate '", plate_idx,
              "': ", n_failed, " value(s) could not be converted to numeric ",
              "(e.g. ", paste(head(bad_values, 3), collapse = ", "), "). Set to NA.")
    }
    PeakForgeR_data[[col]] <- converted
  }
  PeakForgeR_data <- janitor::clean_names(PeakForgeR_data)

  message("PeakForgeR CSV reimported for plate: ", plate_idx,
          " (", nrow(PeakForgeR_data), " rows)")
  return(PeakForgeR_data)
}

#' check_sil_standards
#'
#' This function checks if the SIL standards on a given plate match the SIL standards for a specified mrm_guide version.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to check the SIL standards for.
#' @param current_version The version of the MRM guide to compare against.
#' @return A logical value indicating whether the SIL standards on the plate match the SIL standards for the specified version.
#' @examples
#' \dontrun{
#' check_sil_standards(master_list, plate_idx, current_version)
#' }
check_sil_standards <- function(master_list, plate_idx, current_version) {
  # Extract SILs from the plate. Use the same non-alphabetic delimiter pattern
  # as read_mrm_guides to avoid false positives from names like "Silybin".
  sil_pattern <- "(^|[^A-Za-z])SIL([^A-Za-z]|$)"
  sil_on_plate <- master_list$data$PeakForgeR_report[[plate_idx]] %>%
    dplyr::filter(grepl(sil_pattern, .data$molecule_name, ignore.case = TRUE)) %>%
    dplyr::pull(.data$molecule_name) %>%
    unique()

  template_versions <- setdiff(names(master_list$templates$mrm_guides), "by_plate")

  sil_by_version <- lapply(template_versions, function(version) {
    master_list$templates$mrm_guides[[version]]$mrm_guide %>%
      dplyr::filter(grepl(sil_pattern, `Precursor Name`, ignore.case = TRUE)) %>%
      dplyr::pull(`Precursor Name`) %>%
      unique()
  })
  names(sil_by_version) <- template_versions

  current_sils <- sil_by_version[[current_version]]

  other_versions <- setdiff(template_versions, current_version)
  other_sils <- unlist(sil_by_version[other_versions], use.names = FALSE)

  # Compute SILs unique to current version
  unique_sils_current <- setdiff(current_sils, other_sils)

  # If all SILs overlap across versions, fall back to matching against full set
  if (length(unique_sils_current) == 0) {
    unique_sils_current <- current_sils
  }

  # Short-circuit: if no unique SILs exist at all, we cannot confirm this version
  if (length(unique_sils_current) == 0) {
    message("check_sil_standards: no SIL standards found in version '", current_version,
            "'. Cannot confirm version match.")
    return(FALSE)
  }

  # Match plate SILs to unique SILs and keep if 90% match
  matching_sils <- intersect(sil_on_plate, unique_sils_current)
  match_ratio <- length(matching_sils) / length(unique_sils_current)

  sil_found <- match_ratio >= 0.90

  return(sil_found)
}

#' save_plate_data
#'
#' This function saves the master list data for a given plate to an RDA file.
#' @keywords internal
#' @importFrom callr r_bg
#' @param master_list A list containing project details and data.
#' @param plate_idx The index of the plate to save the data for.
#' @return Saves the master list data to an RDA file in the specified project directory.
#' @examples
#' \dontrun{
#' save_plate_data(master_list, plate_idx)
#' }
save_plate_data <- function(master_list, plate_idx) {
  message("Saving plate data for: ", plate_idx)
  long_path <- file.path(master_list$project_details$project_dir,
                         plate_idx,
                         "data",
                         "rda")
  short_junction <- tempfile(pattern = "rda_")
  if (nchar(long_path) > 260 && .Platform$OS.type == "windows") {
    message("Path exceeds 260 chars; creating junction: ", short_junction, " -> ", long_path)
    # Prefer Sys.junction(); see REVIEW_REPORT DOCK-C2. Fall back to a
    # single pre-quoted cmd /c string via shell() only if Sys.junction fails.
    ok <- tryCatch({
      Sys.junction(long_path, short_junction)
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(ok)) {
      cmd <- sprintf('cmd /c mklink /J "%s" "%s"', short_junction, long_path)
      shell(cmd, intern = FALSE)
    }
    save_path <- short_junction
  } else {
    # Use full path directly on macOS/Linux
    save_path <- file.path(master_list$project_details$project_dir,
                           plate_idx,
                           "data",
                           "rda")
  }

  # Serialise master_list to a tempfile in the parent so the callr child only
  # receives a path string. This avoids doubling peak memory by sending the
  # entire object (potentially hundreds of MB) through the callr pipe.
  tmp_rds <- tempfile(pattern = "master_list_save_", fileext = ".rds")
  saveRDS(master_list, file = tmp_rds, compress = FALSE)
  on.exit(unlink(tmp_rds), add = TRUE)

  # Capture the background handle so we can wait on / inspect exit status.
  # Previously the handle was discarded, allowing the save to race against
  # session exit and any junction cleanup to leak silently on child crash.
  # See REVIEW_REPORT BC-C2.
  handle <- callr::r_bg(function(tmp_rds, save_path, plate_idx, short_junction) {
    master_list <- readRDS(tmp_rds)
    save(
      master_list,
      file = file.path(
        save_path,
        paste0(Sys.Date(), "_", master_list$project_details$project_name, "_",
               plate_idx, "_PeakForgeR.rda")
      ),
      compress = FALSE
    )
    # Clean up junction after save completes (avoids race condition)
    if (!is.null(short_junction) && dir.exists(short_junction)) {
      unlink(short_junction, recursive = TRUE)
    }
  }, args = list(
    tmp_rds = tmp_rds,
    save_path = save_path,
    plate_idx = plate_idx,
    short_junction = if (nchar(long_path) > 260 && .Platform$OS.type == "windows") short_junction else NULL
  ))

  # Wait up to 10 minutes for the save to finish (large master_list).
  # callr $wait() blocks up to `timeout` ms; check $is_alive() afterwards
  # to distinguish "finished" from "timed out".
  # Guard against a NULL handle: r_bg can return NULL when stubbed in tests.
  if (is.null(handle) || !inherits(handle, c("r_process", "process"))) {
    message("Plate data save initiated for: ", plate_idx)
    return(invisible(NULL))
  }
  save_timeout_ms <- 10 * 60 * 1000L
  tryCatch(
    handle$wait(timeout = save_timeout_ms),
    error = function(e) NULL
  )
  if (isTRUE(handle$is_alive())) {
    # Timed out — kill the child and clean up the junction ourselves.
    try(handle$kill(), silent = TRUE)
    if (nchar(long_path) > 260 && .Platform$OS.type == "windows" &&
        dir.exists(short_junction)) {
      unlink(short_junction, recursive = TRUE)
    }
    stop("save_plate_data: background save for plate '", plate_idx,
         "' exceeded ", save_timeout_ms / 1000, "s. Child process killed. ",
         "Check disk space, permissions on: ", save_path, call. = FALSE)
  }
  status <- handle$get_exit_status()
  if (is.null(status) || !identical(as.integer(status), 0L)) {
    # Child crashed or exited with non-zero status.
    child_out <- tryCatch(handle$read_all_output(), error = function(e) "")
    child_err <- tryCatch(handle$read_all_error(), error = function(e) "")
    if (nchar(long_path) > 260 && .Platform$OS.type == "windows" &&
        dir.exists(short_junction)) {
      unlink(short_junction, recursive = TRUE)
    }
    stop("save_plate_data: background save for plate '", plate_idx,
         "' failed (exit status: ",
         if (is.null(status)) "NULL" else status, ").",
         if (nzchar(child_out)) paste0("\nChild stdout:\n", child_out) else "",
         if (nzchar(child_err)) paste0("\nChild stderr:\n", child_err) else "",
         call. = FALSE)
  }
  message("Plate data save completed for: ", plate_idx)
}
