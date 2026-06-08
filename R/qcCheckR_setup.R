#qcCheckR Setup Functions ----
# Project setup, directory creation, file discovery functions
# Split from qcCheckR_Utils.R

###Primary Function----
#' qcCheckR_setup_project
#'
#' This function sets up the project by initialising the master list, setting up project directories, and updating the script log.
#' @keywords internal
#' @param user_name Character string representing the user name for the project.
#' @param project_directory Directory path for the project folder containing the wiff folder and .wiff and .wiff.scan files for each plate.
#' @param mrm_template_list List of lists for mrm_guides.
#' @param QC_sample_label Key for filtering QC samples from sample list.
#' @param sample_tags Vector of character strings to pull sample types for names
#' @param mv_threshold threshold for missing value filter. default is 50%.
#' @param lod_threshold Numeric instrumental limit of detection (peak area)
#'   below which values are counted as missing. default is 5000.
#' @return The updated `master_list` object with the project setup details.
#' @examples
#' \dontrun{
#' qcCheckR_setup_project(
#'   user_name, project_directory, mrm_template_list,
#'   QC_sample_label, sample_tags, mv_threshold, lod_threshold
#' )
#' }
qcCheckR_setup_project <- function(user_name,
                                   project_directory,
                                   mrm_template_list,
                                   QC_sample_label,
                                   sample_tags,
                                   mv_threshold,
                                   lod_threshold = 5000) {
  project_directory <- validate_project_directory(project_directory)
  master_list <- initialise_master_list()
  master_list <- store_environment_details(master_list)
  master_list <- qcCheckR_set_project_details(
    master_list,
    user_name,
    project_directory,
    QC_sample_label,
    sample_tags,
    mv_threshold,
    lod_threshold
  )
  master_list <- qcCheckR_read_mrm_guides(master_list, mrm_template_list)
  qcCheckR_setup_project_directories(master_list)
  master_list <- qcCheckR_import_PeakForgeR_reports(master_list)
  master_list <- find_method_version(master_list)
  master_list <- update_script_log(master_list,
                                   "project_setup",
                                   "start_time",
                                   "data_preparation")
  return(master_list)
}

###Sub Functions----
#' qcCheckR_set_project_details
#'
#' This function sets project details in the master list.
#' @keywords internal
#' @param master_list The master list object.
#' @param user_name Character string representing the user name for the project.
#' @param project_directory Directory path for the project folder.
#' @param QC_sample_label Key for filtering QC samples from sample list.
#' @param sample_tags Character vector of sample tags to filter sample types from file_names.
#' @param mv_threshold Numeric value for the missing value sample threshold.
#' @param lod_threshold Numeric instrumental limit of detection (peak area)
#'   below which values are counted as missing. Default is 5000.
#' @return The updated master list object with project details.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_set_project_details(master_list,
#'                                             user_name,
#'                                             project_directory,
#'                                             QC_sample_label,
#'                                             sample_tags,
#'                                             mv_threshold,
#'                                             lod_threshold)
#' }
qcCheckR_set_project_details <- function(master_list,
                                         user_name,
                                         project_directory,
                                         QC_sample_label,
                                         sample_tags,
                                         mv_threshold,
                                         lod_threshold = 5000) {
  master_list$project_details$project_dir <- project_directory
  master_list$project_details$user_name <- user_name
  master_list$project_details$project_name <- basename(master_list$project_details$project_dir)
  master_list$project_details$plateIDs <- c()
  master_list$project_details$plate_method_versions <- list()
  master_list$project_details$qc_type <- QC_sample_label
  master_list$project_details$script_log$timestamps$start_time <- Sys.time()
  master_list$project_details$mv_sample_threshold <- mv_threshold
  master_list$project_details$lod_threshold <- lod_threshold
  #Set sample tags: use user-supplied tags if provided, otherwise ANPC defaults
  if (!is.null(sample_tags)) {
    master_list$project_details$sample_tags <- sample_tags
  } else if (master_list$project_details$user_name == "ANPC") {
    master_list$project_details$sample_tags <- c("pqc",
                                                 "qc",
                                                 "vltr",
                                                 "sltr",
                                                 "ltr",
                                                 "blank",
                                                 "istds",
                                                 "cond",
                                                 "sample")
  } else {
    master_list$project_details$sample_tags <- sample_tags
  }
  return(master_list)
}


#' qcCheckR_read_mrm_guides
#'
#' This function reads MRM guides from user-supplied paths in the mrm_template_list.
#' @keywords internal
#' @param master_list The master list object.
#' @param mrm_template_list List of MRM guide file paths for tsv.
#' @return The updated master list object with MRM guides.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_read_mrm_guides(master_list, mrm_template_list)
#' }
qcCheckR_read_mrm_guides <- function(master_list, mrm_template_list) {
  if (master_list$project_details$user_name == "ANPC" &&
      is.null(mrm_template_list)) {

    read_and_clean_sil <- function(path) {
      guide <- readr::read_tsv(path, show_col_types = FALSE)
      replace_precursor_symbols(guide, columns = c("Precursor Name", "Note"))
    }

    read_and_clean_conc <- function(path) {
      guide <- readr::read_tsv(path, show_col_types = FALSE)
      replace_precursor_symbols(guide, columns = c("SIL_name"))
    }

    master_list$templates$mrm_guides <- list(
      v1 = list(
        SIL_guide  = read_and_clean_sil(
          system.file("extdata", "LGW_lipid_mrm_template_v1.tsv", package = "MStargetR")
        ),
        conc_guide = read_and_clean_conc(
          system.file("extdata", "LGW_SIL_batch_103.tsv", package = "MStargetR")
        )
      ),
      v2 = list(
        SIL_guide  = read_and_clean_sil(
          system.file("extdata", "LGW_lipid_mrm_template_v2.tsv", package = "MStargetR")
        ),
        conc_guide = read_and_clean_conc(
          system.file(
            "extdata",
            "LGW_SIL_batch_Ultimate_2023_03_06.tsv",
            package = "MStargetR"
          )
        )
      ),
      v3 = list(
        SIL_guide  = read_and_clean_sil(
          system.file("extdata", "LGW_lipid_mrm_template_v3.tsv", package = "MStargetR")
        ),
        conc_guide = read_and_clean_conc(
          system.file(
            "extdata",
            "LGW_SIL_batch_Ultimate_2023_03_06.tsv",
            package = "MStargetR"
          )
        )
      ),
      v4 = list(
        SIL_guide  = read_and_clean_sil(
          system.file("extdata", "LGW_lipid_mrm_template_v4.tsv", package = "MStargetR")
        ),
        conc_guide = read_and_clean_conc(
          system.file("extdata", "v4_ISTD_conc_updated.tsv", package = "MStargetR")
        )
      )
    )

  } else {
    for (version in names(mrm_template_list)) {
      guide_paths <- mrm_template_list[[version]]

      read_guide <- function(path, columns) {
        ext <- tools::file_ext(path)
        guide <- if (ext == "tsv") {
          readr::read_tsv(path, show_col_types = FALSE)
        } else if (ext == "csv") {
          readr::read_csv(path, show_col_types = FALSE)
        } else {
          stop(paste("Unsupported file type:", ext))
        }
        replace_precursor_symbols(guide, columns = columns)
      }

      master_list$templates$mrm_guides[[version]]$SIL_guide  <- read_guide(guide_paths$SIL_guide,
                                                                           columns = c("Precursor Name", "Note"))
      master_list$templates$mrm_guides[[version]]$conc_guide <- read_guide(guide_paths$conc_guide, columns = c("SIL_name"))
    }
  }

  validate_qcCheckR_mrm_template_list(master_list)

  return(master_list)
}

#' qcCheckR_setup_project_directories
#'
#' This function sets up the project directories for the master list.
#' @keywords internal
#' @param master_list The master list object.
#' @return \code{invisible(NULL)}; called for its side-effects (directory creation).
#' @examples
#' \dontrun{
#' qcCheckR_setup_project_directories(master_list)
#' }
qcCheckR_setup_project_directories <- function(master_list) {
  #set qcCheckR write path
  qcCheckR_path <- file.path(master_list$project_details$project_dir, "all")
  # Create project directories if they do not exist
  if (!dir.exists(qcCheckR_path)) {
    ok <- dir.create(qcCheckR_path, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop("Failed to create qcCheckR directory at: ", qcCheckR_path, call. = FALSE)
    }
  }

  subdirs <- c("data", "html_report", "xlsx_report")
  for (subdir in subdirs) {
    dir_path <- file.path(qcCheckR_path, subdir)
    if (!dir.exists(dir_path)) {
      ok <- dir.create(dir_path, showWarnings = FALSE)
      if (!ok) {
        stop("Failed to create subdirectory: ", dir_path, call. = FALSE)
      }
    }
  }
  if (!dir.exists(qcCheckR_path)) {
    stop("Failed to create qcCheckR directory at: ", qcCheckR_path, call. = FALSE)
  } else {
    message("qcCheckR directory set up at: ", qcCheckR_path)
  }
  invisible(NULL)
}

#' qcCheckR_import_PeakForgeR_reports
#'
#' This function imports PeakForgeR reports from the project directory and stores them in the master list.
#' @keywords internal
#' @param master_list The master list object.
#' @return The updated master list object with PeakForgeR reports imported.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_import_PeakForgeR_reports(master_list)
#' }
qcCheckR_import_PeakForgeR_reports <- function(master_list) {
  PeakForgeR_report_files <- list.files(
    master_list$project_details$project_dir,
    pattern = "_PeakForgeR_.*\\.(csv|tsv)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = FALSE,
    include.dirs = FALSE
  )

  if (length(PeakForgeR_report_files) == 0) {
    stop(
      "No report files found in the specified project directory. Please ensure reports are in .csv or .tsv format and named with '_PeakForgeR_' pattern.",
      call. = FALSE
    )
  }

  for (file in PeakForgeR_report_files) {
    short_link <- NULL

    if (.Platform$OS.type == "windows" && nchar(file) > 260) {
      path <- dirname(file)
      short_link <- file.path(
        tempdir(),
        paste0("PeakForgeR_", Sys.getpid(), "_", substring(gsub("[^0-9A-Za-z]", "", basename(file)), 1, 12))
      )
      if (dir.exists(short_link)) {
        unlink(short_link, recursive = TRUE, force = TRUE)
      }
      mklink_out <- system2("cmd", args = c("/c", "mklink", "/J",
                               shQuote(short_link), shQuote(path)),
              stdout = TRUE, stderr = TRUE)
      mklink_status <- attr(mklink_out, "status")
      if (!is.null(mklink_status) && mklink_status != 0) {
        stop("Failed to create directory junction for long path '", file,
             "'. mklink exit status: ", mklink_status,
             ". Output: ", paste(mklink_out, collapse = " "),
             call. = FALSE)
      }
      file_to_read <- file.path(short_link, basename(file))
    } else {
      file_to_read <- file
    }

    file_ext <- tools::file_ext(file_to_read)
    if (file_ext == "csv") {
      PeakForgeR_report <- readr::read_csv(file_to_read, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    } else if (file_ext == "tsv") {
      PeakForgeR_report <- readr::read_tsv(file_to_read, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    } else {
      warning(paste("Unsupported file type:", file))
      next
    }

    if (!is.null(short_link) && dir.exists(short_link)) {
      unlink(short_link, recursive = TRUE, force = TRUE)
    }

    file_name <- basename(file_to_read) %>%
      sub("\\.(csv|tsv)$", "", .) %>%
      sub("_PeakForgeR_", "_", .)

    colnames(PeakForgeR_report) <- gsub("\\.", "", colnames(PeakForgeR_report))
    colnames(PeakForgeR_report) <- gsub(" ", "", colnames(PeakForgeR_report))

    PeakForgeR_report <- PeakForgeR_report[!is.na(PeakForgeR_report$FileName), ]
    area_numeric <- suppressWarnings(as.numeric(PeakForgeR_report$Area))
    height_numeric <- suppressWarnings(as.numeric(PeakForgeR_report$Height))
    PeakForgeR_report <- PeakForgeR_report[
      !is.na(area_numeric) & area_numeric != 0 &
      !is.na(height_numeric) & height_numeric != 0, ]
    PeakForgeR_report <- PeakForgeR_report[rowSums(!is.na(PeakForgeR_report)) > 0L, ]
    if (identical(master_list$project_details$user_name, "ANPC")) {
      anpc_drop_mask <- grepl(
        "(?i)(?:^|[_-])(?:COND|BLANK|ISTDs)(?=[_-]|\\d|$)",
        PeakForgeR_report$FileName, perl = TRUE
      )
      if (any(anpc_drop_mask)) {
        message("ANPC filter: dropping ", sum(anpc_drop_mask),
                " row(s) from '", basename(file), "': ",
                paste(unique(PeakForgeR_report$FileName[anpc_drop_mask]), collapse = ", "))
      }
      PeakForgeR_report <- PeakForgeR_report[!anpc_drop_mask, ]
    }
    PeakForgeR_report <- suppressWarnings(readr::type_convert(PeakForgeR_report, col_types = NULL))
    master_list$data$PeakForgeRReport[[file_name]] <- PeakForgeR_report
    master_list$project_details$plateIDs <- c(master_list$project_details$plateIDs, file_name)
  }

  return(master_list)
}

#' Find method versions
#'
#' This function finds the method versions for each plate in the master list.
#' It matches internal standards to the SIL guide and stores the matching SIL_guide.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with method versions found for each plate.
find_method_version <- function(master_list) {
  for (plate_id in names(master_list$data$PeakForgeRReport)) {
    #Gather unique SIL molecule names from the PeakForgeR report
    report_SILS <-  master_list$data$PeakForgeRReport[[plate_id]] %>%
      dplyr::select(dplyr::contains("MoleculeName")) %>%
      dplyr::filter(grepl("SIL", MoleculeName, ignore.case = TRUE)) %>%
      unique()

    #Check for SILs in the SIL guides
    for (version in names(master_list$templates$mrm_guides)) {
      sil_guide <- master_list$templates$mrm_guides[[version]]$SIL_guide
      sil_targets <- sil_guide %>%
        dplyr::select(dplyr::contains("Precursor Name")) %>%
        dplyr::filter(grepl("SIL", `Precursor Name`, ignore.case = TRUE)) %>%
        unique()

      #Check if all SILs in the report are present in the SIL guide
      if (nrow(report_SILS) > 0 && all(report_SILS$MoleculeName %in% sil_targets$`Precursor Name`)) {
        master_list$project_details$plate_method_versions[[plate_id]] <- version
        #message found version
        message("Method version found for plate ", plate_id, ": ", version)
        break
      }
    }
  }
  unmatched <- setdiff(
    names(master_list$data$PeakForgeRReport),
    names(master_list$project_details$plate_method_versions)
  )
  if (length(unmatched) > 0) {
    stop("Could not match SIL guide version for plate(s): ",
         paste(unmatched, collapse = ", "),
         ". Check that MRM templates contain matching SIL standards.",
         call. = FALSE)
  }

  return(master_list)
}
