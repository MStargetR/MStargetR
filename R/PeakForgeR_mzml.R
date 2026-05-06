# PeakForgeR_mzml.R
# mzML file import, parsing, and chromatogram handling functions.
# Includes mzR MRM finding, peak boundary detection, and lipid matching.

# ANPC-specific pattern to exclude conditioning, blank, and ISTD-only runs from
# the QC mzML list. The pattern matches filenames whose COND/BLANK/ISTDs token
# is delimited by underscore, hyphen, a digit, or the string boundary so that
# partial matches inside sample codes are avoided.
# Case-insensitive (perl = TRUE) so "istds", "Istds", etc. are all excluded.
ANPC_EXCLUDE_PATTERN <- "(?i)(?:^|[_-])(?:COND|BLANK|ISTDs)(?=[_-]|\\d|$)"

#.----
#Import mzML Files Functions ----

###Primary Function----
#' import_mzml
#'
#' This function imports mzML files for each plate using the mzR package, extracts relevant information, and updates the script log.
#' @keywords internal
#' @param plateID Plate ID for the current plate.
#' @param master_list Master list generated internally.
#' @return The updated `master_list` object with the mzML import details.
#' @examples
#' \dontrun{
#' import_mzml("plateID", master_list)
#' }
import_mzml <- function(plateID, master_list) {
  validate_master_list_project_directory(master_list)
  mzml_filelist <- initialise_mzml_filelist(master_list)
  master_list <- process_plates(master_list, mzml_filelist)
  master_list <- update_script_log(
    master_list,
    "mzR_mzml_import",
    "project_setup",
    "peak_picking_and_integration"
  )
  return(master_list)
}

###Sub Functions----

#' initialise_mzml_filelist
#'
#' This function initializes a list of mzML files for each plate in the master list, excluding files with specific patterns.
#' @keywords internal
#' @param master_list A list containing project details, including plate IDs and project directory.
#' @return A list of mzML files for each plate, excluding files with "COND", "Blank", or "ISTDs" in their names.
#' @examples
#' \dontrun{
#' initialise_mzml_filelist(master_list)
#' }
initialise_mzml_filelist <- function(master_list) {
  mzml_filelist <- list()

  for (idx_plate in master_list$project_details$plateID) {
    mzml_filelist[[idx_plate]] <- list.files(
      file.path(
        master_list$project_details$project_dir,
        idx_plate,
        "data",
        "mzml"
      ),
      pattern = "\\.mzML$",
      full.names = FALSE,
      ignore.case = TRUE
    )
    if (identical(master_list$project_details$user_name, "ANPC")) {
      mzml_filelist[[idx_plate]] <- mzml_filelist[[idx_plate]][
        !grepl(ANPC_EXCLUDE_PATTERN, mzml_filelist[[idx_plate]], perl = TRUE)
      ]
    }
  }
  return(mzml_filelist)
}


#' process_plates
#'
#' This function processes mzML files for each plate in the master list, extracting relevant information and updating the master list.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param mzml_filelist A list of mzML files for each plate.
#' @return The updated `master_list` object with processed mzML data.
#' @examples
#' \dontrun{
#' process_plates(master_list, mzml_filelist)
#' }
process_plates <- function(master_list, mzml_filelist) {
  for (idx_plate in master_list$project_details$plateID) {
    message("Processing mzML files for plate: ", idx_plate)
    master_list$data[[idx_plate]]$mzR <- list()
    long_path <- file.path(master_list$project_details$project_dir,
                           idx_plate,
                           "data",
                           "mzml")

    with_short_junction(long_path, function(mzml_path) {
      for (idx_mzML in mzml_filelist[[idx_plate]]) {
        master_list$data[[idx_plate]]$mzR[[idx_mzML]] <<- list()
        mzR_obj <- NULL
        tryCatch({
          mzR_obj <- mzR::openMSfile(filename = file.path(mzml_path, idx_mzML))
          master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_header <<- mzR::chromatogramHeader(mzR_obj)
          master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_chromatogram <<- mzR::chromatograms(mzR_obj)
          run_info <- tryCatch(mzR::instrumentInfo(mzR_obj), error = function(e) list())
          master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_timestamp <<-
            if (!is.null(run_info$startTimeStamp)) run_info$startTimeStamp else NA_character_
        }, error = function(e) {
          warning("Skipping corrupt mzML file '", idx_mzML, "': ", conditionMessage(e))
          master_list$data[[idx_plate]]$mzR[[idx_mzML]] <<- NULL
        }, finally = {
          if (!is.null(mzR_obj)) try(mzR::close(mzR_obj), silent = TRUE)
        })
      }
    })

    # Use the first mzML with a non-empty timestamp rather than the
    # alphabetical first. Conditioning / blank / equilibration injections
    # at the start of a plate often have stripped or empty
    # startTimeStamp headers, which would otherwise produce NA_integer_
    # global_timestamp downstream and warn from extract_acquisition_year.
    plate_timestamp <- pick_first_valid_timestamp(master_list$data[[idx_plate]]$mzR,
                                                  mzml_filelist[[idx_plate]])
    master_list$data$global_timestamp[[idx_plate]] <- extract_acquisition_year(plate_timestamp)
    master_list$project_details$mzml_sample_list[[idx_plate]] <- update_sample_list(master_list, idx_plate)
    message("mzML import complete for plate: ", idx_plate,
            " (", length(mzml_filelist[[idx_plate]]), " files)")
  }
  return(master_list)
}


#' pick_first_valid_timestamp
#'
#' Returns the first non-empty \code{mzR_timestamp} from the imported mzR
#' entries, scanning files in the order given by \code{file_order}. Returns
#' \code{NA_character_} if no entry has a usable timestamp, in which case
#' \code{extract_acquisition_year} will warn and return \code{NA_integer_}.
#' @keywords internal
#' @param mzR_entries Named list of mzR import entries (typically
#'   \code{master_list$data[[plate]]$mzR}).
#' @param file_order Character vector of mzML filenames in the order to scan.
#' @return Character timestamp, or \code{NA_character_} if none found.
pick_first_valid_timestamp <- function(mzR_entries, file_order) {
  if (is.null(mzR_entries) || length(file_order) == 0) {
    return(NA_character_)
  }
  for (idx_mzML in file_order) {
    ts <- mzR_entries[[idx_mzML]]$mzR_timestamp
    if (!is.null(ts) && length(ts) == 1 && !is.na(ts) && nzchar(trimws(ts))) {
      return(ts)
    }
  }
  return(NA_character_)
}


#' extract_acquisition_year
#'
#' This function extracts the year from a timestamp string and converts it to a numeric value.
#' @keywords internal
#' @param timestamp A string representing the timestamp.
#' @return A numeric value representing the year extracted from the timestamp.
#' @examples
#' \dontrun{
#' extract_acquisition_year("2025-06-17T13:31:58Z")
#' }
extract_acquisition_year <- function(timestamp) {
  if (is.null(timestamp) || length(timestamp) == 0 ||
      is.na(timestamp) || !nzchar(trimws(timestamp))) {
    warning("extract_acquisition_year: timestamp is missing or empty; ",
            "returning NA_integer_ for global_timestamp.",
            call. = FALSE)
    return(NA_integer_)
  }
  # as.integer() emits "NAs introduced by coercion" for non-numeric strings;
  # preserve that warning so callers can detect unparseable timestamps.
  year <- as.integer(stringr::str_sub(timestamp, 1, 4))
  return(year)
}


#' update_sample_list
#'
#' This function updates the sample list for a given plate in the master list.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param idx_plate The index of the plate to update the sample list for.
#' @return The updated sample list for the specified plate.
#' @examples
#' \dontrun{
#' update_sample_list(master_list, idx_plate)
#' }
update_sample_list <- function(master_list, idx_plate) {
  if (is.null(master_list$project_details$mzml_sample_list[[idx_plate]])) {
    master_list$project_details$mzml_sample_list[[idx_plate]] <- character()
  }
  return(c(
    master_list$project_details$mzml_sample_list[[idx_plate]],
    names(master_list$data[[idx_plate]]$mzR)
  ))
}

#.----
#mzR Mrm FindR Functions-----

###Primary Function----
#' mzR_mrm_findR
#'
#' This function processes mzML files to find peak apex and boundaries using QC mzR data, updates the mrm guide, and provides peak boundary information.
#' @keywords internal
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param FUNC_OPTION_qc_type QC type used in the experiment passed internally.
#' @return A list containing updated mrm guide and peak boundary information.
#' @examples
#' \dontrun{
#' mzR_mrm_findR(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
#' }
mzR_mrm_findR <- function(FUNC_mzR,
                          FUNC_mrm_guide,
                          FUNC_OPTION_qc_type) {
  validate_mzR_parameters(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
  mzML_filelist <- get_mzML_filelist(FUNC_mzR)
  mzML_filelist_qc <- filter_mzML_filelist_qc(mzML_filelist, FUNC_OPTION_qc_type)
  FUNC_tibble <- process_files(FUNC_mzR, FUNC_mrm_guide, mzML_filelist_qc)
  FUNC_output <- create_output(FUNC_tibble, FUNC_mrm_guide, mzML_filelist)
  return(FUNC_output)
}

###Sub Functions----

#' validate_mzR_parameters
#'
#' This function validates parameters mzR_mrmfindR. If parameters fail validation script stops
#' @keywords internal
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param FUNC_OPTION_qc_type QC type used in the experiment (LTR, PQC, none) parsed internally.
#' @return NULL
#' @examples
#' \dontrun{
#' validate_mzR_parameters(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
#' }
validate_mzR_parameters <- function(FUNC_mzR,
                                    FUNC_mrm_guide,
                                    FUNC_OPTION_qc_type) {
  if (!is.list(FUNC_mzR)) {
    stop("validate_mzR_parameters: 'FUNC_mzR' must be a list. Got: ",
         paste(class(FUNC_mzR), collapse = ", "), call. = FALSE)
  }
  if (!is.data.frame(FUNC_mrm_guide)) {
    stop("validate_mzR_parameters: 'FUNC_mrm_guide' must be a data.frame. Got: ",
         paste(class(FUNC_mrm_guide), collapse = ", "), call. = FALSE)
  }
  if (!is.character(FUNC_OPTION_qc_type) || length(FUNC_OPTION_qc_type) != 1) {
    stop("validate_mzR_parameters: 'FUNC_OPTION_qc_type' must be a single character string. Got: ",
         paste(class(FUNC_OPTION_qc_type), collapse = ", "), call. = FALSE)
  }
}

#' get_mzML_filelist
#'
#' This function generates the mzML_filelist from FUNC_mzR
#' @keywords internal
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @return A list containing mzML file information from plate.
#' @examples
#' \dontrun{
#' get_mzML_filelist(FUNC_mzR)
#' }
get_mzML_filelist <- function(FUNC_mzR) {
  mzML_filelist <- NULL
  for (idx_plate in names(FUNC_mzR)) {
    mzML_filelist <- c(mzML_filelist, names(FUNC_mzR[[idx_plate]]))
  }
  return(mzML_filelist)
}

#' filter_mzML_filelist_qc
#'
#' This function filters the mzML_filelist for user selected quality control samples
#' @keywords internal
#' @param mzML_filelist A list containing mzML file information from plate.
#' @param FUNC_OPTION_qc_type QC type used in the experiment parsed internally.
#' @return A list containing quality control sample data.
#' @examples
#' \dontrun{
#' filter_mzML_filelist_qc(mzML_filelist,FUNC_OPTION_qc_type)
#' }
filter_mzML_filelist_qc <- function(mzML_filelist, FUNC_OPTION_qc_type) {
  # Use fixed = TRUE so qc_type is treated as a literal string (not a regex),
  # preventing metacharacters such as "+" or "." from causing unexpected matches.
  # Case-insensitive matching is achieved by lowercasing both sides.
  qc_idx <- grep(tolower(FUNC_OPTION_qc_type), tolower(mzML_filelist), fixed = TRUE)
  mzML_filelist_qc <- mzML_filelist[qc_idx]

  if (length(mzML_filelist_qc) == 0) {
    warning("No QC files found matching '", FUNC_OPTION_qc_type,
            "' in ", length(mzML_filelist), " mzML files. ",
            "Check that qc_type matches your filename convention.",
            call. = FALSE)
  } else {
    message("  Found ", length(mzML_filelist_qc), " QC file(s) matching '",
            FUNC_OPTION_qc_type, "'")
  }
  return(mzML_filelist_qc)
}

#' process_files
#'
#' This function process each mzml to produce a tibble of mrm data for all samples on plate.
#' @keywords internal
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param mzML_filelist_qc A list containing mzML file information from plate for only quality control samples.
#' @return A tibble containing mrm data for all samples on the plate.
#' @examples
#' \dontrun{
#' process_files(FUNC_mzR, FUNC_mrm_guide, mzML_filelist_qc)
#' }
process_files <- function(FUNC_mzR,
                          FUNC_mrm_guide,
                          mzML_filelist_qc) {
  FUNC_tibble <- list()

  for (idx_mzML in mzML_filelist_qc) {
    FUNC_tibble[[idx_mzML]] <- tibble::tibble()

    for (idx_plate in names(FUNC_mzR)) {
      if (sum(idx_mzML == names(FUNC_mzR[[idx_plate]])) == 1) {
        result <- tryCatch({
          process_mrm_transitions(FUNC_mzR, FUNC_mrm_guide, idx_plate, idx_mzML)
        }, error = function(e) {
          message(sprintf("Removing %s from optimisation due to lack of datapoints during aquisition: \n Error:%s ",
                          idx_mzML, e$message))
          return(NULL)
        })

        if (!is.null(result)) {
          FUNC_tibble[[idx_mzML]] <- result
        }
      }
    }
  }

  FUNC_tibble <- dplyr::bind_rows(FUNC_tibble)

  # Warn on silently-dropped rows. Previously "multiple match" (isobaric
  # lipids that fit the wider 0.05 Da / RT window) were filtered with no
  # feedback, so users could not see they had been dropped. See
  # REVIEW_REPORT BC-H3.
  if ("lipid_class" %in% colnames(FUNC_tibble)) {
    multi_match_rows <- FUNC_tibble[FUNC_tibble$lipid_class == "multiple match", , drop = FALSE]
    if (nrow(multi_match_rows) > 0) {
      per_transition <- paste(
        sprintf("precursor=%.4f -> product=%.4f",
                multi_match_rows$precursor_mz,
                multi_match_rows$product_mz),
        collapse = "; "
      )
      warning(
        "process_files: ", nrow(multi_match_rows),
        " chromatogram row(s) matched multiple lipids in the mrm_guide and were dropped. ",
        "This usually indicates isobaric lipids within the fallback tolerance. ",
        "Transitions affected: ", per_transition,
        call. = FALSE
      )
    }

    FUNC_tibble <- FUNC_tibble %>%
      dplyr::filter(lipid_class != "no match") %>%
      dplyr::filter(lipid_class != "multiple match")
  }

  return(FUNC_tibble)
}



#' process_mrm_transitions
#'
#' This function processes mrm_transition for each sample on a plate.
#' @keywords internal
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @return A tibble of mrm data for the specific idx_plate/idx_mzML.
#' @examples
#' \dontrun{
#' process_mrm_transitions(FUNC_mzR, FUNC_mrm_guide, idx_plate, idx_mzML)
#' }
process_mrm_transitions <- function(FUNC_mzR,
                                    FUNC_mrm_guide,
                                    idx_plate,
                                    idx_mzML) {
  header <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_header
  # Identify SRM/MRM chromatograms.
  # Primary: the ProteoWizard CV term "SRM chromatogram" on chromatogramType.
  # See REVIEW_REPORT BC-H5 — some pwiz versions leave TIC/BPC in the header
  # at index >= 2, so matching by type is the authoritative filter.
  mrm_indices <- integer(0)
  if ("chromatogramType" %in% colnames(header)) {
    mrm_indices <- which(
      grepl("SRM chromatogram|MRM chromatogram",
            header$chromatogramType, ignore.case = TRUE)
    )
  }

  # Secondary: gate on BOTH precursor AND product isolation window target m/z
  # being non-NA and > 0. Bruker mzMLs populate precursor but leave product
  # NA, which historically produced garbage indices. See REVIEW_REPORT BC-H4.
  if (length(mrm_indices) == 0 &&
      all(c("precursorIsolationWindowTargetMZ",
            "productIsolationWindowTargetMZ") %in% colnames(header))) {
    mrm_indices <- which(
      !is.na(header$precursorIsolationWindowTargetMZ) &
        header$precursorIsolationWindowTargetMZ > 0 &
        !is.na(header$productIsolationWindowTargetMZ) &
        header$productIsolationWindowTargetMZ > 0
    )
  }

  # Tertiary (legacy / last-resort): chromatogramIndex >= 2, then >=3.
  # This path is deprecated and will be removed in a future release once all
  # supported vendor converters reliably populate chromatogramType or
  # isolation-window m/z columns. If your mzML regularly reaches this branch,
  # please open an issue with a representative file so the primary/secondary
  # filters can be extended.
  if (length(mrm_indices) == 0) {
    if ("chromatogramIndex" %in% colnames(header)) {
      mrm_indices <- which(header$chromatogramIndex >= 2)
    }
    if (length(mrm_indices) == 0) {
      if (nrow(header) >= 3) {
        mrm_indices <- seq.int(from = 3, to = nrow(header))
        message("  Warning: Could not identify MRM chromatograms by type; ",
                "falling back to index >= 3 assumption.")
      } else {
        mrm_indices <- integer(0)
      }
    }
  }
  row_list <- vector("list", length(mrm_indices))
  row_idx <- 0L
  for (idx_mrm in mrm_indices) {
    if (nrow(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]) > 0) {
      precursor_mz <- header$precursorIsolationWindowTargetMZ[idx_mrm]
      product_mz <- header$productIsolationWindowTargetMZ[idx_mrm]
      baseline_value <- calculate_baseline(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
      peak_apex_idx <- find_peak_apex_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
      peak_start_idx <- find_peak_start_idx(FUNC_mzR,
                                            idx_plate,
                                            idx_mzML,
                                            idx_mrm,
                                            peak_apex_idx,
                                            baseline_value)
      peak_end_idx <- find_peak_end_idx(FUNC_mzR,
                                        idx_plate,
                                        idx_mzML,
                                        idx_mrm,
                                        peak_apex_idx,
                                        baseline_value)
      mzml_rt_apex <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime[peak_apex_idx] %>% round(2)
      mzml_rt_start <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime[peak_start_idx] %>% round(2)
      mzml_rt_end <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime[peak_end_idx] %>% round(2)
      lipid_info <- find_lipid_info(
        FUNC_mrm_guide,
        precursor_mz,
        product_mz,
        mzml_rt_apex,
        FUNC_mzR,
        idx_plate,
        idx_mzML,
        idx_mrm
      )
      row_idx <- row_idx + 1L
      row_list[[row_idx]] <- tibble::tibble(
        mzml = idx_mzML,
        lipid_class = lipid_info$class,
        lipid = lipid_info$name,
        precursor_mz = precursor_mz,
        product_mz = product_mz,
        peak_apex = mzml_rt_apex,
        peak_start = mzml_rt_start,
        peak_end = mzml_rt_end
      )
    }
  }
  FUNC_tibble <- dplyr::bind_rows(row_list[seq_len(row_idx)])
  return(FUNC_tibble)
}

#' calculate_baseline
#'
#' This function calculates baseline for a single transition.
#' @keywords internal
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @return A numeric value for baseline.
#' @examples
#' \dontrun{
#' calculate_baseline(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
#' }
calculate_baseline <- function(FUNC_mzR, idx_plate, idx_mzML, idx_mrm) {
  intensities <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][, 2]
  baseline_value <- stats::quantile(intensities, 0.1, na.rm = TRUE, names = FALSE)
  return(baseline_value)
}

#' find_peak_apex_idx
#'
#' This function finds peak apex for a single transition.
#' @keywords internal
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @return A numeric value for peak apex index.
#' @examples
#' \dontrun{
#' find_peak_apex_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
#' }
find_peak_apex_idx <- function(FUNC_mzR, idx_plate, idx_mzML, idx_mrm) {
  intensities <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][, 2]
  n <- length(intensities)
  # which.max returns the *first* index among ties, biasing toward the
  # rising edge. This is intentional: in MRM chromatograms, the left-most
  # of tied maxima is more likely to reflect the true apex than a flat
  # plateau tail. No smoothing is applied before argmax.
  peak_apex_idx <- which.max(intensities)
  # For chromatograms with fewer than 10 points, accept the raw max
  min_points_for_trim <- 10L
  # Trim 5 points from each edge to avoid selecting noise spikes at
  # chromatogram boundaries as the apex
  edge_trim <- 5L
  if (n < min_points_for_trim) return(peak_apex_idx)
  if (peak_apex_idx < edge_trim || peak_apex_idx > (n - edge_trim)) {
    peak_apex_idx <- which.max(intensities[edge_trim:(n - edge_trim)]) + (edge_trim - 1L)
  }
  return(peak_apex_idx)
}

#' find_peak_start_idx
#'
#' This function finds peak start for a single transition.
#' @keywords internal
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @param peak_apex_idx Index of peak apex passed from process_files function.
#' @param baseline_value Numeric value indicating transitions baseline. Passed from process_files function.
#' @return A numeric value for peak start index.
#' @examples
#' \dontrun{
#' find_peak_start_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm, peak_apex_idx, baseline_value)
#' }
find_peak_start_idx <- function(FUNC_mzR,
                                idx_plate,
                                idx_mzML,
                                idx_mrm,
                                peak_apex_idx,
                                baseline_value) {
  # N_START_CROSSINGS_FROM_APEX: take the crossing 3 positions before the last
  # baseline crossing prior to the apex. This avoids picking a noise dip
  # immediately before the peak rise as the start boundary; 3 was chosen
  # empirically on ANPC lipid data as the minimum that consistently captures the
  # foot of the peak without extending into the preceding trough.
  N_START_CROSSINGS_FROM_APEX <- 3L
  baseline_idx <- which((FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][, 2]) < baseline_value)
  before_apex <- baseline_idx[baseline_idx < peak_apex_idx]
  offset <- max(1L, length(before_apex) - N_START_CROSSINGS_FROM_APEX)
  peak_start_idx <- before_apex[offset]
  if (length(peak_start_idx) == 0 || is.na(peak_start_idx) || peak_start_idx < 1) {
    peak_start_idx <- 1
  }
  return(peak_start_idx)
}

#' find_peak_end_idx
#'
#' This function finds peak end for a single transition.
#' @keywords internal
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @param peak_apex_idx Index of peak apex passed from process_files function.
#' @param baseline_value Numeric value indicating transitions baseline. Passed from process_files function.
#' @return A numeric value for peak end index.
#' @examples
#' \dontrun{
#' find_peak_end_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm, peak_apex_idx, baseline_value)
#' }
find_peak_end_idx <- function(FUNC_mzR,
                              idx_plate,
                              idx_mzML,
                              idx_mrm,
                              peak_apex_idx,
                              baseline_value) {
  intensities <- FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]][, 2]
  baseline_idx <- which(intensities < baseline_value)
  # Select the 3rd baseline crossing after the apex to capture the full peak
  # tail and avoid truncating at noise dips immediately after the apex
  n_baseline_crossings <- 3L
  after_apex_crossings <- baseline_idx[baseline_idx > peak_apex_idx]
  peak_end_idx <- after_apex_crossings[n_baseline_crossings]
  if (is.na(peak_end_idx)) {
    # Fewer than n_baseline_crossings exist after the apex: use the last
    # available crossing rather than falling back to the full chromatogram end,
    # which would over-integrate. Log a notice so the caller can investigate.
    if (length(after_apex_crossings) > 0) {
      message("find_peak_end_idx: fewer than ", n_baseline_crossings,
              " baseline crossings after apex for idx_mzML=", idx_mzML,
              ", idx_mrm=", idx_mrm, ". Using last available crossing.")
      peak_end_idx <- after_apex_crossings[length(after_apex_crossings)]
    } else {
      peak_end_idx <- length(intensities)
    }
  }
  if (peak_end_idx > length(intensities)) {
    peak_end_idx <- length(intensities)
  }
  return(peak_end_idx)
}


#' find_lipid_info
#'
#' This function finds and matches lipids to mrm data.
#' @keywords internal
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param precursor_mz A numeric value for precursor mass to charge ratio.
#' @param product_mz A numeric value for product mass to charge ratio.
#' @param mzml_rt_apex A numeric value for retention time apex.
#' @param FUNC_mzR List from master_list containing mzR object for each sample, mzR_header, mzR_chromatogram parsed internally.
#' @param idx_plate A string naming the current plate ID being processed. Passed from process_files function.
#' @param idx_mzML A string naming the current mzML file being processed. Passed from process_files function.
#' @param idx_mrm A string identifying the current mrm. Passed from process_files function.
#' @return A list contain lipid class and lipid species information.
#' @examples
#' \dontrun{
#' find_lipid_info(FUNC_mrm_guide, precursor_mz, product_mz,
#'                 mzml_rt_apex, FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
#' }
find_lipid_info <- function(FUNC_mrm_guide,
                            precursor_mz,
                            product_mz,
                            mzml_rt_apex,
                            FUNC_mzR,
                            idx_plate,
                            idx_mzML,
                            idx_mrm,
                            strict_mz_tol = 0.01,
                            fallback_mz_tol = 0.05,
                            rt_tol = 0.1) {
  # Tolerances parameterised (see REVIEW_REPORT BC-H3). Defaults match the
  # historical hardcoded values so behaviour is unchanged for existing callers.
  # Guard against NA in guide m/z columns (e.g. after janitor::clean_names rename drift)
  valid_guide <- !is.na(FUNC_mrm_guide$precursor_mz) & !is.na(FUNC_mrm_guide$product_mz)
  lipid_idx <- which(
    valid_guide &
    abs(FUNC_mrm_guide$precursor_mz - precursor_mz) < strict_mz_tol &
      abs(FUNC_mrm_guide$product_mz - product_mz) < strict_mz_tol
  )
  if (length(lipid_idx) != 1) {
    lipid_idx <- which(
      valid_guide &
      FUNC_mrm_guide$precursor_mz > (precursor_mz - fallback_mz_tol) &
        FUNC_mrm_guide$precursor_mz < (precursor_mz + fallback_mz_tol) &
        FUNC_mrm_guide$product_mz > (product_mz - fallback_mz_tol) &
        FUNC_mrm_guide$product_mz < (product_mz + fallback_mz_tol) &
        FUNC_mrm_guide$explicit_retention_time > (min(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime) -
                                                    rt_tol) &
        FUNC_mrm_guide$explicit_retention_time < (max(FUNC_mzR[[idx_plate]][[idx_mzML]]$mzR_chromatogram[[idx_mrm]]$rtime) +
                                                    rt_tol)
    )
    if (length(lipid_idx) == 1) {
      message("  Note: Lipid matched using wider tolerance (", fallback_mz_tol,
              " Da) for m/z ",
              round(precursor_mz, 4), " -> ", round(product_mz, 4))
    }
  }
  if (length(lipid_idx) > 1) {
    lipid_class <- "multiple match"
    lipid <- "multiple match"
  } else if (length(lipid_idx) == 0) {
    lipid_class <- "no match"
    lipid <- "no match"
  } else if (length(lipid_idx) == 1) {
    lipid_class <- FUNC_mrm_guide$molecule_list_name[lipid_idx]
    lipid <- FUNC_mrm_guide$precursor_name[lipid_idx]
  }
  return(list(class = lipid_class, name = lipid))
}


#' create_output
#'
#' This function updated mrm_guide and peak_boundary_update for later use in PeakForgeR
#' @keywords internal
#' @param FUNC_tibble A tibble of mrm details per sample per lipid species.
#' @param FUNC_mrm_guide Tibble of mrm details parsed internally. See run mrm_template_guide for example.
#' @param mzML_filelist A list containing mzML file information from plate.
#' @return A list containing two tibbles for mrm_guide_updated and peak_boundary_update.
#' @examples
#' \dontrun{
#' create_output(FUNC_tibble, FUNC_mrm_guide, mzML_filelist)
#' }
create_output <- function(FUNC_tibble,
                          FUNC_mrm_guide,
                          mzML_filelist) {
  if (nrow(FUNC_tibble) == 0) {
    return(
      list(
        mrm_guide_updated = tibble::tibble(),
        peak_boundary_update = tibble::tibble()
      )
    )
  }

  unique_lipids <- unique(FUNC_tibble$lipid)
  guide_rows <- vector("list", length(unique_lipids))
  boundary_rows <- vector("list", length(unique_lipids))
  for (i in seq_along(unique_lipids)) {
    idx_lipid <- unique_lipids[i]
    lipid_data <- FUNC_tibble[FUNC_tibble$lipid == idx_lipid, ]
    guide_match <- FUNC_mrm_guide[FUNC_mrm_guide$precursor_name == idx_lipid, ]
    if (nrow(guide_match) > 1) {
      warning("create_output: precursor_name '", idx_lipid, "' matched ",
              nrow(guide_match), " rows in mrm_guide; using first match. ",
              "Check for duplicate entries across channels.",
              call. = FALSE)
      guide_match <- guide_match[1L, , drop = FALSE]
    }

    guide_rows[[i]] <- tibble::tibble(
      "Molecule List Name" = unique(lipid_data$lipid_class),
      "Precursor Name" = idx_lipid,
      "Precursor Mz" = guide_match$precursor_mz,
      "Precursor Charge" = guide_match$precursor_charge,
      "Product Mz" = guide_match$product_mz,
      "Product Charge" = guide_match$product_charge,
      "Explicit Retention Time" = stats::median(lipid_data$peak_apex),
      "Explicit Retention Time Window" = guide_match$explicit_retention_time_window,
      "Note" = guide_match$note
    )

    # Compute per-file peak bounds by joining on the mzml filename column.
    # Fall back to the global quantile for files absent from QC data — and
    # also when `lipid_data` has no `mzml` column at all (e.g. legacy
    # callers / fixtures that skip the per-file join), in which case every
    # file inherits the scalar global bound.
    global_min <- stats::quantile(lipid_data$peak_start, 0.25, na.rm = TRUE, names = FALSE)
    global_max <- stats::quantile(lipid_data$peak_end, 0.75, na.rm = TRUE, names = FALSE)
    has_mzml_col <- "mzml" %in% colnames(lipid_data)
    per_file_start <- vapply(mzML_filelist, function(fn) {
      if (!has_mzml_col) return(global_min)
      rows <- lipid_data[lipid_data$mzml == fn, ]
      if (nrow(rows) > 0) rows$peak_start[1L] else global_min
    }, numeric(1), USE.NAMES = FALSE)
    per_file_end <- vapply(mzML_filelist, function(fn) {
      if (!has_mzml_col) return(global_max)
      rows <- lipid_data[lipid_data$mzml == fn, ]
      if (nrow(rows) > 0) rows$peak_end[1L] else global_max
    }, numeric(1), USE.NAMES = FALSE)
    boundary_rows[[i]] <- tibble::tibble(
      "FileName" = mzML_filelist,
      "FullPeptideName" = idx_lipid,
      "MinStartTime" = per_file_start,
      "MaxEndTime" = per_file_end
    )
  }
  FUNC_output <- list()
  FUNC_output$mrm_guide_updated <- dplyr::bind_rows(guide_rows) %>% dplyr::arrange(`Precursor Name`)
  FUNC_output$peak_boundary_update <- dplyr::bind_rows(boundary_rows) %>% dplyr::arrange(`FullPeptideName`)
  return(FUNC_output)
}
