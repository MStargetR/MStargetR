#qcCheckR Data Preparation Functions ----
# Data import, preparation, sample type assignment, template loading
# Split from qcCheckR_Utils.R

##Transpose PeakForgeR Report Data Functions ----
###Primary Function ----

#' Transpose PeakForgeR Report Data
#'
#' This function transposes PeakForgeR report data for each plate in the master list.
#' It reshapes the data, cleans sample names, converts values to numeric, and stores the result.
#' @param master_list A list containing project details and data.
#' @keywords internal
#' @return The updated `master_list` object with transposed peak area data.
#'
#' @examples
#' \dontrun{
#' # master_list must first be built by qcCheckR_setup_project():
#' # master_list <- qcCheckR_setup_project(user_name, project_directory,
#' #                                        mrm_template_list, QC_sample_label,
#' #                                        sample_tags, mv_threshold)
#' master_list <- qcCheckR_transpose_data(master_list)
#' }
qcCheckR_transpose_data <- function(master_list) {
  validate_master_list(master_list)
  master_list$data$peakArea$transposed <- list()

  for (plate_id in master_list$project_details$plateIDs) {
    message("\nTransposing plate: ", plate_id)
    matching_name <- find_matching_report(master_list$data$PeakForgeRReport, plate_id)

    if (length(matching_name) == 1) {
      tryCatch({
        transposed <- transpose_plate_data(master_list$data$PeakForgeRReport[[matching_name]])
        master_list$data$peakArea$transposed[[plate_id]] <- transposed
        message("\nSuccessfully transposed plate: ", plate_id)
      }, error = function(e) {
        message("\nError transposing plate: ", plate_id)
        message("\nError message: ", e$message)
      })
    } else if (length(matching_name) > 1) {
      message("\nMultiple matching PeakForgeR reports found for plate: ",
              plate_id)
    } else {
      message("\nNo matching PeakForgeR report found for plate: ", plate_id)
    }
  }

  return(master_list)
}

###Sub Functions ----
#' Validate Master List Structure
#'
#' This function checks if the master list has the required structure and data.
#' @keywords internal
#' @param master_list The master list object to validate.
#' @return NULL if the structure is valid, otherwise throws an error.
validate_master_list <- function(master_list) {
  if (!is.list(master_list) ||
      !is.list(master_list$data$PeakForgeRReport)) {
    stop("Invalid master_list format.", call. = FALSE)
  }
}

#' Find Matching PeakForgeR Report Name
#'
#' This function finds the matching PeakForgeR report name based on the plate ID.
#' @keywords internal
#' @param report_list A list of PeakForgeR reports.
#' @param plate_id The ID of the plate to match.
#' @return The name of the matching report, or NULL if not found.
find_matching_report <- function(report_list, plate_id) {
  names(report_list)[grepl(plate_id, names(report_list))]
}

#' Transpose and Clean Plate Data
#'
#' This function transposes the plate data from the PeakForgeR report, reshaping it into a wide format.
#' It cleans the sample names by removing the file extension and converts area values to numeric.
#' @keywords internal
#' @param data The PeakForgeR report data frame for a specific plate.
#' @return A transposed tibble with sample names as rows and molecule names as columns.
transpose_plate_data <- function(data) {
  transposed <- data %>%
    tidyr::pivot_wider(
      id_cols = FileName,
      names_from = MoleculeName,
      values_from = Area,
      names_glue = "{MoleculeName}"
    ) %>%
    dplyr::rename(sample_name = FileName)

  transposed$sample_name <- sub("\\.mzML$", "", transposed$sample_name)
  transposed <- transposed %>%
    dplyr::mutate(dplyr::across(-sample_name, ~ {
      converted <- suppressWarnings(as.numeric(.x))
      n_bad <- sum(is.na(converted) & !is.na(.x))
      if (n_bad > 0) {
        warning(n_bad, " non-numeric value(s) in column '",
                dplyr::cur_column(), "' converted to NA", call. = FALSE)
      }
      converted
    }))
  transposed <- tibble::as_tibble(transposed)
  # Remove all-NA metabolite columns (keep sample_name)
  all_na_cols <- names(transposed)[sapply(transposed, function(col) all(is.na(col)))]
  all_na_cols <- setdiff(all_na_cols, "sample_name")
  if (length(all_na_cols) > 0) {
    transposed <- transposed %>% dplyr::select(-dplyr::all_of(all_na_cols))
  }

  return(transposed)
}

#.----
##Sort and QC Check Data Functions ----
### Primary Function ----
#' Sort and QC Check Data
#'
#' This function sorts the transposed peak area data by run order and performs QC checks.
#' It assigns sample types, validates QC coverage, and sets the appropriate QC type for the project.
#'
#' Before iterating plates, the cohort-wide date format is resolved once so
#' every plate uses the same calendar convention. mzML \code{startTimeStamp}
#' headers (ISO 8601, locale-invariant) are preferred per sample where
#' available; the \code{date_order} argument controls how to parse any
#' Skyline-exported \code{AcquiredTime} strings that fall back to the
#' string path.
#'
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param date_order One of \code{"auto"} (default), \code{"dmy"}, \code{"mdy"},
#'   or \code{"ymd"} / \code{"iso"}. Forwarded to \code{parse_sample_timestamp}
#'   for any sample where mzR \code{startTimeStamp} is unavailable. When
#'   \code{"auto"}, this function inspects the cohort and selects an
#'   unambiguous order; if every value is ambiguous and no plate-name
#'   \code{_YYYYMMDD$} hint can break the tie, it errors so the user must
#'   pick explicitly rather than silently get the wrong dates.
#' @return The updated `master_list` with sorted data and QC check results.
#'
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_sort_data(master_list)
#' }
qcCheckR_sort_data <- function(master_list,
                               date_order = c("auto", "dmy", "mdy", "ymd", "iso")) {
  if (missing(date_order)) {
    date_order <- "auto"
  } else {
    date_order <- match.arg(date_order)
  }
  if (date_order == "iso") date_order <- "ymd"

  effective_order <- if (identical(date_order, "auto")) {
    detect_cohort_date_order(master_list)
  } else {
    date_order
  }

  master_list$project_details$date_order <- effective_order
  master_list$project_details$run_orders <- list()
  master_list$data$peakArea$sorted <- list()

  for (plate_id in names(master_list$data$peakArea$transposed)) {
    report_list <- master_list$data$PeakForgeRReport
    if (is.list(report_list) && length(report_list) > 0) {
      matched_report_name <- find_matching_report(report_list, plate_id)
      if (length(matched_report_name) != 1) {
        message("\nCould not resolve unique PeakForgeR report for plate: ", plate_id, " (skipping sort)")
        next
      }
      report_data <- report_list[[matched_report_name]]
    } else {
      report_data <- report_list[[plate_id]]
    }
    mzR_entries <- master_list$data[[plate_id]]$mzR
    run_order <- extract_run_order(report_data, plate_id,
                                   mzR_entries = mzR_entries,
                                   date_order = effective_order)
    run_order <- assign_sample_type(master_list$project_details$sample_tags, run_order)
    validate_qc_types(run_order, master_list$project_details$sample_tags)
    sorted_data <- sort_and_filter_data(
      run_order,
      master_list$data$peakArea$transposed[[plate_id]],
      master_list$project_details$sample_tags
    )
    sorted_data$sample_ID <- extract_sample_id(sorted_data$sample_name)
    sorted_data <- sorted_data %>% dplyr::relocate(sample_ID, .after = sample_name)
    master_list$project_details$run_orders[[plate_id]] <- run_order
    master_list$data$peakArea$sorted[[plate_id]] <- sorted_data
    rm(run_order, sorted_data)
  }

  master_list <- assess_qc_coverage(master_list)
  master_list <- set_project_qc_type(master_list)
  master_list <- finalise_sorted_data(master_list)

  return(master_list)
}

###Sub Functions ----
#' Extract Run Order from PeakForgeR Report
#'
#' Builds the per-plate run order frame used downstream by qcCheckR. mzML
#' \code{startTimeStamp} headers (ISO 8601, locale-invariant) are preferred
#' on a per-sample basis when \code{mzR_entries} is supplied; Skyline's
#' \code{AcquiredTime} string is used as a fallback for any sample whose
#' mzML did not carry a usable header. The string-parse path honours
#' \code{date_order} so the locale-dependent dmy/mdy ambiguity is resolved
#' by the caller's cohort-level decision rather than by silent first-match
#' inside the parser.
#'
#' @keywords internal
#' @param report The PeakForgeR report data frame.
#' @param plate_id The ID of the plate to filter by.
#' @param mzR_entries Optional named list of mzR entries (typically
#'   \code{master_list$data[[plate_id]]$mzR}). Each element should carry an
#'   \code{mzR_timestamp} character scalar (an ISO 8601 timestamp from the
#'   mzML \code{startTimeStamp} header). Where present and non-empty, this
#'   value replaces the report's \code{AcquiredTime} for that sample.
#' @param date_order Forwarded to \code{parse_sample_timestamp} for the
#'   fallback path; see that function for accepted values.
#' @return A data frame containing the sample names, timestamps, and other relevant information.
extract_run_order <- function(report, plate_id, mzR_entries = NULL,
                              date_order = "auto") {
  extracted_data <- report %>%
    dplyr::select(dplyr::contains(c("FileName", "AcquiredTime"))) %>%
    dplyr::distinct(FileName, .keep_all = TRUE)

  # Build a per-FileName ISO 8601 lookup from any mzR entries the caller
  # supplied. mzML startTimeStamp is mandated ISO 8601 by the PSI-MS spec,
  # so it is locale-invariant and eliminates the dmy/mdy ambiguity that
  # bites Skyline's AcquiredTime string export.
  iso_lookup <- character(0)
  if (!is.null(mzR_entries) && length(mzR_entries) > 0) {
    for (mzml_name in names(mzR_entries)) {
      ts <- mzR_entries[[mzml_name]]$mzR_timestamp
      if (!is.null(ts) && length(ts) == 1 && !is.na(ts) &&
          nzchar(trimws(ts))) {
        iso_lookup[mzml_name] <- ts
      }
    }
  }

  raw_filename <- as.character(extracted_data$FileName)
  mzR_iso <- if (length(iso_lookup) > 0) {
    iso_lookup[raw_filename]
  } else {
    rep(NA_character_, length(raw_filename))
  }
  effective_ts <- ifelse(
    !is.na(mzR_iso) & nzchar(mzR_iso),
    mzR_iso,
    as.character(extracted_data$AcquiredTime)
  )

  n_total <- sum(!is.na(extracted_data$AcquiredTime) &
                   nzchar(as.character(extracted_data$AcquiredTime)))
  n_iso <- sum(!is.na(mzR_iso) & nzchar(mzR_iso))
  if (n_total > 0 && n_iso > 0 && n_iso < n_total) {
    message("Plate '", plate_id, "': using mzML startTimeStamp for ",
            n_iso, "/", n_total,
            " samples; falling back to AcquiredTime parsing for the ",
            "remainder (date_order='", date_order, "').")
  }

  extracted_data <- extracted_data %>%
    dplyr::mutate(FileName = sub("\\.mzML$", "", FileName)) %>%
    dplyr::rename(sample_name = FileName) %>%
    dplyr::mutate(
      sample_timestamp = parse_sample_timestamp(effective_ts,
                                                date_order = date_order)
    ) %>%
    dplyr::select(-dplyr::any_of("AcquiredTime")) %>%
    dplyr::filter(!is.na(sample_timestamp)) %>%
    dplyr::arrange(sample_timestamp)

  # Post-parse sanity check: large inter-sample gaps on a single plate
  # suggest a format-detection error. Mostly silent on the mzR-ISO path
  # because ISO is unambiguous; still useful for the AcquiredTime fallback.
  if (nrow(extracted_data) > 1) {
    sorted_ts <- sort(extracted_data$sample_timestamp, na.rm = TRUE)
    gaps <- diff(sorted_ts)
    suspicious <- which(abs(as.numeric(gaps, units = "days")) > 30)
    if (length(suspicious) > 0) {
      warning("Plate '", plate_id, "': ",
              length(suspicious), " timestamp gap(s) > 30 days detected. ",
              "This may indicate a date format mismatch.",
              call. = FALSE)
    }
  }

  extracted_data <- extracted_data %>%
    dplyr::mutate(
      sample_plate_id = plate_id,
      sample_plate_order = dplyr::row_number(),
      sample_matrix = dplyr::case_when(
        stringr::str_detect(sample_name, "(?i)_SER_") ~ "SER",
        stringr::str_detect(sample_name, "(?i)_PLA_") ~ "PLA",
        stringr::str_detect(sample_name, "(?i)_URI_") ~ "URI",
        TRUE ~ NA_character_
      )
    )

  return(extracted_data)
}

#' Extract a YYYYMMDD Date Hint From a Plate ID
#'
#' Many ANPC plate IDs end with an \code{_YYYYMMDD} suffix (e.g.
#' \code{..._BHASp06_20211004}) that names the acquisition date in an
#' unambiguous, locale-independent form. When the dmy/mdy heuristics tie
#' across an otherwise-ambiguous cohort, this hint is used to tip the
#' decision toward whichever calendar convention places the parsed dates
#' closest to the embedded date. The looser 6-digit \code{_DDMMYY} or
#' \code{_YYMMDD} suffixes are intentionally not matched, because in
#' practice they have been observed to be project codes rather than
#' acquisition dates.
#'
#' @keywords internal
#' @param plate_id A character scalar plate identifier.
#' @return A length-1 POSIXct (UTC) or \code{NA} if no usable hint is
#'   present.
extract_plate_date_hint <- function(plate_id) {
  if (!is.character(plate_id) || length(plate_id) != 1 ||
      is.na(plate_id)) {
    return(as.POSIXct(NA_character_, tz = "UTC"))
  }
  m <- regmatches(plate_id, regexpr("_\\d{8}$", plate_id))
  if (length(m) == 0) return(as.POSIXct(NA_character_, tz = "UTC"))
  ts <- sub("^_", "", m)
  year  <- suppressWarnings(as.integer(substr(ts, 1, 4)))
  month <- suppressWarnings(as.integer(substr(ts, 5, 6)))
  day   <- suppressWarnings(as.integer(substr(ts, 7, 8)))
  if (anyNA(c(year, month, day))) {
    return(as.POSIXct(NA_character_, tz = "UTC"))
  }
  if (year < 2000 || year > 2100 || month < 1 || month > 12 ||
      day < 1 || day > 31) {
    return(as.POSIXct(NA_character_, tz = "UTC"))
  }
  suppressWarnings(as.POSIXct(
    sprintf("%04d-%02d-%02dT12:00:00", year, month, day),
    format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
  ))
}

#' Detect Cohort-Wide Date Format for AcquiredTime Strings
#'
#' Skyline's \code{AcquiredTime} export uses the system locale of whoever
#' ran the export, so the same mzML files can produce DMY (UK), MDY (US),
#' ISO, or other formats on different machines. This helper resolves the
#' format once per cohort so every plate gets a consistent interpretation.
#'
#' Decision order:
#' \enumerate{
#'   \item If every non-empty \code{AcquiredTime} value parses as ISO 8601,
#'         return \code{"ymd"} (no further work needed).
#'   \item Otherwise count cohort-wide DMY vs MDY parse successes; any
#'         single value with day part \eqn{>12} locks DMY, any with
#'         month part \eqn{>12} locks MDY. The format with the higher
#'         parse count wins.
#'   \item If the counts are tied (every value is digit-ambiguous), use
#'         per-plate \code{_YYYYMMDD$} hints to vote between formats by
#'         computing which interpretation places the median parsed
#'         timestamp closer to the plate-name date.
#'   \item If still ambiguous and no hints exist, \code{stop()} so the
#'         caller is forced to supply \code{date_order} explicitly rather
#'         than silently receive the wrong dates.
#' }
#'
#' @keywords internal
#' @param master_list The qcCheckR master_list. Must have
#'   \code{data$PeakForgeRReport} populated.
#' @return One of \code{"dmy"}, \code{"mdy"}, or \code{"ymd"}. Never
#'   returns \code{"auto"} (the caller passed that intent in).
detect_cohort_date_order <- function(master_list) {
  report_list <- master_list$data$PeakForgeRReport
  if (!is.list(report_list) || length(report_list) == 0) {
    # No reports loaded -- nothing to disambiguate. Default to ymd so any
    # downstream ISO timestamps from mzR pass through unchanged.
    return("ymd")
  }

  plate_ids <- names(master_list$data$peakArea$transposed)
  if (length(plate_ids) == 0) plate_ids <- names(report_list)

  per_plate_ts <- list()
  for (pid in plate_ids) {
    matched <- find_matching_report(report_list, pid)
    if (length(matched) != 1) next
    raw <- report_list[[matched]]$AcquiredTime
    if (is.null(raw)) next
    raw <- as.character(raw)
    raw <- raw[!is.na(raw) & nzchar(raw)]
    if (length(raw) == 0) next
    per_plate_ts[[pid]] <- raw
  }

  all_ts <- unlist(per_plate_ts, use.names = FALSE)
  if (length(all_ts) == 0) return("ymd")

  iso_a <- suppressWarnings(as.POSIXct(all_ts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  iso_b <- suppressWarnings(as.POSIXct(all_ts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  iso_c <- suppressWarnings(as.POSIXct(all_ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (all(!is.na(iso_a) | !is.na(iso_b) | !is.na(iso_c))) {
    message("qcCheckR: AcquiredTime is ISO 8601 across the cohort; ",
            "using date_order='ymd'.")
    return("ymd")
  }

  parsed_dmy <- suppressWarnings(as.POSIXct(all_ts, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"))
  needs <- is.na(parsed_dmy)
  if (any(needs)) {
    parsed_dmy[needs] <- suppressWarnings(as.POSIXct(
      all_ts[needs], format = "%d/%m/%Y %H:%M", tz = "UTC"))
  }
  parsed_mdy <- suppressWarnings(as.POSIXct(all_ts, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"))
  needs <- is.na(parsed_mdy)
  if (any(needs)) {
    parsed_mdy[needs] <- suppressWarnings(as.POSIXct(
      all_ts[needs], format = "%m/%d/%Y %H:%M", tz = "UTC"))
  }

  n_dmy <- sum(!is.na(parsed_dmy))
  n_mdy <- sum(!is.na(parsed_mdy))

  if (n_dmy > 0 && n_mdy == 0) {
    message("qcCheckR: cohort AcquiredTime values resolved unambiguously ",
            "as day-first; using date_order='dmy'.")
    return("dmy")
  }
  if (n_mdy > 0 && n_dmy == 0) {
    message("qcCheckR: cohort AcquiredTime values resolved unambiguously ",
            "as month-first; using date_order='mdy'.")
    return("mdy")
  }
  if (n_dmy > n_mdy) {
    message("qcCheckR: cohort AcquiredTime values favour day-first (",
            n_dmy, " dmy vs ", n_mdy,
            " mdy successes); using date_order='dmy'.")
    return("dmy")
  }
  if (n_mdy > n_dmy) {
    message("qcCheckR: cohort AcquiredTime values favour month-first (",
            n_dmy, " dmy vs ", n_mdy,
            " mdy successes); using date_order='mdy'.")
    return("mdy")
  }

  # Tied -- use plate-name YYYYMMDD hints.
  votes_dmy <- 0L
  votes_mdy <- 0L
  for (pid in names(per_plate_ts)) {
    hint <- extract_plate_date_hint(pid)
    if (is.na(hint)) next
    ts_pid <- per_plate_ts[[pid]]
    p_dmy <- suppressWarnings(as.POSIXct(ts_pid, format = "%d/%m/%Y %H:%M:%S", tz = "UTC"))
    needs <- is.na(p_dmy)
    if (any(needs)) {
      p_dmy[needs] <- suppressWarnings(as.POSIXct(
        ts_pid[needs], format = "%d/%m/%Y %H:%M", tz = "UTC"))
    }
    p_mdy <- suppressWarnings(as.POSIXct(ts_pid, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"))
    needs <- is.na(p_mdy)
    if (any(needs)) {
      p_mdy[needs] <- suppressWarnings(as.POSIXct(
        ts_pid[needs], format = "%m/%d/%Y %H:%M", tz = "UTC"))
    }
    med_dmy <- if (any(!is.na(p_dmy))) stats::median(as.numeric(p_dmy), na.rm = TRUE) else NA_real_
    med_mdy <- if (any(!is.na(p_mdy))) stats::median(as.numeric(p_mdy), na.rm = TRUE) else NA_real_
    hint_sec <- as.numeric(hint)
    diff_dmy <- if (!is.na(med_dmy)) abs(med_dmy - hint_sec) else Inf
    diff_mdy <- if (!is.na(med_mdy)) abs(med_mdy - hint_sec) else Inf
    if (!is.finite(diff_dmy) && !is.finite(diff_mdy)) next
    if (diff_mdy < diff_dmy) votes_mdy <- votes_mdy + 1L
    else if (diff_dmy < diff_mdy) votes_dmy <- votes_dmy + 1L
  }

  if (votes_dmy == 0 && votes_mdy == 0) {
    stop("qcCheckR: could not unambiguously detect the date format for ",
         "AcquiredTime in this cohort. All slash-separated values are ",
         "<= 12 in both day and month positions, and no plate IDs end ",
         "with a _YYYYMMDD suffix to use as a hint. Re-run with ",
         "date_order = 'dmy' or 'mdy' explicitly, or re-run after ",
         "loading mzML files so ISO 8601 startTimeStamps can be used ",
         "(see PeakForgeR).",
         call. = FALSE)
  }
  if (votes_mdy > votes_dmy) {
    message("qcCheckR: cohort AcquiredTime values are digit-ambiguous; ",
            "plate-name _YYYYMMDD hints (", votes_mdy, " mdy vs ",
            votes_dmy, " dmy) select month-first. ",
            "Using date_order='mdy'.")
    return("mdy")
  }
  if (votes_dmy > votes_mdy) {
    message("qcCheckR: cohort AcquiredTime values are digit-ambiguous; ",
            "plate-name _YYYYMMDD hints (", votes_dmy, " dmy vs ",
            votes_mdy, " mdy) select day-first. ",
            "Using date_order='dmy'.")
    return("dmy")
  }
  stop("qcCheckR: could not unambiguously detect the date format for ",
       "AcquiredTime in this cohort. Plate-name _YYYYMMDD hints tied ",
       "(", votes_dmy, " dmy vs ", votes_mdy, " mdy). Re-run with ",
       "date_order = 'dmy' or 'mdy' explicitly.",
       call. = FALSE)
}


#' Assign Sample Type
#'
#' This function assigns sample types based on the sample names and user-defined QC types.
#' @keywords internal
#' @param sample_tags A character vector passed from master_list$project_details$sample_tags, containing QC types.
#' @param run_order A data frame containing the run order information.
assign_sample_type <- function(sample_tags, run_order) {
  if (is.null(sample_tags) || length(sample_tags) < 1) {
    stop("sample_tags must be a character vector with at least one QC type.", call. = FALSE)
  }


  # Build sample_type programmatically instead of eval(parse())
  sample_type_vec <- rep("sample", nrow(run_order))
  # Sort tags longest-first so specific tags (e.g. "VLTR_PS") match before
  # shorter substrings (e.g. "VLTR" or "LTR") that could greedily overlap.
  # Treat any non-alphabetic character (underscore, hyphen, space, dot) as a
  # tag delimiter so filenames like "...PLATE_3-PLASMA LTR_19.mzML" are
  # classified correctly. Letter-bounded checks still prevent partial matches
  # (e.g. "ltr" inside "vltr", "pqc" inside "vpqc"). Mirrors the SIL pattern
  # in PeakForgeR_Utils.R::read_mrm_guides.
  sorted_tags <- sample_tags[order(nchar(sample_tags), decreasing = TRUE)]
  for (tag in sorted_tags) {
    pattern <- paste0("(?:^|[^A-Za-z])", tag, "(?=[^A-Za-z]|$)")
    matches <- stringr::str_detect(
      run_order$sample_name,
      stringr::regex(pattern, ignore_case = TRUE)
    )
    # Only assign to samples not yet classified (first match wins)
    unclassified <- sample_type_vec == "sample"
    sample_type_vec[matches & unclassified] <- tag
  }

  data <- run_order %>%
    dplyr::mutate(sample_type = sample_type_vec)

  return(data)
}

#' Validate QC Types
#'
#' This function validates the sample types in the run order against the provided sample tags.
#' @keywords internal
#' @param run_order A data frame containing the run order information.
#' @param sample_tags A character vector of QC types to validate against.
#' @return NULL if validation passes, otherwise throws an error.
validate_qc_types <- function(run_order, sample_tags) {
  invalid <- run_order %>%
    dplyr::filter(sample_type != "sample" & !(sample_type %in% sample_tags))
  if (nrow(invalid) > 0) {
    message(paste(utils::capture.output(print(invalid)), collapse = "\n"))
    stop("Invalid QC sample_type detected.", call. = FALSE)
  }
  if (all(run_order$sample_type == "sample")) {
    plate_id <- unique(run_order$sample_plate_id)
    plate_label <- if (length(plate_id) == 1) plate_id else paste(plate_id, collapse = ", ")
    sample_preview <- utils::head(run_order$sample_name, 10)
    stop(
      "No QC types were identified for plate '", plate_label, "'.\n",
      "  sample_tags used: ", paste(sample_tags, collapse = ", "), "\n",
      "  First filenames in this plate (none matched any tag as a ",
      "delimiter-bounded substring):\n    ",
      paste(sample_preview, collapse = "\n    "),
      call. = FALSE
    )
  }
}

#' Sort and Filter Data
#'
#' This function sorts the run order data and filters it based on sample types.
#' @keywords internal
#' @param run_order A data frame containing the run order information.
#' @param transposed_data A data frame containing the transposed peak area data.
#' @param sample_tags A character vector of sample tags to filter by.
#' @return A data frame containing the sorted and filtered data.
sort_and_filter_data <- function(run_order, transposed_data, sample_tags) {
  data <- run_order %>%
    dplyr::left_join(transposed_data, by = "sample_name") %>%
    dplyr::arrange(sample_timestamp) %>%
    dplyr::mutate(sample_run_index = dplyr::row_number())

  return(data)
}

#' extract_sample_id
#'
#' This function pulls sampleID from file name.
#' @keywords internal
#' @param filenames sample_name column of .data
#' @return sample_ID column with unique sample identifiers
extract_sample_id <- function(filenames) {
  if (length(filenames) == 1)
    return(filenames)
  # Tokenise using _, -, .
  token_lists <- strsplit(filenames, "[-_.]")

  # Flatten all tokens to count frequency
  all_tokens <- unlist(token_lists)
  token_freq <- table(all_tokens)

  # Identify tokens that appear in all filenames
  common_tokens <- names(token_freq[token_freq >= length(filenames)])

  # Remove common tokens from each filename
  sample_ids <- lapply(token_lists, function(tokens) {
    unique_tokens <- tokens[!tokens %in% common_tokens]
    if (length(unique_tokens) == 0) return(paste(tokens, collapse = "_"))
    paste(unique_tokens, collapse = "_")
  })

  return(unlist(sample_ids))
}

#' Assess QC Coverage
#'
#' This function assesses the QC coverage for each plate in the master list.
#' It checks the ratio of QC samples to total samples and determines if the QC passed or failed.
#' @keywords internal
#' @param master_list A list containing project details and sorted peak area data.
#' @return The updated master list with QC coverage results.
assess_qc_coverage <- function(master_list) {
  master_list$project_details$qc_passed <- list()
  master_list$project_details$global_qc_pass <- list()

  for (plate_id in names(master_list$data$peakArea$sorted)) {
    plate_data <- master_list$data$peakArea$sorted[[plate_id]]
    qc_types <- plate_data %>%
      dplyr::filter(sample_type != "sample") %>%
      dplyr::distinct(sample_type) %>%
      dplyr::pull()

    QC_MIN_RATIO <- 8 / 125  # 6.4% minimum QC-to-total sample ratio
    for (qc in qc_types) {
      total <- nrow(plate_data)
      count <- sum(tolower(plate_data$sample_type) == tolower(qc))
      ratio <- count / total
      status <- if (ratio < QC_MIN_RATIO || count < 2)
        "fail"
      else
        "pass"
      master_list$project_details$qc_passed[[plate_id]][[qc]] <- status
      master_list$project_details$global_qc_pass[[qc]] <- "pass"
    }
  }

  for (plate_id in names(master_list$data$peakArea$sorted)) {
    for (qc in names(master_list$project_details$qc_passed[[plate_id]])) {
      if (master_list$project_details$qc_passed[[plate_id]][[qc]] == "fail") {
        master_list$project_details$global_qc_pass[[qc]] <- "fail"
      }
    }
  }
  message(paste(utils::capture.output(print(master_list$project_details$global_qc_pass)), collapse = "\n"))
  message(paste(utils::capture.output(print(master_list$project_details$qc_passed)), collapse = "\n"))
  return(master_list)
}

#' Set Project QC Type
#'
#' This function sets the QC type for the project based on user-specified QC types and global QC pass status.
#' @keywords internal
#' @param master_list A list containing project details and QC pass status.
#' @return The updated master list with the project QC type set.
set_project_qc_type <- function(master_list) {
  user_qc <- master_list$project_details$qc_type
  global_pass <- master_list$project_details$global_qc_pass

  if (!is.null(global_pass[[user_qc]]) &&
      global_pass[[user_qc]] == "pass") {
    message("qcCheckeR has set QC to user-specified type: ", user_qc)
    master_list$project_details$qc_type <- user_qc
  } else {
    warning("User-specified QC type ", user_qc, " did not pass QC checks.")
    passed_qcs <- names(global_pass)[global_pass == "pass"]
    if (length(passed_qcs) > 0) {
      qc_counts <- sapply(passed_qcs, function(qc) {
        sum(sapply(master_list$project_details$qc_passed, function(plate) {
          if (!is.null(plate[[qc]]) && plate[[qc]] == "pass")
            1
          else
            0
        }))
      })
      best_qc <- names(which.max(qc_counts))
      master_list$project_details$qc_type <- best_qc
      warning("qcCheckeR is switching QC type from '", user_qc, "' to '", best_qc,
              "'. Downstream RSD filtering and sample-flag semantics will reflect '",
              best_qc, "' rather than your original choice.",
              immediate. = TRUE, call. = FALSE)
    } else {
      stop(
        "No QC types passed. Stopping script.
            \n Please check sample_tags and filenames are correct."
      )
    }
  }

  return(master_list)
}

#' Finalise Sorted Data
#'
#' This function finalises the sorted data by adding sample type factors, reversing the order of sample types, and setting the sample data source.
#'
#' In addition to the legacy two-value `sample_type` column (`"qc"` / `"sample"`),
#' a new `sample_class` column is populated with three mutually-exclusive
#' values:
#' \itemize{
#'   \item \code{"qc"} — the chosen pooled QC (project-level `qc_type`).
#'   \item \code{"sample"} — biological study samples.
#'   \item \code{"other"} — blanks, SIL/IS injections, conditioning samples,
#'     and non-chosen QCs (any `sample_tags` value that is not the chosen
#'     `qc_type`). These must never be treated as biological samples for
#'     QC-vs-sample statistics (medians, drift models, RSD, etc.).
#' }
#'
#' Downstream code should prefer `sample_class` for QC-vs-sample splits.
#' `sample_type` is kept populated the old way for backward compatibility
#' with existing exports and downstream consumers.
#' @keywords internal
#' @param master_list A list containing project details and sorted peak area data.
#' @return The updated master list with finalised sorted data, including the
#'   additive `sample_class` column described above.
finalise_sorted_data <- function(master_list) {
  for (plate_id in names(master_list$data$peakArea$sorted)) {
    sorted <- master_list$data$peakArea$sorted[[plate_id]]
    qc_type <- master_list$project_details$qc_type

    tag_levels <- unique(c("sample", master_list$project_details$sample_tags))
    sorted <- sorted %>%
      dplyr::mutate(
        sample_type_factor = factor(sample_type, levels = tag_levels, ordered = TRUE),
        sample_type_factor_rev = factor(
          sample_type_factor,
          levels = rev(levels(sample_type_factor)),
          ordered = TRUE
        ),
        # 3-way class: chosen QC = "qc", biological "sample" = "sample",
        # everything else (blanks, SIL, non-chosen QCs, conditioning) = "other".
        # Kept additive to preserve backward compatibility of sample_type.
        sample_class = dplyr::case_when(
          tolower(sample_type) == tolower(qc_type) ~ "qc",
          tolower(sample_type) == "sample" ~ "sample",
          TRUE ~ "other"
        ),
        sample_type = ifelse(tolower(sample_type) == tolower(qc_type), "qc", "sample"),
        sample_data_source = ".peakArea"
      ) %>%
      dplyr::relocate(sample_type_factor,
               sample_type_factor_rev,
               sample_class,
               sample_data_source,
               .after = sample_type)


    master_list$data$peakArea$sorted[[plate_id]] <- sorted
  }

  all_samples <- dplyr::bind_rows(master_list$data$peakArea$sorted) %>%
    dplyr::arrange(sample_timestamp) %>%
    dplyr::mutate(sample_run_index = dplyr::row_number()) %>%
    dplyr::relocate(sample_run_index, .before = sample_name)

  # Preserve chronological plate order: split() uses factor/alphabetic order;
  # re-index by first appearance in the arranged data to keep time order.
  plate_order <- unique(all_samples$sample_plate_id)
  split_list <- split(all_samples, all_samples$sample_plate_id)
  master_list$data$peakArea$sorted <- split_list[plate_order]


  # Drop all-NA columns, but always keep exact metadata column names so that
  # (a) a sample_ID column that is all-NA is preserved and (b) metabolite names
  # that happen to contain the substring "sample" are not incorrectly retained.
  .metadata_cols <- c(
    "sample_run_index", "sample_name", "sample_type", "sample_type_factor",
    "sample_type_factor_rev", "sample_class", "sample_plate_id",
    "sample_timestamp", "sample_data_source"
  )
  master_list$data$peakArea$sorted <- lapply(
    master_list$data$peakArea$sorted,
    function(df) {
      keep <- names(df)[names(df) %in% .metadata_cols |
                          !vapply(df, function(col) all(is.na(col)), logical(1))]
      df[, keep, drop = FALSE]
    }
  )



  return(master_list)
}

#.----
##Impute Missing Data ----
###Primary Function ----
#' Impute Missing Data
#'
#' This function imputes missing and zero values in the `master_list` data using the minimum intensity of each feature in the batch divided by 2.
#' It handles infinite and NaN values, applies imputation, and merges metadata back into the result.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with imputed peak area data.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_impute_data(master_list)
#' }
qcCheckR_impute_data <- function(master_list) {
  master_list$data$peakArea$imputed <- list()

  for (plate_id in names(master_list$data$peakArea$sorted)) {
    message("  Imputing missing values for plate: ", plate_id)
    sorted_data <- master_list$data$peakArea$sorted[[plate_id]]
    impute_matrix <- prepare_imputation_matrix(sorted_data)
    imputed <- apply_lgw_imputation(impute_matrix)
    imputed <- merge_metadata(sorted_data, imputed)
    master_list$data$peakArea$imputed[[plate_id]] <- imputed
  }

  message("  Imputation complete for ",
          length(master_list$data$peakArea$imputed), " plate(s).")
  return(master_list)
}

###Sub Functions ----
#' Prepare Imputation Matrix
#'
#' This function prepares the imputation matrix from the sorted peak area data.
#' It removes sample name columns, converts the data to a matrix, and replaces problematic values (zeros, infinite, NaN).
#' @keywords internal
#' @param data A tibble containing the sorted peak area data.
#' @return A tibble ready for imputation, with problematic values replaced.
prepare_imputation_matrix <- function(data) {
  rownames(data) <- NULL
  # Drop sample_* metadata before the numeric column scan. Some metadata
  # columns are numeric (sample_run_index, sample_plate_order) and would
  # otherwise be carried into the imputation matrix, then collide on the
  # subsequent left_join in merge_metadata() and get suffixed as .x/.y --
  # silently destroying sample_run_index for every downstream consumer
  # (statTarget pheno build, RSD tables, etc.).
  prepped_data <- data %>%
    tibble::column_to_rownames("sample_name") %>%
    dplyr::select(-dplyr::starts_with("sample")) %>%
    dplyr::select(tidyselect::where(is.numeric)) %>%
    replace_problematic_values()
  return(prepped_data)
}

#' Replace Problematic Values
#'
#' This function replaces zeros, infinite, and NaN values in a matrix with NA.
#' @keywords internal
#' @param mat A numeric matrix.
#' @return A matrix with zeros replaced by NA, and infinite and NaN values also replaced by NA.
replace_problematic_values <- function(mat) {
  mat <- as.matrix(mat)
  mat[mat == 0] <- NA
  mat[is.infinite(mat)] <- NA
  mat[is.nan(mat)] <- NA
  return(mat)
}

#' LGW Impute
#'
#' This function performs imputation on a tibble by replacing zero values with half the minimum non-zero value in each column.
#' It calculates the minimum non-zero value for each column, divides it by 2, and replaces zero values with this calculated value.
#' @keywords internal
#' @param x A tibble containing numeric data.
#' @return A tibble with zero values replaced by half the minimum non-zero value in each column.
lgw_impute <- function(x) {
  # Validate parameter
  if (!tibble::is_tibble(x)) {
    x <- tibble::as_tibble(x)
  }

  if (!all(sapply(x, is.numeric))) {
    x <- x %>% dplyr::mutate(dplyr::across(
      tidyselect::where(is.character) &
        !matches("sample_name"),
      ~ suppressWarnings(as.numeric(.))
    ))

  }

  # Convert zeros to NA once, before calculating min/2 values.
  # This is intentional: replace_problematic_values() performs the same
  # conversion upstream when lgw_impute is called via apply_lgw_imputation,
  # but lgw_impute also handles the standalone case where zeros have not yet
  # been pre-processed.
  x <- x %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(., . == 0, NA)))

  # Calculate min/2 values for each column
  min_half_values <- purrr::map(.x = x, .f = ~ {
    non_zero_values <- .x[.x > 0]
    if (length(non_zero_values) == 0 ||
        all(is.na(non_zero_values))) {
      return(NA)
    }

    min(non_zero_values, na.rm = TRUE) / 2
  })

  all_zero_cols <- names(min_half_values)[sapply(min_half_values, is.na)]
  if (length(all_zero_cols) > 0) {
    message("  Note: ", length(all_zero_cols),
            " metabolite(s) have all-zero/all-NA values and cannot be imputed; ",
            "dropping columns: ",
            paste(head(all_zero_cols, 5), collapse = ", "),
            if (length(all_zero_cols) > 5) "..." else "")
    # Drop all-NA / all-zero columns rather than leaving them as all-NA
    # (downstream RSD and filter code would otherwise mix NA with min/2).
    x <- x %>% dplyr::select(-dplyr::all_of(all_zero_cols))
    min_half_values <- min_half_values[setdiff(names(min_half_values), all_zero_cols)]
  }

  # Replace NAs with min/2 values (zeros were already converted to NA by
  # replace_problematic_values() before this function is called)
  x %>%
    tidyr::replace_na(replace = min_half_values)
}

#' Apply LGW Imputation
#'
#' A wrapper to apply lgw_impute to the dataframe
#' @keywords internal
#' @param mat dataframe or matrix for imputation to be applied
apply_lgw_imputation <- function(mat) {
  df <- as.data.frame(mat)
  sample_names <- rownames(df)
  imputed <- lgw_impute(df)
  imputed <- tibble::as_tibble(imputed)
  imputed$sample_name <- sample_names
  imputed <- dplyr::relocate(imputed, sample_name)
  imputed %>%
    dplyr::mutate(dplyr::across(-sample_name, ~ ifelse(is.infinite(.), 1, .)))
}

#'Merge_Metadata
#'
#' This function merges metadata from the original data with the imputed data.
#' It selects sample name columns from the original data and performs a left join with the imputed data.
#' It also adds a column indicating the data source of the imputed data.
#' @keywords internal
#' @param original_data A tibble containing the original peak area data with sample names.
#' @param imputed_data A tibble containing the imputed peak area data with sample names.
#' @return A tibble with merged metadata and imputed data, including a sample data source column.
merge_metadata <- function(original_data, imputed_data) {
  metadata <- dplyr::select(original_data, dplyr::contains("sample"))
  merged <- dplyr::left_join(metadata, imputed_data, by = "sample_name")
  merged$sample_data_source <- ".peakAreaImputed"
  return(merged)
}

#.----
##Calculate Response and Concentration ----
###Primary Function ----
#' Calculate Response and Concentration
#'
#' This function calculates the response ratio and concentration for each sample in the `master_list` data.
#' It uses SIL internal standards and template guides to compute values for both sorted and imputed data.
#' @keywords internal
#' @param master_list A list containing project details, peak area data, and SIL templates.
#' @return The updated `master_list` with calculated response and concentration data.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_calculate_response_concentration(master_list)
#' }
qcCheckR_calculate_response_concentration <- function(master_list) {
  master_list$data$response <- list()
  master_list$data$concentration <- list()

  batches <- names(master_list$data$peakArea$imputed)
  message("  Calculating response ratios and concentrations for ",
          length(batches), " plate(s)...")

  for (plate_id in batches) {
    template_version <- master_list$project_details$plate_method_versions[[plate_id]]
    master_list$project_details$is_ver <- template_version
    master_list$templates[["Plate SIL version"]][[plate_id]] <- template_version

    for (data_type in c("sorted", "imputed")) {
      master_list <- calculate_plate_response_concentration(master_list, plate_id, data_type, template_version)
    }
  }

  master_list <- harmonise_lipid_columns(master_list)
  message("  Response and concentration calculation complete.")
  return(master_list)
}

###Sub Functions ----

#' Calculate Response and Concentration for a Plate
#'
#' This function calculates the response and concentration for a specific plate in the master list.
#' It retrieves the peak area data, identifies SIL columns, and processes each SIL target to compute response ratios and concentrations.
#' @keywords internal
#' @param master_list A list containing project details, peak area data, and SIL templates.
#' @param plate_id The ID of the plate to process.
#' @param data_type The type of data to process (either "sorted" or "imputed").
#' @param template_version The version of the template to use for processing.
#' @return The updated master list with calculated response and concentration data for the specified plate.
calculate_plate_response_concentration <- function(master_list,
                                                   plate_id,
                                                   data_type,
                                                   template_version) {
  data <- master_list$data$peakArea[[data_type]][[plate_id]]
  sil_notes <- master_list$templates$mrm_guides[[template_version]]$SIL_guide$Note
  # Swap intersect args so the result follows sil_notes order (stable) rather
  # than data column order (data-dependent).
  sil_cols <- dplyr::intersect(sil_notes, colnames(dplyr::select(data, dplyr::contains("SIL"))))

  # Reorder columns: non-SIL first, then SIL
  data <- dplyr::bind_cols(dplyr::select(data, -dplyr::contains("SIL")), dplyr::select(data, dplyr::all_of(sil_cols)))
  master_list$data$peakArea[[data_type]][[plate_id]] <- data

  # Initialise response and concentration lists
  master_list$data$response[[data_type]][[plate_id]] <- dplyr::select(data, dplyr::contains("sample"))
  master_list$data$concentration[[data_type]][[plate_id]] <- dplyr::select(data, dplyr::contains("sample"))

  for (sil in sil_cols) {
    master_list <- process_sil_target(master_list, plate_id, data_type, template_version, sil)
  }

  # Tag data sources
  master_list$data$response[[data_type]][[plate_id]]$sample_data_source <- paste0(".response.", data_type)
  master_list$data$concentration[[data_type]][[plate_id]]$sample_data_source <- paste0("concentration.", data_type)

  return(master_list)
}

#' Process SIL Target
#'
#' This function processes a specific SIL target by calculating the response ratio and concentration for each sample.
#' It retrieves the precursor names from the SIL guide, calculates the response ratio by dividing the peak area by the SIL value, and computes the concentration using the concentration factor from the template.
#' @keywords internal
#' @param master_list A list containing project details, peak area data, and SIL templates.
#' @param plate_id The ID of the plate to process.
#' @param data_type The type of data to process (either "sorted" or "imputed").
#' @param template_version The version of the template to use for processing.
#' @param sil The name of the SIL target to process.
#' @return The updated master list with calculated response and concentration data for the specified SIL target.
process_sil_target <- function(master_list,
                               plate_id,
                               data_type,
                               template_version,
                               sil) {
  sil_guide <- master_list$templates$mrm_guides[[template_version]]$SIL_guide
  precursors <- sil_guide %>% dplyr::filter(Note == sil) %>% dplyr::pull(`Precursor Name`)


  if (length(precursors) == 0)
    return(master_list)

  data <- master_list$data$peakArea[[data_type]][[plate_id]]
  sil_values <- data[[sil]]
  # Guard against duplicate sample_name values (e.g. re-injections) before
  # column_to_rownames, which would otherwise fail with a cryptic error and
  # silently corrupt downstream joins.
  if (anyDuplicated(data$sample_name)) {
    dups <- data$sample_name[duplicated(data$sample_name)]
    stop(sprintf(
      "Duplicate sample_name values detected (%d duplicates: %s). Re-injections must be disambiguated.",
      sum(duplicated(data$sample_name)),
      paste(head(dups, 5), collapse = ", ")
    ), call. = FALSE)
  }
  target_data <- dplyr::select(data, sample_name, dplyr::any_of(precursors)) %>%
                  tibble::column_to_rownames("sample_name")

  if (ncol(target_data) == 0)
    return(master_list)

  # Calculate response
  response <- as.matrix(target_data / sil_values)
  sil_zero_mask <- is.infinite(response)
  sil_na_mask <- is.na(response) & !is.na(as.matrix(target_data))
  n_sil_failures <- sum(sil_zero_mask | sil_na_mask)

  if (n_sil_failures > 0) {
    affected_samples <- rownames(target_data)[rowSums(sil_zero_mask | sil_na_mask) > 0]
    message("  SIL '", sil, "': ", n_sil_failures,
            " response value(s) set to NA due to zero/missing SIL ",
            "(", length(affected_samples), " sample(s) affected: ",
            paste(head(affected_samples, 3), collapse = ", "),
            if (length(affected_samples) > 3) "..." else "", ")")
  }

  response[is.na(response) | is.infinite(response)] <- NA
  response_df <- as.data.frame(response) %>% tibble::rownames_to_column("sample_name")

  master_list$data$response[[data_type]][[plate_id]] <- dplyr::left_join(master_list$data$response[[data_type]][[plate_id]], response_df, by = "sample_name")

  # Calculate concentration
  conc_factor <- master_list$templates$mrm_guides[[template_version]]$conc_guide %>%
    dplyr::filter(SIL_name == sil) %>%
    dplyr::pull(concentration_factor)


  if (length(conc_factor) == 1) {
    concentration <- as.matrix(response * conc_factor)
    concentration[is.na(concentration) |
                    is.infinite(concentration)] <- NA
    concentration_df <- as.data.frame(concentration) %>% tibble::rownames_to_column("sample_name")

    master_list$data$concentration[[data_type]][[plate_id]] <- dplyr::left_join(master_list$data$concentration[[data_type]][[plate_id]], concentration_df, by = "sample_name")
  }

  return(master_list)
}

#' Harmonise Lipid Columns
#'
#' This function harmonises the lipid columns across response and concentration data types in the master list.
#' It ensures that all lipid subtypes have the same columns by selecting only the common lipids across all plates.
#' @keywords internal
#' @param master_list A list containing project details and response/concentration data.
#' @return The updated master list with harmonised lipid columns.
harmonise_lipid_columns <- function(master_list) {
  for (data_type in c("response", "concentration")) {
    for (subtype in names(master_list$data[[data_type]])) {
      plates <- master_list$data[[data_type]][[subtype]]
      if (length(plates) == 0) next
      # Use union rather than intersect so that metabolites missing from one
      # plate are kept (filled with NA) on all other plates, rather than
      # being silently dropped project-wide.
      all_cols <- unique(unlist(lapply(plates, colnames), use.names = FALSE))
      if (is.null(all_cols) || length(all_cols) == 0) next
      # Put sample_* metadata FIRST, then lipid columns sorted alphabetically.
      # Without this, alphabetical sort of the union pushes lipid names
      # (CE_14_0, PC_36_2, ...) to the front and metadata to the right edge,
      # so head()/print() output makes the data look like it has no metadata.
      meta_cols  <- sort(grep("^sample", all_cols, value = TRUE))
      lipid_cols <- sort(setdiff(all_cols, meta_cols))
      ordered_cols <- c(meta_cols, lipid_cols)
      for (plate_id in names(plates)) {
        df <- plates[[plate_id]]
        n_rows <- nrow(df)
        missing_cols <- setdiff(ordered_cols, colnames(df))
        if (length(missing_cols) > 0) {
          # Preserve the original row count: an empty input frame must stay
          # empty (0 rows) rather than gain a spurious 1-row NA value.
          df[missing_cols] <- lapply(
            missing_cols,
            function(x) if (n_rows == 0) logical(0) else rep(NA, n_rows)
          )
        }
        master_list$data[[data_type]][[subtype]][[plate_id]] <- df[, ordered_cols, drop = FALSE]
      }
    }
  }
  return(master_list)
}
