#qcCheckR Filtering Functions ----
# QC filtering, RSD calculation, SIL flagging, metabolite filtering
# Split from qcCheckR_Utils.R

#' Resolve the configured instrumental LOD threshold
#'
#' Returns \code{master_list$project_details$lod_threshold} when it is a single
#' valid numeric, otherwise falls back to \code{DEFAULT_LOD_THRESHOLD}. Mirrors
#' the \code{sil_mv_threshold} / \code{rsd_threshold} fallback idiom used
#' elsewhere so every below-LOD count uses the same threshold.
#' @keywords internal
#' @param master_list The master list object.
#' @return A single numeric LOD threshold (peak area).
resolve_lod_threshold <- function(master_list) {
  lod <- master_list$project_details$lod_threshold
  if (is.null(lod) || !is.numeric(lod) || length(lod) != 1L || is.na(lod)) {
    lod <- DEFAULT_LOD_THRESHOLD
  }
  lod
}

## Set QC Type for Filtering ----
###Primary Function ----
#' Set QC Type for Filtering
#'
#' Determines and sets the QC type for filtering based on the global QC pass status in the `master_list`.
#' If no viable QC type is found, the function stops execution and prints a detailed error message.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with the QC type set and filters initialized.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_set_qc(master_list)
#' }
qcCheckR_set_qc <- function(master_list) {
  qc_type <- determine_qc_type(master_list)

  if (qc_type == "unknown") {
    stop_with_qc_error(
      project_name = master_list$project_details$project_name,
      global_qc_pass = master_list$project_details$global_qc_pass,
      plate_qc_passed = master_list$project_details$qc_passed
    )
  } else {
    message(paste(utils::capture.output(print(master_list$project_details$global_qc_pass)), collapse = "\n"))
    message(paste(utils::capture.output(print(master_list$project_details$qc_passed)), collapse = "\n"))
    notify_qc_type(qc_type)
  }

  master_list$project_details$qc_type <- qc_type
  master_list$filters <- list()

  return(master_list)
}

###Sub Functions ----

#' Determine QC Type
#'
#' This function determines the QC type based on the global QC pass status.
#' It checks if there are multiple QC types that have passed.
#' Assesses if a secondary QC sample is available such as a pooled Quality controls.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return A string indicating the QC type ("pqc", "ltr", or "unknown").
determine_qc_type <- function(master_list) {
  global_qc_pass <- master_list$project_details$global_qc_pass
  passed_qc <- names(global_qc_pass[global_qc_pass == "pass"])
  user_supplied_qc <- master_list$project_details$qc_type

  if (is.null(passed_qc) || length(passed_qc) == 0) {
    message("No viable QC type found for filtering. Please check your QC_sample_label is correct.")
    return("unknown")

  } else if (length(passed_qc) == 1) {
    message(passed_qc[1], " is the only viable QC.")
    return(passed_qc[1])

  } else if (length(passed_qc) > 1) {
    if (user_supplied_qc %in% passed_qc) {
      message("Multiple valid QC types found. Reverting to default user choice.")
      return(master_list$project_details$qc_type)
    } else {
      # Return "unknown" here per the function's documented contract and
      # let the caller (qcCheckR_set_qc) emit the authoritative "STOPPING
      # SCRIPT" error via stop_with_qc_error(), which surfaces both the
      # global QC pass status and per-plate QC assessment. An earlier
      # remediation replaced this return with an inline stop() that lost
      # that diagnostic context.
      message(
        "Multiple QC types found, but user-supplied QC ('", user_supplied_qc,
        "') is not among them. Valid QC types: ",
        paste(passed_qc, collapse = ", "),
        ". Please supply a valid QC_sample_label and rerun."
      )
      return("unknown")
    }
  }

}




#' Notify QC Type
#'
#' This function prints a message indicating the QC type that has been set for filtering.
#' @keywords internal
#' @param qc_type A string indicating the QC type (e.g. "pqc" or "ltr").
#' @return NULL
notify_qc_type <- function(qc_type) {
  msg <- paste("qcCheckeR has set filtering QC to:", qc_type)
  message(msg, "\n")
}

#' Stop with QC Error
#'
#' This function stops the script execution and prints a detailed error message if no viable QC type is found.
#' It includes information about the global QC pass status and plate QC assessment.
#' @keywords internal
#' @param project_name A string containing the project name.
#' @param global_qc_pass A list containing the global QC pass status.
#' @param plate_qc_passed A list containing the plate QC assessment.
#' @return Stops the script execution with an error message.
stop_with_qc_error <- function(project_name,
                               global_qc_pass,
                               plate_qc_passed) {
  global_qc_pass_str <- utils::capture.output(str(global_qc_pass))
  plate_qc_passed_str <- utils::capture.output(str(plate_qc_passed))

  error_message <- paste0(
    "STOPPING SCRIPT\n",
    "There are no viable qc samples for RSD filtering: ",
    project_name,
    ".\n",
    "Please refer to the details below:\n",
    "Global QC Assessment:\n",
    paste(global_qc_pass_str, collapse = "\n"),
    "\n",
    "Plate QC Assessment:\n",
    paste(plate_qc_passed_str, collapse = "\n")
  )

  stop(error_message)
}


#. ----
## Sample Filter ----
###Primary Function ----
#' Sample Filter
#'
#' Flags samples based on missing values and summed signal intensity in the `master_list` data.
#' Applies thresholds to identify low-quality samples and aggregates results across plates.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with sample filter flags and failed sample lists.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_sample_filter(master_list)
#' }
qcCheckR_sample_filter <- function(master_list) {
  master_list$filters$samples.missingValues <- list()
  master_list$filters$failed_samples <- list()
  lod <- resolve_lod_threshold(master_list)

  for (idx_batch in names(master_list$data$peakArea$sorted)) {
    sample_data <- master_list$data$peakArea$sorted[[idx_batch]]
    sample_meta <- sample_data %>%
      dplyr::select(sample_run_index,
             sample_name,
             sample_plate_id,
             sample_type_factor)

    #Extract lipid data
    lipid_data <- sample_data %>%
      dplyr::select(!dplyr::contains("sample")) %>%
      dplyr::select(!dplyr::contains("SIL")) %>%
      as.matrix()
    # Extract SIL data
    sil_data <- sample_data %>%
      dplyr::select(!dplyr::contains("sample")) %>%
      dplyr::select(dplyr::contains("SIL")) %>%
      as.matrix()

    #Flags for missing values and signal intensity
    flags <- list(
      #Summed signal intensities
      summed.lipid.signal = rowSums(lipid_data, na.rm = TRUE),
      summed.SIL.Int.Std.signal = rowSums(sil_data, na.rm = TRUE),
      #Missing /zero values below the LOD threshold. Explicitly require non-NA
      # so that all-NA features are not counted as passing the LOD threshold.
      missing.lipid = rowSums(!is.na(lipid_data) & lipid_data < lod),
      missing.SIL = rowSums(!is.na(sil_data) & sil_data < lod),
      #NA values
      na.lipid = rowSums(is.na(lipid_data), na.rm = TRUE),
      na.SIL = rowSums(is.na(sil_data), na.rm = TRUE),
      #NaN values
      nan.lipid = rowSums(is.nan(lipid_data), na.rm = TRUE),
      nan.SIL = rowSums(is.nan(sil_data), na.rm = TRUE),
      #Infinite values
      inf.lipid = rowSums(is.infinite(lipid_data), na.rm = TRUE),
      inf.SIL = rowSums(is.infinite(sil_data), na.rm = TRUE)
    )

    #Join metadata with flags
    flags <- cbind(sample_meta, flags)

    # Use only missing.* and na.* to avoid double-counting: is.nan() implies
    # is.na(), so nan.* values are already captured in na.*. inf.* values are
    # similarly captured by is.na() after replace_problematic_values().
    flags$totalMissingValues.lipid <- rowSums(flags[, c("missing.lipid", "na.lipid"), drop = FALSE])
    flags$totalMissingValues.SIL <- rowSums(flags[, c("missing.SIL", "na.SIL"), drop = FALSE])

    # Relative threshold only: flags samples below 20% of the plate median
    # summed signal. If the entire plate has low signal (e.g. a failed plate),
    # the median is also low and no samples will be flagged. This is a known
    # limitation; an absolute intensity floor is not applied here because the
    # appropriate floor is instrument- and method-specific.
    flags$sample.lipid.intensity.flag <- as.integer(flags$summed.lipid.signal < stats::median(flags$summed.lipid.signal) * 0.20)
    flags$sample.SIL.Int.Std.intensity.flag <- as.integer(
      flags$summed.SIL.Int.Std.signal < stats::median(flags$summed.SIL.Int.Std.signal) * 0.33
    )

    lipid_threshold <- ncol(lipid_data) * (master_list$project_details$mv_sample_threshold / 100)
    sil_threshold <- ncol(sil_data) * 0.33

    flags$sample.missing.value.flag <- as.integer(
      flags$totalMissingValues.lipid > lipid_threshold |
        flags$totalMissingValues.SIL > sil_threshold
    )

    flags$sample.flag <- as.integer(rowSums(as.matrix(flags[c(
      "sample.lipid.intensity.flag",
      "sample.SIL.Int.Std.intensity.flag",
      "sample.missing.value.flag"
    )]), na.rm = TRUE) > 0)

    flags$failed_samples <- ifelse(flags$sample.flag == 1, flags$sample_name, NA)

    master_list$filters$samples.missingValues[[idx_batch]] <- as.data.frame(flags)
    master_list$filters$failed_samples[[idx_batch]] <- flags$failed_samples
  }

  master_list$filters$samples.missingValues <- do.call(rbind, master_list$filters$samples.missingValues)
  master_list$filters$failed_samples <- master_list$filters$samples.missingValues %>%
                                        dplyr::filter(!is.na(failed_samples)) %>%
                                        dplyr::pull(sample_name)

  n_failed <- length(master_list$filters$failed_samples)
  n_total <- nrow(master_list$filters$samples.missingValues)
  message("  Sample filter: ", n_failed, "/", n_total,
          " samples flagged for removal.")

  return(master_list)
}

#.----
##SIL Internal Standard Filter ----
###Primary Function ----
#' SIL Internal Standard Filter
#'
#' Filters SIL internal standards based on missing values in the `master_list` data.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with SIL internal standard filter flags.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_sil_IntStd_filter(master_list)
#' }
qcCheckR_sil_IntStd_filter <- function(master_list) {
  master_list$filters$sil.intStd.missingValues <- list()

  plate_names <- names(master_list$data$peakArea$sorted)

  # Require every plate to have a resolved template version. Without this
  # guard, downstream calculate_sil_flags_per_plate silently attaches NULL
  # for missing plates, initialise_sil_summary still provides the column,
  # and the aggregate check in calculate_sil_flags_per_version no longer
  # fires masking a fully-missing templates list.
  plate_versions <- master_list$templates$`Plate SIL version`
  missing_versions <- setdiff(plate_names, names(plate_versions))
  if (length(missing_versions) > 0L || length(plate_versions) == 0L) {
    stop(
      "Missing required column: template_version. ",
      "No Plate SIL version mapping for plate(s): ",
      paste(if (length(missing_versions)) missing_versions else plate_names,
            collapse = ", "),
      ". Populate master_list$templates$`Plate SIL version`.",
      call. = FALSE
    )
  }

  sil_flag_list <- vector("list", length(plate_names))
  names(sil_flag_list) <- plate_names

  for (idx_batch in plate_names) {
    sil_flags <- calculate_sil_flags_per_plate(master_list, idx_batch)
    master_list$filters$sil.intStd.missingValues[[idx_batch]] <- sil_flags
    sil_flag_list[[idx_batch]] <- sil_flags
  }

  master_list$filters$sil.intStd.missingValues$summary <- dplyr::bind_rows(
    c(list(initialise_sil_summary()), sil_flag_list)
  )

  master_list <- calculate_sil_flags_per_version(master_list)

  master_list$filters$failed_sil.intStds <- unique(unlist(master_list$filters$failed_sil.intStds, use.names = FALSE))

  message("  SIL filter: ", length(master_list$filters$failed_sil.intStds),
          " SIL internal standard(s) flagged for removal.")

  return(master_list)
}


### Sub Functions ----
#' Initialise SIL Summary Data Frame
#'
#' This function initializes an empty data frame to store SIL summary statistics.
#' It includes columns for lipid names, template versions, plate IDs, and various flags related to SIL internal standards.
#' @keywords internal
#' @return An empty data frame with specified columns for SIL summary statistics.
initialise_sil_summary <- function() {
  data.frame(
    lipid = character(),
    template_version = character(),
    plateID = character(),
    peakArea_below_LOD = numeric(),
    naValues = numeric(),
    nanValues = numeric(),
    infValues = numeric(),
    totalMissingValues = numeric(),
    flag_SIL_intStd_Plate = numeric(),
    stringsAsFactors = FALSE
  )
}

#' Calculate SIL Flags per Plate
#'
#' This function calculates flags for SIL internal standards on a per-plate basis.
#' It computes the number of peak areas below a threshold, counts missing values, and flags plates with excessive missing values.
#' @keywords internal
#' @importFrom rlang .env .data
#' @param master_list A list containing project details and data.
#' @param idx_batch The index of the batch (plate) to process.
#' @return A tibble containing SIL flags for each lipid, including counts of peak areas below a threshold, missing values, and flags for excessive missing values.
calculate_sil_flags_per_plate <- function(master_list, idx_batch) {
  lod <- resolve_lod_threshold(master_list)
  sil_names <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    dplyr::select(dplyr::contains("SIL")) %>%
    names()

  valid_sample_names <- master_list$filters$samples.missingValues %>%
    dplyr::filter(.data$sample_plate_id == idx_batch, .data$sample.flag == 0) %>%
    dplyr::pull(.data$sample_name)

  sil_matrix <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    dplyr::filter(.data$sample_name %in% .env$valid_sample_names) %>%
    dplyr::select(dplyr::contains("SIL")) %>%
    as.matrix()

  flags <- tibble::tibble(
    lipid = sil_names,
    # Require non-NA: NA must not be counted as "below LOD pass".
    peakArea_below_LOD = colSums(!is.na(sil_matrix) & sil_matrix < lod),
    naValues = colSums(is.na(sil_matrix), na.rm = TRUE),
    nanValues = colSums(is.nan(sil_matrix), na.rm = TRUE),
    infValues = colSums(is.infinite(sil_matrix), na.rm = TRUE)
  )

  flags$totalMissingValues <- rowSums(
    flags[, c("peakArea_below_LOD", "naValues", "nanValues", "infValues")],
    na.rm = TRUE
  )

  #Flag if SIL is missing in greater than the configured threshold (default 5%) of samples
  valid_sample_count <- nrow(sil_matrix)
  sil_mv_threshold <- master_list$project_details$sil_mv_threshold
  if (is.null(sil_mv_threshold) || !is.numeric(sil_mv_threshold)) sil_mv_threshold <- 0.05
  flags$flag_SIL_intStd_Plate <- as.integer(flags$totalMissingValues > (valid_sample_count * sil_mv_threshold))

  flags$template_version <- master_list$templates$`Plate SIL version`[[idx_batch]]
  flags$plateID <- idx_batch

  return(flags)
}

#' Calculate SIL Flags per Version
#'
#' This function calculates flags for SIL internal standards across different
#' template versions. It aggregates SIL flags from all plates for each version,
#' counts missing values, and flags versions with excessive missing values.
#' @keywords internal
#' @importFrom rlang .env .data
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with SIL flags calculated for each version.
calculate_sil_flags_per_version <- function(master_list) {
  lod <- resolve_lod_threshold(master_list)
  master_list$filters$sil.intStd.missingValues$PROJECT.flag.SIL.intStd <- list()
  master_list$filters$failed_sil.intStds <- list()

  if (!"template_version" %in% colnames(master_list$filters$sil.intStd.missingValues$summary)) {
    stop("Missing required column: template_version")
  }

  for (version in unique(master_list$filters$sil.intStd.missingValues$summary$template_version)) {
    version_data <- master_list$filters$sil.intStd.missingValues$summary %>%
      dplyr::filter(template_version == version)

    lipid_list <- unique(version_data$lipid)
    plate_list <- unique(version_data$plateID)

    valid_sil_samples <- master_list$filters$samples.missingValues %>%
      dplyr::filter(.data$sample.flag == 0,
                    .data$sample_plate_id %in% plate_list) %>%
      dplyr::pull(.data$sample_name)

    # Bind only the current version's plates. Using bind_rows() across all
    # plates would union column names, so a SIL measured only on a different
    # version's plate would leak in here as an all-NA column for this version.
    sil_bound_data <- dplyr::bind_rows(master_list$data$peakArea$sorted[plate_list]) %>%
      dplyr::filter(.data$sample_name %in% .env$valid_sil_samples)

    if (nrow(sil_bound_data) == 0) next

    # Capture the full set of SIL columns present in the bound data BEFORE
    # the all-NA filter, so we can re-insert any that get dropped because
    # they are entirely NA project-wide. Sourcing this from the data (not
    # the SIL_guide) is critical: the guide can list SILs that were never
    # measured in this project, and re-inserting those would flag phantom
    # failures that don't appear in any plate-level summary.
    sil_columns <- sil_bound_data %>%
      dplyr::select(dplyr::contains("SIL")) %>%
      colnames()

    sil_matrix <- sil_bound_data %>%
      dplyr::select(dplyr::all_of(sil_columns)) %>%
      dplyr::select(tidyselect::where(~ !all(is.na(.)))) %>%
      as.matrix()

    # Defined here (not after the missing-SIL re-insertion below) because the
    # re-insertion block references valid_sample_count to set naValues /
    # totalMissingValues for entirely-NA SILs.
    valid_sample_count <- nrow(sil_matrix)

    version_flags <- tibble::tibble(
      lipid = colnames(sil_matrix),
      # Require non-NA: NA must not be counted as "below LOD pass".
      peakArea_below_LOD = colSums(!is.na(sil_matrix) & sil_matrix < lod),
      naValues = colSums(is.na(sil_matrix), na.rm = TRUE),
      nanValues = colSums(is.nan(sil_matrix), na.rm = TRUE),
      infValues = colSums(is.infinite(sil_matrix), na.rm = TRUE)
    )

    version_flags$totalMissingValues <- rowSums(version_flags %>% dplyr::select(-lipid), na.rm = TRUE)

    # Re-insert SILs that were present in the bound data but dropped by the
    # where(~ !all(is.na(.))) selector above (i.e. entirely NA project-wide).
    # These must be flagged as failed because they have 100% missing values.
    missing_sils <- setdiff(sil_columns, version_flags$lipid)
    if (length(missing_sils) > 0) {
      missing_rows <- tibble::tibble(
        lipid = missing_sils,
        peakArea_below_LOD = 0,
        naValues = valid_sample_count,
        nanValues = 0,
        infValues = 0,
        totalMissingValues = as.numeric(valid_sample_count)
      )
      version_flags <- dplyr::bind_rows(version_flags, missing_rows)
    }

    master_list$filters$sil.intStd.missingValues$allPlates[[version]] <- version_flags

    master_list$filters$sil.intStd.missingValues$PROJECT.flag.SIL.intStd[[version]] <- as.integer(version_flags$totalMissingValues > (valid_sample_count * 0.05))

    master_list$filters$failed_sil.intStds[[version]] <- version_flags$lipid[master_list$filters$sil.intStd.missingValues$PROJECT.flag.SIL.intStd[[version]] == 1]
  }

  return(master_list)
}


# . ----

## Lipid Filter ----
###Primary Function ----
#' Lipid Filter
#'
#' Filters lipids based on missing values across plates and template versions.
#' Flags lipids with more than 50% missing values and compiles a list of failed lipids.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with lipid filter flags and failed lipid list.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_lipid_filter(master_list)
#' }
qcCheckR_lipid_filter <- function(master_list) {
  master_list <- initialise_lipid_filter(master_list)

  for (idx_batch in names(master_list$data$peakArea$sorted)) {
    lipid_data <- get_lipid_data(master_list, idx_batch)
    lipid_flags <- calculate_lipid_flags(master_list, idx_batch, lipid_data)

    master_list$filters$lipid.missingValues[[idx_batch]] <- lipid_flags
    master_list$filters$lipid.missingValues$summary <- rbind(master_list$filters$lipid.missingValues$summary,
                                                             lipid_flags)
  }

  master_list <- process_lipid_versions(master_list)

  master_list$filters$failed_lipids <- unique(unlist(master_list$filters$failed_lipids, use.names = FALSE))

  message("  Lipid filter: ", length(master_list$filters$failed_lipids),
          " lipid feature(s) flagged for removal.")

  return(master_list)
}

### Sub Functions ----

#' Initialise lipid filter structure
#'
#' This function initialses the structure for lipid filtering in the `master_list`.
#' It creates a list to store lipid missing values, a summary data frame, and lists for project flags and failed lipids.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with initialised lipid filter structure.
initialise_lipid_filter <- function(master_list) {
  master_list$filters$lipid.missingValues <- list()
  master_list$filters$lipid.missingValues$summary <- data.frame(
    lipid = character(),
    silFilter.flag.Lipid = numeric(),
    peakArea_below_LOD = numeric(),
    naValues = numeric(),
    nanValues = numeric(),
    infValues = numeric(),
    totalMissingValues = numeric(),
    flag.Lipid.Plate = numeric(),
    template_version = character(),
    plateID = character(),
    stringsAsFactors = FALSE
  )
  master_list$filters$lipid.missingValues$PROJECT.flag.lipid <- list()
  master_list$filters$failed_lipids <- list()

  return(master_list)
}

#' Extract lipid matrix for a batch
#'
#' This function extracts the lipid data matrix for a specific batch from the `master_list`.
#' It filters out samples with missing values and selects relevant columns.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param idx_batch The index of the batch (plate) to process.

get_lipid_data <- function(master_list, idx_batch) {
  valid_samples <- master_list$filters$samples.missingValues %>%
    dplyr::filter(.data$sample.flag == 0) %>%
    dplyr::pull(.data$sample_name)

  lipid_data <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    dplyr::filter(.data$sample_name %in% valid_samples) %>%
    dplyr::select(!dplyr::contains("sample") & !dplyr::contains("SIL")) %>%
    as.matrix()

  return(lipid_data)
}


#' Calculate lipid flags for a batch
#'
#' This function calculates flags for lipids based on their signal intensity and missing values in a specific batch.
#' It checks for SIL internal standards, counts peak areas below a threshold, and flags plates with excessive missing values.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param idx_batch The index of the batch (plate) to process.
#' @param lipid_matrix A matrix containing lipid data for the batch.
#' @return A tibble containing lipid flags, including counts of peak areas below a threshold, missing values, and flags for excessive missing values.
calculate_lipid_flags <- function(master_list, idx_batch, lipid_matrix) {

  lod <- resolve_lod_threshold(master_list)
  lipid_names <- colnames(lipid_matrix)
  SIL_version <- master_list$templates$`Plate SIL version`[[idx_batch]]
  failed_sil <- master_list$filters$failed_sil.intStds
  stopifnot(
    "failed_sil.intStds must be a character vector; ensure qcCheckR_sil_IntStd_filter ran before qcCheckR_lipid_filter" =
      is.character(failed_sil)
  )

  if (ncol(lipid_matrix) == 0) {
    return(tibble::tibble(
      lipid = character(),
      silFilter.flag.Lipid = numeric(),
      peakArea_below_LOD = numeric(),
      naValues = numeric(),
      nanValues = numeric(),
      infValues = numeric(),
      totalMissingValues = numeric(),
      flag.Lipid.Plate = numeric(),
      template_version = SIL_version,
      plateID = idx_batch
    )[0, ])
  }

  if (is.null(SIL_version)) {
    stop("Missing template version for batch: ", idx_batch)
  }

  if (is.null(master_list$templates$mrm_guides[[SIL_version]]$SIL_guide)) {
    sil_flag <- rep(0, length(lipid_names))
  } else {
    sil_flag <- as.integer(
      lipid_names %in% dplyr::filter(
        master_list$templates$mrm_guides[[SIL_version]]$SIL_guide,
        Note %in% failed_sil
      )[["Precursor Name"]]
    )
  }

  lipid_flags <- tibble::tibble(
    lipid = lipid_names,
    silFilter.flag.Lipid = sil_flag,
    # Require non-NA: NA must not be counted as "below LOD pass".
    peakArea_below_LOD = colSums(!is.na(lipid_matrix) & lipid_matrix < lod),
    naValues = colSums(is.na(lipid_matrix), na.rm = TRUE),
    nanValues = colSums(is.nan(lipid_matrix), na.rm = TRUE),
    infValues = colSums(is.infinite(lipid_matrix), na.rm = TRUE),
    template_version = SIL_version,
    plateID = idx_batch
  )
  count_cols <- c("peakArea_below_LOD", "naValues", "nanValues", "infValues")
  lipid_flags$totalMissingValues <- rowSums(
    lipid_flags[, count_cols, drop = FALSE],
    na.rm = TRUE
  )
  lipid_flags$flag.Lipid.Plate <- as.integer(lipid_flags$totalMissingValues > (
    nrow(lipid_matrix) * (master_list$project_details$mv_sample_threshold / 100)
  ) |
    lipid_flags$silFilter.flag.Lipid == 1)

  return(lipid_flags)
}

#' Process lipid flags across template versions
#'
#' This function processes lipid flags across different template versions.
#' It aggregates lipid flags from all plates for each version, counts missing values, and flags versions with excessive missing values.
#' @keywords internal
#' @importFrom rlang .env .data
#' @param master_list A list containing project details and data.
process_lipid_versions <- function(master_list) {
  lod <- resolve_lod_threshold(master_list)
  for (version in unique(master_list$filters$lipid.missingValues$summary$template_version)) {
    version_data <- master_list$filters$lipid.missingValues$summary %>%
      dplyr::filter(template_version == version)

    lipid_list <- unique(version_data$lipid)
    plate_list <- unique(version_data$plateID)

    valid_sample_names <- master_list$filters$samples.missingValues %>%
      dplyr::filter(.data$sample.flag == 0) %>%
      dplyr::pull(.data$sample_name)

    bound_data <- master_list$data$peakArea$sorted %>%
      dplyr::bind_rows() %>%
      dplyr::filter(.data$sample_plate_id %in% plate_list) %>%
      dplyr::filter(.data$sample_name %in% .env$valid_sample_names)

    if (nrow(bound_data) == 0) next

    lipid_matrix <- bound_data %>%
      dplyr::select(tidyselect::where(~ !all(is.na(.)))) %>%
      dplyr::select(!dplyr::contains("sample") & !dplyr::contains("SIL")) %>%
      as.matrix()

    version_flags <- tibble::tibble(
      lipid = colnames(lipid_matrix),
      # Require non-NA: NA must not be counted as "below LOD pass".
      peakArea_below_LOD = colSums(!is.na(lipid_matrix) & lipid_matrix < lod),
      naValues = colSums(is.na(lipid_matrix), na.rm = TRUE),
      nanValues = colSums(is.nan(lipid_matrix), na.rm = TRUE),
      infValues = colSums(is.infinite(lipid_matrix), na.rm = TRUE)
    )

    version_flags$totalMissingValues <- rowSums(version_flags %>% dplyr::select(-lipid), na.rm = TRUE)
    master_list$filters$lipid.missingValues$allPlates[[version]] <- version_flags

    valid_sample_count <- nrow(lipid_matrix)
    master_list$filters$lipid.missingValues$PROJECT.flag.lipid[[version]] <- as.integer(version_flags$totalMissingValues > (
      valid_sample_count * (master_list$project_details$mv_sample_threshold / 100)
    ))

    master_list$filters$failed_lipids[[version]] <- version_flags$lipid[master_list$filters$lipid.missingValues$PROJECT.flag.lipid[[version]] == 1]
  }
  return(master_list)
}



#. ----

##RSD Filter ----
###Primary Function ----
#' RSD Filter
#'
#' Filters features per plate with a %RSD > 30% based on the precision of measurement in the `master_list` data.
#' Applies filtering to peakArea, concentration, and statTarget concentration data sources.
#' @keywords internal
#' @param master_list Master list from previous functions.
#' @return The updated `master_list` with RSD filter flags.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_RSD_filter(master_list)
#' }
qcCheckR_RSD_filter <- function(master_list) {
  master_list$filters$rsd <- tibble::tibble()

  # RSD for peakArea is computed from the pre-imputation "sorted" data.
  # lgw_impute replaces zeros/NAs with min/2, which deflates standard
  # deviation for poor-detection features and produces optimistically low RSDs.
  # calculate_rsd() drops NAs pairwise and requires >= 3 non-NA QC points.
  peakArea_raw <- master_list$data$peakArea$sorted
  # For "concentration", use concentration$sorted which is derived from
  # non-imputed sorted peak areas via the SIL ratio
  # (calculate_plate_response_concentration). It does NOT use lgw-imputed
  # values, so RSDs here reflect genuine measurement variability.
  # If concentration$sorted is unavailable, fall back to the imputed path;
  # in that case RSDs are a lower bound and should be interpreted cautiously.
  concentration_raw <- master_list$data$concentration$sorted
  if (is.null(concentration_raw)) {
    concentration_raw <- master_list$data$concentration$imputed
  }

  # Apply RSD filtering for each data source
  master_list$filters$rsd <- dplyr::bind_rows(
    calculate_rsd(master_list, "peakArea", peakArea_raw),
    calculate_rsd(master_list, "peakArea", list(
      allBatches = dplyr::bind_rows(peakArea_raw)
    )),
    calculate_rsd(
      master_list,
      "concentration",
      concentration_raw
    ),
    calculate_rsd(master_list, "concentration", list(
      allBatches = dplyr::bind_rows(concentration_raw)
    )),
    calculate_rsd(
      master_list,
      "concentration[statTarget]",
      master_list$data$concentration$statTargetProcessed
    ),
    calculate_rsd(
      master_list,
      "concentration[statTarget]",
      list(
        allBatches = dplyr::bind_rows(master_list$data$concentration$statTargetProcessed)
      )
    )
  )

  # Clean and format RSD table
  if (nrow(master_list$filters$rsd) > 0) {
    master_list$filters$rsd <- master_list$filters$rsd %>%
      dplyr::rename(dataSource = V1, dataBatch = V2) %>%
      dplyr::mutate(dplyr::across(!dplyr::contains("data"), as.numeric)) %>%
      dplyr::mutate(dplyr::across(!dplyr::contains("data"), \(x) round(x, 2)))
  }

  # Update script log
  master_list <- update_script_log(master_list,
                                   "data_filtering",
                                   "data_preparation",
                                   "summary_report")

  return(master_list)
}

###Sub Functions ----
#'Calculate RSD for a given data source and batch list
#'
#'This function calculates the relative standard deviation (RSD) for each feature in the provided data batches.
#'It filters out failed samples and selects only QC samples, then computes the RSD values.
#' @keywords internal
#' @param master_list Master list containing project details and data.
#' @param source_name Name of the data source (e.g., "peakArea", "concentration").
#' @param data_batches A list of data batches to process.
calculate_rsd <- function(master_list, source_name, data_batches) {
  rsd_results <- list()

  # QC-H6: if the entire input list is empty (e.g. statTargetProcessed was
  # never populated), emit a single placeholder row of NA_real_ so that
  # downstream consumers can see the stage ran but produced no usable RSDs.
  if (length(data_batches) == 0 || is.null(names(data_batches))) {
    placeholder <- tibble::tibble(V1 = source_name, V2 = "allBatches")
    return(placeholder)
  }

  for (batch_name in names(data_batches)) {
    batch_df <- data_batches[[batch_name]]
    if (is.null(batch_df) || nrow(batch_df) == 0 || !"sample_name" %in% colnames(batch_df))
      next

    # Prefer sample_class (three-way: qc / sample / other) when present so
    # that blanks / SIL / non-chosen QCs are not treated as QCs. Fall back
    # to legacy sample_type == "qc" when older pipelines feed this code.
    if ("sample_class" %in% colnames(batch_df)) {
      data <- batch_df %>%
        dplyr::filter(!.data$sample_name %in% master_list$filters$failed_samples) %>%
        dplyr::filter(.data$sample_class == "qc")
    } else {
      data <- batch_df %>%
        dplyr::filter(!.data$sample_name %in% master_list$filters$failed_samples) %>%
        dplyr::filter(.data$sample_type %in% c("qc"))
    }
    meta_cols <- grep("^sample_", colnames(data), value = TRUE)
    sil_cols  <- grep("^SIL[_. ]", colnames(data), value = TRUE, ignore.case = TRUE)
    data <- data %>%
      dplyr::select(-dplyr::any_of(c(meta_cols, sil_cols)))

    if (nrow(data) == 0)
      next

    # QC-C3: compute RSD on the raw (non-imputed) data using pairwise NA
    # drop. Require >= 3 non-NA QC points per metabolite; NA_real_ otherwise.
    # Use |mean| in the denominator and the shared .RSD_ZERO_EPS guard so
    # qcCheckR, batchCorrectR and the Shiny helpers report identical RSDs.
    # Use vapply to avoid silent matrix coercion of character columns.
    col_sd   <- vapply(data, function(x) stats::sd(x, na.rm = TRUE), numeric(1))
    col_mean <- vapply(data, function(x) mean(x, na.rm = TRUE), numeric(1))
    data_mat <- as.matrix(data)
    col_n_nonNA <- colSums(!is.na(data_mat))
    rsd_values <- ifelse(
      col_n_nonNA < 3 | abs(col_mean) < .RSD_ZERO_EPS,
      NA_real_,
      (col_sd / abs(col_mean)) * 100
    )
    rsd_row <- c(V1 = source_name, V2 = batch_name, rsd_values)
    rsd_results[[batch_name]] <- rbind(rsd_row) %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      dplyr::mutate(dplyr::across(names(rsd_values), as.numeric))
  }

  # QC-H6: if every batch was skipped (empty / malformed input), still
  # return a labelled NA row so downstream consumers see the stage ran.
  if (length(rsd_results) == 0) {
    return(tibble::tibble(V1 = source_name, V2 = "allBatches"))
  }

  dplyr::bind_rows(rsd_results)
}

#. ----

#Summary Report----
###Primary Function ----
#' Summary Report
#'
#' Generates a summary report for the `master_list` data,
#' including metrics for cohorts, matrix types, sample counts, lipid targets,
#' SIL versions, missing value filter flags, and RSD percentages.
#' @keywords internal
#' @param master_list Master list generated by previous functions.
#' @return The updated `master_list` with the summary report.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_summary_report(master_list)
#' }
qcCheckR_summary_report <- function(master_list) {
  #Create sample metrics
  sample_tags <- setdiff(
    as.character(unique(dplyr::pull(
      dplyr::select(dplyr::bind_rows(master_list$data$peakArea$sorted),
                    sample_type_factor)
    ))),
    "sample"
  )

  sample_metrics <- paste0(sample_tags, "Samples")

  metrics <- c(
    "MatrixType",
    "totalSamples",
    "studySamples",
    paste0(sample_metrics),
    "lipidTargets",
    "matchedLipidTargets",
    "SIL.version",
    "SIL.IntStds",
    "missingValueFilterFlags[samples]",
    "missingValueFilterFlags[SIL.IS]",
    "missingValueFilterFlags[lipidTargets]",
    "rsd<30%[peakArea]",
    "rsd<20%[peakArea]",
    "rsd<10%[peakArea]",
    "rsd<30%[concentration]",
    "rsd<20%[concentration]",
    "rsd<10%[concentration]",
    "rsd<30%[concentration.statTarget]",
    "rsd<20%[concentration.statTarget]",
    "rsd<10%[concentration.statTarget]"
  )

  master_list$summary_tables <- list()
  master_list$summary_tables$projectOverview <- tibble::tibble(metric = metrics)

  # Per-plate summary
  for (idx_batch in names(master_list$data$peakArea$sorted)) {
    plate_summary <- generate_plate_summary(master_list, idx_batch, metrics, sample_tags)
    master_list$summary_tables$projectOverview <- dplyr::left_join(master_list$summary_tables$projectOverview,
                                                            plate_summary,
                                                            by = "metric")
  }

  # Inter-plate summary
  inter_plate_summary <- generate_inter_plate_summary(master_list, metrics, sample_tags)
  master_list$summary_tables$projectOverview <- dplyr::left_join(master_list$summary_tables$projectOverview,
                                                          inter_plate_summary,
                                                          by = "metric")

  # Update script log
  master_list <- update_script_log(master_list,
                                   "summary_report",
                                   "data_filtering",
                                   "plot_generation")

  return(master_list)
}

###Sub Functions ----
#' Generate Plate Summary
#'
#' This function generates a summary for a specific plate in the `master_list`.
#' It includes metrics such as matrix type, sample counts, lipid targets,
#' SIL versions, missing value filter flags, and RSD percentages.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param idx_batch The index of the batch (plate) to process.
#' @param metrics tibble of the metrics to be included in the summary.
#' @param sample_tags A vector of sample type tags to be included in the summary.
#' @return A tibble containing the summary metrics for the specified plate.
generate_plate_summary <- function(master_list,
                                   idx_batch,
                                   metrics,
                                   sample_tags) {
  data <- master_list$data$peakArea$sorted[[idx_batch]]
  lipid_data <- data %>% dplyr::select(-dplyr::contains("sample"), -dplyr::contains("SIL"))
  sil_data <- data %>% dplyr::select(dplyr::contains("SIL"))

  # Summary RSD counts use fixed display thresholds (30/20/10 %).
  # These are for the summary table only and are independent of the
  # configurable filter threshold (project_details$rsd_threshold).
  rsd_metrics <- function(source) {
    rsd <- master_list$filters$rsd %>%
      dplyr::filter(dataBatch == idx_batch, dataSource == source) %>%
      dplyr::select(-dplyr::contains("data")) %>%
      dplyr::select(-dplyr::any_of(master_list$filters$failed_lipids))
    rsd_threshold <- master_list$project_details$rsd_threshold
    if (is.null(rsd_threshold) || !is.numeric(rsd_threshold) || length(rsd_threshold) != 1) {
      rsd_threshold <- 30
    }
    c(sum(rsd < rsd_threshold, na.rm = TRUE),
      sum(rsd < 20, na.rm = TRUE),
      sum(rsd < 10, na.rm = TRUE))
  }

  # QC-H3: collapse multi-matrix plates into a single scalar so the row
  # stays length-1 (prevents length drift / misalignment of subsequent
  # metrics when unique(sample_matrix) returns > 1 value).
  tag_counts <- if (length(sample_tags) > 0) {
    sapply(sample_tags, function(tag) nrow(dplyr::filter(data, sample_type_factor == tag)))
  } else {
    integer(0)
  }
  row_values <- c(
    paste(unique(data$sample_matrix), collapse = ","),
    nrow(data),
    nrow(dplyr::filter(data, sample_type_factor == "sample")),
    tag_counts,
    ncol(lipid_data),
    ncol(lipid_data),
    # Protect against empty / multi-value SIL version strings similarly.
    paste(unique(master_list$templates$`Plate SIL version`[[idx_batch]]), collapse = ","),
    ncol(sil_data),
    nrow(
      dplyr::filter(
        master_list$filters$samples.missingValues,
        sample_plate_id == idx_batch,
        sample.flag == 1
      )
    ),
    sum(master_list$filters$sil.intStd.missingValues[[idx_batch]][["flag_SIL_intStd_Plate"]] == 1),
    sum(master_list$filters$lipid.missingValues[[idx_batch]][["flag.Lipid.Plate"]] == 1),
    rsd_metrics("peakArea"),
    rsd_metrics("concentration"),
    rsd_metrics("concentration[statTarget]")
  ) %>% unlist()
  stopifnot(length(row_values) == length(metrics))
  plate_summary <- tibble::tibble(
    metric = metrics,
    !!idx_batch := row_values
  )

  return(plate_summary)
}

#' Generate Inter-Plate Summary
#'
#' This function generates a summary of all plates in the `master_list`.
#' It aggregates data across all plates, including metrics such as matrix type,
#' sample counts, lipid targets, SIL versions, missing value filter flags,
#' and RSD percentages.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param metrics tibble of the metrics to be included in the summary.
#' @param sample_tags A vector of sample type tags to be included in the summary.
#' @return A tibble containing the inter-plate summary metrics.
generate_inter_plate_summary <- function(master_list, metrics, sample_tags) {
  data <- dplyr::bind_rows(master_list$data$peakArea$sorted)
  lipid_data <- data %>% dplyr::select(-dplyr::contains("sample"), -dplyr::contains("SIL"))
  sil_data <- data %>% dplyr::select(dplyr::contains("SIL"))

  rsd_metrics <- function(source) {
    rsd <- master_list$filters$rsd %>%
      dplyr::filter(dataBatch == "allBatches", dataSource == source) %>%
      dplyr::select(-dplyr::contains("data")) %>%
      dplyr::select(-dplyr::any_of(master_list$filters$failed_lipids))
    c(sum(rsd < 30, na.rm = TRUE),
      sum(rsd < 20, na.rm = TRUE),
      sum(rsd < 10, na.rm = TRUE))
  }

  interplate_summary <- tibble::tibble(
    metric = metrics,
    all_plates = c(
      paste(unique(data$sample_matrix), collapse = ","),
      nrow(data),
      nrow(dplyr::filter(data, sample_type_factor == "sample")),
      sapply(sample_tags, function(tag)
        nrow(dplyr::filter(
          data, sample_type_factor == tag
        ))),
      ncol(lipid_data),
      ncol(dplyr::bind_rows(master_list$data$concentration$statTargetProcessed) %>%
             dplyr::select(-dplyr::contains("sample"))),
      paste(
        unique(master_list$templates$`Plate SIL version`),
        collapse = ","
      ),
      ncol(sil_data),
      nrow(
        dplyr::filter(master_list$filters$samples.missingValues, sample.flag == 1)
      ),
      length(master_list$filters$failed_sil.intStds),
      length(master_list$filters$failed_lipids),
      rsd_metrics("peakArea"),
      rsd_metrics("concentration"),
      rsd_metrics("concentration[statTarget]")
    ) %>% unlist()
  )
  return(interplate_summary)
}
