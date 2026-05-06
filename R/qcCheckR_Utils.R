#qcCheckR internal functions ----

#' Import specific functions from packages
#' @name qcCheckR_import_external_functions
#' @keywords internal
#' @importFrom utils browseURL capture.output
#' @importFrom readr read_csv write_csv read_tsv
#' @importFrom dplyr vars mutate bind_rows bind_cols filter select rename relocate arrange contains intersect pull left_join right_join any_of all_of across distinct rowwise c_across ungroup group_by case_when everything row_number summarise
#' @importFrom purrr map set_names
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 ggplot aes theme element_text labs geom_vline geom_hline geom_point theme_bw scale_shape_manual scale_color_manual scale_size_manual guides guide_legend facet_wrap scale_fill_manual ylab geom_text
#' @importFrom tibble tibble add_column as_tibble is_tibble column_to_rownames rownames_to_column
#' @importFrom tidyr replace_na pivot_wider
#' @importFrom stringr str_extract str_detect
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats median setNames na.omit sd
#' @importFrom tidyselect where
#' @importFrom openxlsx write.xlsx
#' @importFrom tools file_ext
#' @importFrom viridis viridis
#' @importFrom statTarget shiftCor
#' @importFrom ropls opls
NULL


#.----
##Batch Correction and Signal Drift Adjustment ----
### Primary Function ----
#' Batch Correction and Signal Drift Adjustment
#'
#' This function performs batch correction and signal drift adjustment on the concentration data in `master_list` using the `statTarget` package.
#' It prepares phenotype and profile files, runs `statTarget::shiftCor`, and integrates corrected data back into the master list.
#' @keywords internal
#' @param master_list A list containing project details, concentration data, and metadata.
#' @return The updated `master_list` with corrected concentration and peak area data.
qcCheckR_statTarget_batch_correction <- function(master_list) {
  batch_method <- master_list$project_details$batch_method
  if (!is.null(batch_method) && batch_method == "ComBat") {
    master_list <- qcCheckR_combat_correction(master_list)
  } else {
    run_date <- Sys.Date()
    FUNC_list <- initialise_statTarget_environment(master_list)
    FUNC_list$run_date <- run_date
    FUNC_list <- prepare_statTarget_files(FUNC_list)
    FUNC_list <- run_statTarget_shiftCor(FUNC_list, master_list)
    master_list <- integrate_corrected_data(master_list, FUNC_list)
  }
  master_list <- update_script_log(master_list,
                                   "data_preparation",
                                   "project_setup",
                                   "data_filtering")
  return(master_list)
}

### Sub Functions ----
#' Initialise statTarget Environment
#'
#' This function initialises the environment for `statTarget` batch correction.
#' It creates necessary directories, sets up the master data, and flags failed QC injections.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return A list containing the project directory, master data, and metabolite list for `statTarget`.
initialise_statTarget_environment <- function(master_list) {
  dirs <- c("data",
            "data/batch_correction",
            "data/rda",
            "xlsx_report",
            "html_report")
  lapply(dirs, function(d) {
    dir_path <- file.path(master_list$project_details$project_dir, "all", d)
    if (!check_dir_exists(dir_path))
      create_dir(dir_path)
  })


  if (is.null(master_list$project_details$statTarget_qc_type)) {
    master_list$project_details$statTarget_qc_type <- master_list$project_details$qc_type
  }
  FUNC_list <- list()
  FUNC_list$project_dir <- file.path(master_list$project_details$project_dir,
                                     "all",
                                     "data",
                                     "batch_correction")
  FUNC_list$master_data <- dplyr::bind_rows(master_list$data$concentration$imputed)

  # QC-H1: filter the phenotype frame so that statTarget's drift model only
  # sees the chosen QC type plus biological samples. Blanks, SIL-only
  # injections, conditioning runs, and non-chosen QCs must not be treated
  # as biological samples (which was the previous behaviour). Prefer the
  # 3-way sample_class column (QC-C1) when present; fall back to the
  # legacy sample_type for older callers.
  qc_type_lc <- tolower(master_list$project_details$statTarget_qc_type)
  if (!is.character(qc_type_lc) || length(qc_type_lc) != 1L || !nzchar(qc_type_lc)) {
    stop("initialise_statTarget_environment: 'statTarget_qc_type' must be a non-empty string. ",
         "Check that project_details$qc_type is set before calling this function.",
         call. = FALSE)
  }
  if ("sample_class" %in% colnames(FUNC_list$master_data)) {
    FUNC_list$master_data <- FUNC_list$master_data %>%
      dplyr::filter(.data$sample_class %in% c("qc", "sample"))
  } else if ("sample_type" %in% colnames(FUNC_list$master_data)) {
    # Older path: sample_type still carries original tag values here.
    FUNC_list$master_data <- FUNC_list$master_data %>%
      dplyr::filter(tolower(.data$sample_type) %in% c(qc_type_lc, "sample"))
  }

  FUNC_list$master_data$sample_type <- "sample"
  if ("sample_type_factor" %in% colnames(FUNC_list$master_data)) {
    FUNC_list$master_data$sample_type[
      tolower(as.character(FUNC_list$master_data$sample_type_factor)) == qc_type_lc
    ] <- "qc"
  } else {
    FUNC_list$master_data$sample_type[
      tolower(FUNC_list$master_data$sample_type) == qc_type_lc
    ] <- "qc"
  }
  if (sum(FUNC_list$master_data$sample_type == "qc") == 0) {
    stop("No QC samples identified for statTarget batch correction. ",
         "Check that 'qc_type' matches sample names and that sample_tags are correct.",
         call. = FALSE)
  }

  meta_cols_st <- grep("^sample_", colnames(FUNC_list$master_data), value = TRUE)
  FUNC_list$metabolite_list <- FUNC_list$master_data %>%
    dplyr::select(-dplyr::any_of(meta_cols_st)) %>%
    names()

  FUNC_list <- flag_failed_qc_injections(FUNC_list)
  return(FUNC_list)
}

#' Flag Failed QC Injections
#'
#' This function flags failed QC injections by checking the signal intensity of QC samples.
#' It identifies QC samples with low signal intensity and marks them as "sample" in the master data.
#' @keywords internal
#' @param FUNC_list A list containing the master data and metabolite list.
#' @return The updated `FUNC_list` with flagged failed QC injections.
flag_failed_qc_injections <- function(FUNC_list) {
  all_batches <- unique(FUNC_list$master_data$sample_plate_id)
  na_batches  <- all_batches[is.na(all_batches)]
  if (length(na_batches) > 0) {
    warning("flag_failed_qc_injections: ", length(na_batches),
            " row(s) have NA sample_plate_id and will be skipped.")
  }
  all_batches <- all_batches[!is.na(all_batches)]
  qc_fail <- lapply(all_batches, function(batch) {
    batch_qc <- FUNC_list$master_data %>%
      dplyr::filter(sample_type == "qc", sample_plate_id == batch)

    metadata_cols <- grep("^sample_", colnames(batch_qc), value = TRUE)
    sil_cols <- grep("^SIL[_. ]", colnames(batch_qc), value = TRUE, ignore.case = TRUE)
    qc_data <- batch_qc %>%
      dplyr::select(-dplyr::any_of(c(metadata_cols, sil_cols)))

    na_fraction <- rowSums(is.na(qc_data)) / ncol(qc_data)
    all_na      <- na_fraction == 1
    row_sums <- rowSums(qc_data, na.rm = TRUE)
    median_signal <- stats::median(row_sums[!all_na], na.rm = TRUE)

    # Exclude fully-NA rows from low_signal check (their row_sum is 0 only
    # because na.rm=TRUE returns 0 for all-NA rows, not genuine signal).
    low_signal <- !all_na & (row_sums < median_signal * 0.1)
    low_signal[is.na(low_signal)] <- FALSE
    high_na <- na_fraction > 0.5

    failed <- low_signal | high_na
    failed[is.na(failed)] <- FALSE

    failed_names <- FUNC_list$master_data %>%
      dplyr::filter(sample_type == "qc", sample_plate_id == batch) %>%
      dplyr::pull(sample_name)

    if (any(high_na & !low_signal)) {
      message("  Note: ", sum(high_na & !low_signal),
              " QC(s) in batch '", batch,
              "' flagged due to >50% NA values (not low signal)")
    }

    failed_names[failed]
  })

  qc_fail <- unlist(qc_fail)

  if (length(qc_fail) > 0) {
    message("  Flagged ", length(qc_fail),
            " failed QC injection(s): ",
            paste(qc_fail, collapse = ", "))
  } else {
    message("  No failed QC injections detected.")
  }

  FUNC_list$master_data$sample_type[FUNC_list$master_data$sample_name %in% qc_fail] <- "sample"

  return(FUNC_list)
}


#' Prepare statTarget Files
#'
#' This function prepares the phenotype and profile files required for `statTarget` batch correction.
#' It creates a phenotype file with sample metadata and a profile file with metabolite data.
#' @keywords internal
#' @param FUNC_list A list containing the project directory, master data, and metabolite list.
#' @return The updated `FUNC_list` with created phenotype and profile files.
prepare_statTarget_files <- function(FUNC_list) {
  FUNC_list <- create_pheno_file(FUNC_list)
  FUNC_list <- create_profile_file(FUNC_list)
  return(FUNC_list)
}

#' Create Pheno File
#'
#' This function creates a phenotype file from the master data.
#' It selects relevant columns, renames them, and formats the sample IDs and classes.
#' @keywords internal
#' @param FUNC_list A list containing the master data and project directory.
#' @return The updated `FUNC_list` with the created phenotype file.
create_pheno_file <- function(FUNC_list) {
  if (!is.data.frame(FUNC_list$master_data)) {
    stop("create_pheno_file: 'master_data' must be a data frame. Got: ",
         paste(class(FUNC_list$master_data), collapse = ", "),
         call. = FALSE)
  }
  pheno <- FUNC_list$master_data %>%
    dplyr::select(sample_name, sample_plate_id, sample_type, sample_run_index) %>%
    dplyr::rename(batch = sample_plate_id,
           class = sample_type,
           order = sample_run_index) %>%
    dplyr::arrange(order)

  # Reorder so QC samples are first and last within each batch
  qc_prep <- bc_prepare_qc_boundaries(pheno)
  FUNC_list$PhenoFile$template_qc_order <- qc_prep$pheno
  FUNC_list$qc_assessment <- qc_prep$qc_assessment

  # Assign sample names
  FUNC_list$PhenoFile$template_qc_order <- FUNC_list$PhenoFile$template_qc_order %>%
    dplyr::group_by(class) %>%
    dplyr::mutate(sample = dplyr::case_when(
      class == "qc" ~ paste0("QC", dplyr::row_number()),
      class == "sample" ~ paste0("sample", dplyr::row_number())
    )) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(sample, .after = sample_name)

  # Final formatting
  FUNC_list$PhenoFile$template_sample_id <- FUNC_list$PhenoFile$template_qc_order
  FUNC_list$PhenoFile$template_sample_id$class[FUNC_list$PhenoFile$template_sample_id$class == "qc"] <- NA
  FUNC_list$PhenoFile$template_sample_id$order <- seq_len(nrow(FUNC_list$PhenoFile$template_sample_id))
  FUNC_list$PhenoFile$template_sample_id$batch <- as.numeric(factor(FUNC_list$PhenoFile$template_sample_id$batch))

  #Final output file for statTarget
  Output <-  FUNC_list$PhenoFile$template_sample_id %>%
    dplyr::select(sample, batch, class, order)

  # Write to CSV
  run_date <- if (!is.null(FUNC_list$run_date)) FUNC_list$run_date else Sys.Date()
  output_dir <- file.path(FUNC_list$project_dir,
                          paste0(run_date, "_signal_correction_results"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(Output, file.path(output_dir, "PhenoFile.csv"))

  return(FUNC_list)
}

#' Create Profile File
#'
#' This function creates a profile file from the master data.
#' It selects the sample names and metabolite data, renames columns, and formats the data into a matrix.
#' @keywords internal
#' @param FUNC_list A list containing the master data, metabolite list, and project directory.
#' @return The updated `FUNC_list` with the created profile file.
create_profile_file <- function(FUNC_list) {

  if (length(FUNC_list$metabolite_list) == 0) {
    stop("metabolite_list cannot be empty")
  }

  profile <- FUNC_list$master_data %>%
    dplyr::select(sample_name, dplyr::all_of(FUNC_list$metabolite_list))

  # Carry batch / class / synthetic_qc / order through the join so synthetic
  # boundary QC rows (inserted by bc_prepare_qc_boundaries into the pheno
  # but absent from master_data) can be populated via per-batch
  # extrapolation. Without this, those rows stay NA for every metabolite
  # and silently bias the QCRFSC model -- the same defect already fixed on
  # the standalone batchCorrectR path.
  pheno_keys <- c("sample", "sample_name", "batch", "class",
                  "synthetic_qc", "order")
  pheno_keys <- intersect(pheno_keys,
                          colnames(FUNC_list$PhenoFile$template_sample_id))
  ordered_full <- FUNC_list$PhenoFile$template_sample_id %>%
    dplyr::select(dplyr::all_of(pheno_keys)) %>%
    dplyr::left_join(profile, by = "sample_name")

  if ("synthetic_qc" %in% colnames(ordered_full) &&
      any(ordered_full$synthetic_qc, na.rm = TRUE)) {
    ordered_full <- bc_populate_synthetic_qc_values(
      ordered_full, FUNC_list$metabolite_list)
  }

  ordered <- ordered_full %>%
    dplyr::select(sample, dplyr::all_of(FUNC_list$metabolite_list))

  transposed_raw <- tibble::as_tibble(cbind(nms = names(ordered), t(ordered)), .name_repair = "minimal")
  header_row <- as.character(transposed_raw[1, ])
  if (anyDuplicated(header_row) > 0L) {
    dup_vals <- unique(header_row[duplicated(header_row)])
    stop("create_profile_file: duplicate metabolite names after transpose: ",
         paste(dup_vals, collapse = ", "),
         ". Ensure metabolite names are unique before batch correction.", call. = FALSE)
  }
  profile_matrix <- transposed_raw %>%
    stats::setNames(header_row) %>%
    dplyr::rename(name = sample) %>%
    dplyr::filter(name != "sample") %>%
    dplyr::mutate(dplyr::across(-name, as.numeric))

  metabolite_map <- profile_matrix %>%
    dplyr::select(name) %>%
    dplyr::filter(!stringr::str_detect(name, "SIL")) %>%
    dplyr::mutate(metabolite_code = paste0("M", dplyr::row_number()))

  profile_matrix <- dplyr::left_join(metabolite_map, profile_matrix, by = "name") %>%
    dplyr::select(-name) %>%
    dplyr::rename(name = metabolite_code)

  FUNC_list$ProfileFile <- list(ProfileFile = profile_matrix, metabolite_list = metabolite_map)

  run_date <- if (!is.null(FUNC_list$run_date)) FUNC_list$run_date else Sys.Date()
  output_dir <- file.path(FUNC_list$project_dir,
                          paste0(run_date, "_signal_correction_results"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(profile_matrix, file.path(output_dir, "ProfileFile.csv"))
  return(FUNC_list)
}

#' Run statTarget shiftCor
#'
#' This function runs the `statTarget::shiftCor` function to perform signal drift correction on the prepared phenotype and profile files.
#' It specifies the correction parameters and reads the corrected data from the output file.
#' @keywords internal
#' @param FUNC_list A list containing the project directory, phenotype file, and profile file.
#' @param master_list A list containing all project details and data.
#' @return The updated `FUNC_list` with corrected data and adjusted QC means.
run_statTarget_shiftCor <- function(FUNC_list, master_list) {
  run_date <- if (!is.null(FUNC_list$run_date)) FUNC_list$run_date else Sys.Date()
  results_dir <- file.path(FUNC_list$project_dir,
                           paste0(run_date, "_signal_correction_results"))
  samPeno <- file.path(results_dir, "PhenoFile.csv")
  samFile <- file.path(results_dir, "ProfileFile.csv")

  batch_method  <- if (is.null(master_list$project_details$batch_method))  "QCRFSC"  else master_list$project_details$batch_method
  batch_ntree   <- if (is.null(master_list$project_details$batch_ntree))   500L      else master_list$project_details$batch_ntree
  batch_coCV    <- if (is.null(master_list$project_details$batch_coCV))    100       else master_list$project_details$batch_coCV
  batch_Frule   <- if (is.null(master_list$project_details$batch_Frule))   0         else master_list$project_details$batch_Frule
  batch_imputeM <- if (is.null(master_list$project_details$batch_imputeM)) "minHalf" else master_list$project_details$batch_imputeM

  message("  Running statTarget::shiftCor (method = ", batch_method,
          ", ntree = ", batch_ntree, ")...")
  withr::with_dir(results_dir, {
    statTarget::shiftCor(
      samPeno = samPeno,
      samFile = samFile,
      Frule = batch_Frule,
      ntree = batch_ntree,
      MLmethod = batch_method,
      imputeM = batch_imputeM,
      plot = FALSE,
      coCV = batch_coCV
    )
  })
  message("  statTarget::shiftCor completed successfully.")

  corrected <- readr::read_csv(
    file.path(results_dir, "statTarget/shiftCor/After_shiftCor/shift_all_cor.csv"),
    show_col_types = FALSE
  )

  corrected <- clean_statTarget_output(corrected)
  FUNC_list$corrected_data <- list(data = corrected)
  FUNC_list <- transpose_and_merge_corrected(FUNC_list)
  FUNC_list <- adjust_qc_means(FUNC_list, master_list)
  return(FUNC_list)
}

#' Clean statTarget Output
#'
#' This function cleans the output data from `statTarget` by filtering out unwanted rows and renaming columns.
#' It handles different column structures based on the presence of specific sample columns.
#' @keywords internal
#' @param data A tibble containing the output data from `statTarget`.
#' @return A cleaned tibble with renamed columns and numeric data types.
clean_statTarget_output <- function(data) {
  # Only invoke the shared helper for known statTarget formats;
  # return data unchanged if neither format marker is present.
  if ("sample1" %in% colnames(data) || "M1" %in% colnames(data)) {
    return(bc_detect_stattarget_format(data))
  }
  data
}


#' Transpose and Merge Corrected Data
#' This function transposes the corrected data and merges it with the metabolite list from the profile file.
#' It renames columns, filters out unwanted rows, and adds sample metadata.
#' @keywords internal
#' @param FUNC_list A list containing the corrected data and profile file.
#' @return The updated `FUNC_list` with transposed and merged corrected data.
transpose_and_merge_corrected <- function(FUNC_list) {
  raw_transposed <- FUNC_list$ProfileFile$metabolite_list %>%
    dplyr::rename(lipid = metabolite_code) %>%
    dplyr::right_join(FUNC_list$corrected_data$data %>% dplyr::rename(lipid = name), by = "lipid") %>%
    dplyr::select(-lipid) %>%
    as.matrix() %>%
    t() %>%
    data.frame() %>%
    tibble::rownames_to_column()
  transposed_header <- as.character(raw_transposed[1, ])
  transposed <- raw_transposed %>%
    stats::setNames(transposed_header) %>%
    dplyr::filter(name != "name") %>%
    dplyr::mutate(dplyr::across(-name, as.numeric)) %>%
    dplyr::rename(sample = name) %>%
    dplyr::left_join(FUNC_list$PhenoFile$template_sample_id, by = "sample") %>%
    dplyr::left_join(FUNC_list$master_data %>% dplyr::select(dplyr::contains("sample")), by = "sample_name") %>%
    dplyr::select(-sample, -batch, -class, -order)

  #reorder columns to follow FUNC_list$master_data
  metadata_cols <- c("sample_run_index", "sample_name", "sample_timestamp",
                     "sample_plate_id", "sample_plate_order", "sample_matrix",
                     "sample_type", "sample_type_factor", "sample_type_factor_rev",
                     "sample_data_source", "sample_ID")
  transposed <- transposed %>%
    dplyr::select(dplyr::any_of(metadata_cols), dplyr::everything())

  FUNC_list$corrected_data$data_transposed <- transposed
  return(FUNC_list)
}

#' Adjust QC Means
#'
#' This function adjusts the means of QC samples in the corrected data by calculating the correction ratio based on original and corrected means.
#' It applies the correction ratio to the corrected data and updates the sample type for QC samples.
#' @keywords internal
#' @param FUNC_list A list containing the master data and corrected data.
#' @param master_list A list containing the project details and data.
#' @return The updated `FUNC_list` with adjusted QC means in the corrected data.
adjust_qc_means <- function(FUNC_list, master_list) {

  if (length(FUNC_list$metabolite_list) == 0) {
    stop("No metabolite columns found in master_data.")
  }

  original_means <- FUNC_list$master_data %>%
    dplyr::filter(sample_type == "qc") %>%
    dplyr::select(dplyr::all_of(FUNC_list$metabolite_list)) %>%
    colMeans(na.rm = TRUE)

  if (any(is.nan(original_means) | is.na(original_means))) {
    warning("adjust_qc_means: ", sum(is.nan(original_means) | is.na(original_means)),
            " metabolite(s) have all-NA QC values; they will be excluded from mean adjustment.")
  }
  if (any(original_means == 0, na.rm = TRUE)) {
    warning("Original mean contains zero(s), which may lead to invalid correction ratios.")
  }

  # Exclude synthetic boundary QCs from the post-correction mean. They were
  # injected solely to satisfy statTarget::REGfit's "first/last sample must
  # be QC" requirement and have no counterpart in master_data, so
  # original_means is over real QCs only. Including synthetic rows on the
  # corrected side biases the ratio asymmetrically and was the dominant
  # cause of >100x ratios tripping the clamp in bc_compute_mean_ratios.
  corrected_qc <- FUNC_list$corrected_data$data_transposed %>%
    dplyr::filter(sample_type == "qc")
  if ("synthetic_qc" %in% colnames(corrected_qc)) {
    corrected_qc <- corrected_qc[!as_logical_na_false(corrected_qc$synthetic_qc), , drop = FALSE]
  }
  # statTarget may legitimately drop metabolites during signal correction
  # (Frule, coCV, missingness, all-NA QCs). Fall back to the intersection
  # rather than failing with a strict all_of() error -- bc_compute_mean_ratios
  # / bc_apply_mean_ratios already handle the gap, leaving missing
  # metabolites uncorrected. Surface the dropped names so users can see
  # which metabolites the correction step removed.
  available_metabolites <- intersect(FUNC_list$metabolite_list,
                                     colnames(corrected_qc))
  missing_metabolites <- setdiff(FUNC_list$metabolite_list,
                                 colnames(corrected_qc))
  if (length(missing_metabolites) > 0) {
    message("adjust_qc_means: ", length(missing_metabolites),
            " metabolite(s) absent from statTarget-corrected output ",
            "(left uncorrected). First 5: ",
            paste(utils::head(missing_metabolites, 5), collapse = ", "))
  }
  corrected_means <- corrected_qc %>%
    dplyr::select(dplyr::all_of(available_metabolites)) %>%
    colMeans(na.rm = TRUE)

  ratios <- bc_compute_mean_ratios(original_means, corrected_means)
  adjusted <- bc_apply_mean_ratios(FUNC_list$corrected_data$data_transposed, ratios)

  # Use master_data if it already carries sample_type_factor (it was bound
  # from the imputed concentration list in initialise_statTarget_environment).
  # Fall back to re-binding from master_list for callers that supply a simpler
  # FUNC_list without that column (e.g., tests).
  if ("sample_type_factor" %in% colnames(FUNC_list$master_data)) {
    bound_imputed <- FUNC_list$master_data
  } else {
    bound_imputed <- dplyr::bind_rows(master_list$data$concentration$imputed)
  }
  qc_type <- unique(bound_imputed$sample_type_factor[bound_imputed$sample_type == "qc"]) %>% as.character()

  if (length(qc_type) > 1) {
    warning("Multiple QC types found in corrected data: ",
            paste(qc_type, collapse = ", "),
            ". Using project-level qc_type for classification.",
            call. = FALSE)
    qc_type <- master_list$project_details$qc_type
  }

  adjusted$sample_type <- ifelse(
    tolower(adjusted$sample_type_factor) %in% tolower(qc_type), "qc", "sample"
  )
  FUNC_list$corrected_data$data_qc_mean_adjusted <- adjusted
  return(FUNC_list)
}


#' Integrate Corrected Data into Master List
#'
#' This function integrates the corrected data from `statTarget` into the master list.
#' It splits the corrected data by sample plate ID, updates the sample data source, and processes the peak area and concentration data.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param FUNC_list A list containing the corrected data from `statTarget`.
#' @return The updated `master_list` with integrated corrected data.
integrate_corrected_data <- function(master_list, FUNC_list) {
  if (!"sample_plate_id" %in% colnames(FUNC_list$corrected_data$data_qc_mean_adjusted)) {
    stop("Missing required column: sample_plate_id")
  }

  corrected <- FUNC_list$corrected_data$data_qc_mean_adjusted
  # Drop synthetic boundary QC rows (and any other rows that lack a real
  # sample_plate_id from the join with master_data). They were injected by
  # bc_prepare_qc_boundaries solely to anchor statTarget's drift model and
  # have no place in user-facing concentration tables. Without this, the
  # downstream loop tries to filter() on an NA batch, yielding a 0-row
  # frame, then errors with "replacement has 1 row, data has 0" when
  # assigning sample_data_source.
  if ("synthetic_qc" %in% colnames(corrected)) {
    corrected <- corrected[!as_logical_na_false(corrected$synthetic_qc), , drop = FALSE]
  }
  corrected <- corrected[!is.na(corrected$sample_plate_id), , drop = FALSE]
  if ("sample_ID" %in% colnames(corrected)) {
    corrected <- corrected %>% dplyr::relocate(sample_ID, .after = sample_name)
  }
  master_list$data$concentration$corrected <- split(corrected, corrected$sample_plate_id)

  for (batch in names(master_list$data$concentration$corrected)) {
    master_list$data$concentration$corrected[[batch]]$sample_data_source <- ".peakAreaCorrected"
  }

  master_list$data$peakArea$statTargetProcessed <- list()
  master_list$data$concentration$statTargetProcessed <- list()
  for (batch in unique(corrected$sample_plate_id)) {
    batch_data <- corrected %>% dplyr::filter(sample_plate_id == batch)
    if ("sample_ID" %in% colnames(batch_data)) {
      batch_data <- batch_data %>% dplyr::relocate(sample_ID, .after = sample_name)
    }
    batch_data$sample_data_source <- "concentration.statTarget"
    master_list$data$peakArea$statTargetProcessed[[batch]] <- batch_data
    master_list$data$concentration$statTargetProcessed[[batch]] <- batch_data
  }

  return(master_list)
}


#' ComBat Batch Correction for qcCheckR Pipeline
#'
#' Applies empirical Bayes batch correction using \code{sva::ComBat} within
#' the qcCheckR pipeline. Unlike statTarget methods, ComBat does not require
#' QC samples.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated \code{master_list} with corrected data.
qcCheckR_combat_correction <- function(master_list) {
  if (!requireNamespace("sva", quietly = TRUE)) {
    stop("qcCheckR: The 'sva' package is required for ComBat correction. ",
         "Install it with: BiocManager::install('sva')", call. = FALSE)
  }

  message("  Running ComBat batch correction (QC-free method)...")

  # Combine all plates of imputed concentration data
  combined_data <- dplyr::bind_rows(master_list$data$concentration$imputed)

  # Identify metabolite columns (non-sample columns)
  metabolite_cols <- combined_data %>%
    dplyr::select(-dplyr::contains("sample")) %>%
    names()

  # Filter to only numeric metabolite columns
  metabolite_cols <- metabolite_cols[vapply(combined_data[metabolite_cols],
                                            is.numeric, logical(1))]

  if (length(metabolite_cols) == 0) {
    stop("qcCheckR: No numeric metabolite columns found for ComBat correction.",
         call. = FALSE)
  }

  # Get ComBat parameters from master_list
  par_prior <- master_list$project_details$combat_par.prior
  if (is.null(par_prior)) par_prior <- TRUE
  mean_only <- master_list$project_details$combat_mean.only
  if (is.null(mean_only)) mean_only <- FALSE
  ref_batch <- master_list$project_details$combat_ref.batch

  if (!is.null(ref_batch)) {
    available_batches <- unique(as.character(combined_data$sample_plate_id))
    if (!as.character(ref_batch) %in% available_batches) {
      stop("qcCheckR: 'combat_ref.batch' = '", ref_batch,
           "' is not present in the data's sample_plate_id column. ",
           "Available batches: ",
           paste(shQuote(available_batches), collapse = ", "),
           ". Use NULL to adjust against the grand mean instead.",
           call. = FALSE)
    }
  }

  message("    par.prior = ", par_prior, ", mean.only = ", mean_only,
          if (!is.null(ref_batch)) paste0(", ref.batch = ", ref_batch) else "")

  # Prepare the feature matrix using the shared helper
  prep <- bc_prepare_combat_matrix(combined_data, metabolite_cols)

  # Run ComBat (called directly so test stubs can intercept sva::ComBat)
  corrected_matrix <- sva::ComBat(
    dat = prep$dat_combat,
    batch = combined_data$sample_plate_id,
    mod = NULL,
    par.prior = par_prior,
    prior.plots = FALSE,
    mean.only = mean_only,
    ref.batch = ref_batch
  )

  message("    sva::ComBat completed successfully.")

  # Reconstruct corrected data using the shared helper
  corrected <- bc_reconstruct_combat_output(combined_data, corrected_matrix,
                                             prep$kept_features)

  # Set sample_type column for consistency
  qc_type <- master_list$project_details$statTarget_qc_type
  if (is.null(qc_type)) qc_type <- master_list$project_details$qc_type
  corrected$sample_type <- ifelse(
    tolower(corrected$sample_type_factor) == tolower(qc_type),
    "qc", "sample"
  )

  # Split back by plate and integrate into master_list (same structure as statTarget path)
  master_list$data$concentration$corrected <- split(corrected, corrected$sample_plate_id)
  for (batch_name in names(master_list$data$concentration$corrected)) {
    master_list$data$concentration$corrected[[batch_name]]$sample_data_source <- ".peakAreaCorrected"
  }

  master_list$data$peakArea$statTargetProcessed <- list()
  master_list$data$concentration$statTargetProcessed <- list()
  for (batch_name in unique(corrected$sample_plate_id)) {
    batch_data <- corrected %>% dplyr::filter(sample_plate_id == batch_name)
    master_list$data$peakArea$statTargetProcessed[[batch_name]] <- batch_data
    master_list$data$concentration$statTargetProcessed[[batch_name]] <- batch_data
    master_list$data$peakArea$statTargetProcessed[[batch_name]]$sample_data_source <- "concentration.ComBat"
  }

  message("  ComBat batch correction complete.")
  return(master_list)
}


