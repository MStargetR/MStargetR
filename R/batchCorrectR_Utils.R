# batchCorrectR_Utils.R
# Internal helper functions for the batchCorrectR interbatch correction pipeline.

# Known metadata column names -- any column NOT in this list is treated as a metabolite
.METADATA_COLS <- c("sample_name", "sample_plate_id", "sample_plate_order",
                    "sample_timestamp", "sample_matrix", "sample_type",
                    "sample_type_orig", "sample_type_factor", "sample_type_factor_rev",
                    "sample_data_source", "sample_run_index", "sample_ID",
                    "batch", "class", "order", "sample", "synthetic_qc")

# Input Preprocessing ----
#' Preprocess batchCorrectR Input
#'
#' Accepts a single data.frame or a list of data.frames, combines them, and
#' maps column names to the canonical internal format expected by the pipeline.
#'
#' @keywords internal
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 geom_bar
#' @importFrom dplyr n desc
#' @importFrom stats reorder prcomp var
#' @importFrom utils head
#' @param data A data.frame, tibble, or list of data.frames/tibbles.
#' @param batch_column Optional character. Name of the column in \code{data}
#'   that holds the batch identifier. When \code{NULL} (default) the function
#'   auto-detects \code{batch} or \code{sample_plate_id} (in that order). When
#'   supplied, that column's values are copied into the canonical
#'   \code{batch} column for the rest of the pipeline. Use this to drive the
#'   correction off any user-named column (e.g. \code{plate}, \code{run_batch}).
#' @return A single data.frame in canonical format with columns:
#'   \code{sample_name}, \code{batch}, \code{sample_type}, \code{run_order},
#'   plus metabolite columns.
bc_preprocess_input <- function(data, batch_column = NULL) {
  # Combine list of dataframes
  if (is.list(data) && !is.data.frame(data)) {
    if (length(data) == 0)
      stop("batchCorrectR: 'data' list is empty.", call. = FALSE)
    if (!all(vapply(data, is.data.frame, logical(1))))
      stop("batchCorrectR: All elements of 'data' list must be data.frames.",
           call. = FALSE)
    data <- dplyr::bind_rows(data)
  }

  # User-specified batch column overrides the default sample_plate_id->batch
  # mapping below. Copying (rather than renaming) preserves the original
  # column too so bc_reconstruct_output can still emit it in the user's
  # output schema.
  if (!is.null(batch_column)) {
    if (!is.character(batch_column) || length(batch_column) != 1 ||
        !nzchar(batch_column)) {
      stop("batchCorrectR: 'batch_column' must be a single non-empty ",
           "character string or NULL. Got: ", deparse(batch_column),
           call. = FALSE)
    }
    if (!batch_column %in% colnames(data)) {
      stop("batchCorrectR: 'batch_column' = '", batch_column,
           "' was not found in the data. Available columns: ",
           paste(shQuote(colnames(data)), collapse = ", "),
           call. = FALSE)
    }
    if (batch_column != "batch") {
      data$batch <- data[[batch_column]]
    }
  }

  # Column mapping: user-facing names -> canonical internal names
  col_map <- c(
    "sample_run_index" = "run_order",
    "sample_plate_id" = "batch"
  )

  for (old_name in names(col_map)) {
    new_name <- col_map[[old_name]]
    if (old_name %in% colnames(data) && !new_name %in% colnames(data)) {
      colnames(data)[colnames(data) == old_name] <- new_name
    }
  }

  # Use sample_type_factor for QC matching if available.
  # Preserve the original sample_type under sample_type_orig before overwriting
  # so downstream code that needs the user's original labels can still access it.
  if ("sample_type_factor" %in% colnames(data)) {
    if (!"sample_type_orig" %in% colnames(data)) {
      data$sample_type_orig <- data$sample_type
    }
    data$sample_type <- as.character(data$sample_type_factor)
  }
  # Ensure sample_type is always character so factor-level assignments in
  # bc_flag_failed_qc do not silently produce NA when the level is absent.
  if ("sample_type" %in% colnames(data)) {
    data$sample_type <- as.character(data$sample_type)
  }

  # Normalise sample_name by stripping .mzML suffix (qcCheckR does this in
  # extract_run_order), so downstream joins on sample_name match regardless
  # of which pipeline the caller used.
  if ("sample_name" %in% colnames(data)) {
    data$sample_name <- strip_mzml_suffix(data$sample_name)
  }

  # Normalise sample_timestamp to POSIXct so standalone output aligns with
  # qcCheckR::extract_run_order (which emits POSIXct). Character/factor input
  # is parsed via the shared canonical format list.
  if ("sample_timestamp" %in% colnames(data)) {
    data$sample_timestamp <- parse_sample_timestamp(data$sample_timestamp)
  }

  # qcCheckR arranges by sample_timestamp and rebuilds sample_run_index as
  # a contiguous 1..N sequence after bind_rows (R/qcCheckR_dataprep.R:472).
  # Mirror that here so per-plate indices that overlap (e.g. plate A 1..6 and
  # plate B 1..6) are canonicalised to a single run_order sequence.
  if ("sample_timestamp" %in% colnames(data) &&
      inherits(data$sample_timestamp, "POSIXct") &&
      "run_order" %in% colnames(data)) {
    data <- data %>%
      dplyr::arrange(sample_timestamp) %>%
      dplyr::mutate(run_order = dplyr::row_number())
  }

  data
}

# Input Validation ----
#' Validate batchCorrectR Input
#' @keywords internal
#' @param data,qc_label,method,ntree,coCV,Frule,imputeM See \code{batchCorrectR}.
#' @return Invisible TRUE if all checks pass.
bc_validate_input <- function(data, qc_label, method, ntree, coCV, Frule, imputeM) {
  if (!is.data.frame(data))
    stop("batchCorrectR: 'data' must be a data.frame or tibble. Received: ",
         paste(class(data), collapse = ", "), call. = FALSE)
  if (nrow(data) == 0)
    stop("batchCorrectR: 'data' must not be empty (0 rows).", call. = FALSE)
  required_cols <- c("sample_name", "batch", "sample_type", "run_order")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0)
    stop("batchCorrectR: Missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  if (any(duplicated(data$sample_name)))
    stop("batchCorrectR: 'sample_name' must be unique. Found ",
         sum(duplicated(data$sample_name)), " duplicate(s).", call. = FALSE)
  if ("batch" %in% colnames(data)) {
    batch_sample_key <- paste(data$batch, data$sample_name, sep = "\x1f")
    if (any(duplicated(batch_sample_key))) {
      n_dup <- sum(duplicated(batch_sample_key))
      warning("batchCorrectR: ", n_dup,
              " duplicate (batch, sample_name) combination(s) found. ",
              "Each sample should appear only once per batch.",
              call. = FALSE)
    }
  }
  if (!is.numeric(data$run_order))
    stop("batchCorrectR: 'run_order' column must be numeric. Got: ",
         paste(class(data$run_order), collapse = ", "), call. = FALSE)
  if (any(!is.na(data$run_order) & data$run_order != floor(data$run_order)))
    stop("batchCorrectR: 'run_order' must contain only integer values. ",
         "Fractional run_order values cause collisions with synthetic QC ",
         "boundary markers (min-0.5 / max+0.5). Round or coerce to integer.",
         call. = FALSE)
  has_ts <- "sample_timestamp" %in% colnames(data) &&
    inherits(data$sample_timestamp, "POSIXct")
  if (!has_ts && "batch" %in% colnames(data) && length(unique(data$batch)) > 1) {
    batch_ranges <- lapply(unique(data$batch), function(b) {
      ro <- data$run_order[data$batch == b]
      c(min(ro, na.rm = TRUE), max(ro, na.rm = TRUE))
    })
    names(batch_ranges) <- unique(data$batch)
    batches <- names(batch_ranges)
    for (i in seq_along(batches)) {
      for (j in seq_along(batches)) {
        if (j <= i) next
        r1 <- batch_ranges[[i]]; r2 <- batch_ranges[[j]]
        if (r1[1] <= r2[2] && r2[1] <= r1[2])
          warning("batchCorrectR: batch '", batches[i], "' run_order range [",
                  r1[1], ",", r1[2], "] overlaps with batch '", batches[j],
                  "' [", r2[1], ",", r2[2], "]. ",
                  "Global arrange by run_order may interleave batches. ",
                  "Provide sample_timestamp or use non-overlapping run_order.",
                  call. = FALSE)
      }
    }
  }
  if (!is.character(qc_label) || length(qc_label) != 1 || !nzchar(qc_label))
    stop("batchCorrectR: 'qc_label' must be a non-empty single character string. Got: ",
         paste(class(qc_label), collapse = ", "), call. = FALSE)
  if (method != "ComBat") {
    qc_mask <- tolower(data$sample_type) == tolower(qc_label)
    if (sum(qc_mask) == 0)
      stop("batchCorrectR: No QC samples found. Expected '", qc_label,
           "' in sample_type. Found: ",
           paste(unique(data$sample_type), collapse = ", "), call. = FALSE)
    for (b in unique(data$batch)) {
      n_qc <- sum(tolower(data$sample_type[data$batch == b]) == tolower(qc_label))
      if (n_qc < 2)
        stop("batchCorrectR: Batch '", b, "' has < 2 QC samples (", n_qc, " found).",
             call. = FALSE)
    }
  }
  meta_cols <- c("sample_name", "batch", "sample_type", "run_order")
  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  if (length(setdiff(numeric_cols, meta_cols)) == 0)
    stop("batchCorrectR: No numeric metabolite columns found in 'data'.",
         call. = FALSE)
  if (!is.character(method) || length(method) != 1)
    stop("batchCorrectR: 'method' must be a single character string. Got: ",
         paste(class(method), collapse = ", "), call. = FALSE)
  if (!method %in% c("QCRFSC", "ComBat"))
    stop("batchCorrectR: Invalid 'method': '", method,
         "'. Must be one of: 'QCRFSC', 'ComBat'.", call. = FALSE)
  if (!is.numeric(ntree) || length(ntree) != 1 || ntree < 1)
    stop("batchCorrectR: 'ntree' must be a positive integer. Got: ",
         deparse(ntree), call. = FALSE)
  if (!is.numeric(coCV) || length(coCV) != 1 || coCV <= 0)
    stop("batchCorrectR: 'coCV' must be a positive number. Got: ",
         deparse(coCV), call. = FALSE)
  if (!is.numeric(Frule) || length(Frule) != 1 || Frule < 0 || Frule > 1)
    stop("batchCorrectR: 'Frule' must be between 0 and 1. Got: ",
         deparse(Frule), call. = FALSE)
  if (!is.character(imputeM) || length(imputeM) != 1)
    stop("batchCorrectR: 'imputeM' must be a single character string. Got: ",
         paste(class(imputeM), collapse = ", "), call. = FALSE)
  if (!imputeM %in% c("minHalf", "median", "mean", "knn"))
    stop("batchCorrectR: Invalid 'imputeM': '", imputeM,
         "'. Must be one of: 'minHalf', 'median', 'mean', 'knn'.", call. = FALSE)
  invisible(TRUE)
}

# Metabolite Column Detection ----
#' Detect Metabolite Columns
#' @keywords internal
#' @param data A data.frame containing sample and metabolite data.
#' @return Character vector of metabolite column names.
bc_detect_metabolite_columns <- function(data) {
  meta_cols <- unique(c("sample_name", "batch", "sample_type", "run_order",
                        .METADATA_COLS))
  candidate_cols <- setdiff(colnames(data), meta_cols)
  # Only keep numeric columns as metabolites
  metabolite_cols <- candidate_cols[vapply(data[candidate_cols], is.numeric, logical(1))]
  if (length(metabolite_cols) == 0)
    stop("batchCorrectR: No numeric metabolite columns detected.", call. = FALSE)
  metadata_like <- grep("(volume|weight|factor|dilut|inject|conc|ratio)",
                        metabolite_cols, ignore.case = TRUE, value = TRUE)
  if (length(metadata_like) > 0)
    warning("batchCorrectR: the following numeric column(s) look like metadata ",
            "but will be treated as metabolites: ",
            paste(utils::head(metadata_like, 5), collapse = ", "),
            ". Add them to '.METADATA_COLS' to exclude.", call. = FALSE)
  message("    Detected metabolite columns (", length(metabolite_cols), "): ",
          paste(utils::head(metabolite_cols, 5), collapse = ", "),
          if (length(metabolite_cols) > 5) paste0(" ... [", length(metabolite_cols) - 5, " more]") else "")
  return(metabolite_cols)
}

# QC Flagging ----
#' Flag Failed QC Injections
#' @keywords internal
#' @param data A data.frame of sample data.
#' @param qc_label Character identifying QC samples.
#' @param metabolite_cols Character vector of metabolite column names.
#' @return List with \code{data} (modified) and \code{failed_samples} (character).
bc_flag_failed_qc <- function(data, qc_label, metabolite_cols) {
  failed_samples <- character(0)
  data_out <- data
  for (b in unique(data$batch)) {
    batch_qc_mask <- data$batch == b & tolower(data$sample_type) == tolower(qc_label)
    qc_data <- data[batch_qc_mask, metabolite_cols, drop = FALSE]
    if (nrow(qc_data) == 0) next
    row_sums <- rowSums(qc_data, na.rm = TRUE)
    low_signal <- row_sums < (stats::median(row_sums, na.rm = TRUE) * 0.1)
    if (any(low_signal)) {
      failed_names <- data$sample_name[batch_qc_mask][low_signal]
      failed_samples <- c(failed_samples, failed_names)
      data_out$sample_type[data_out$sample_name %in% failed_names] <- "sample"
    }
  }
  list(data = data_out, failed_samples = failed_samples)
}

# RSD Calculation ----
#' Calculate Percent RSD for QC Samples
#' @keywords internal
#' @param data A data.frame of sample data.
#' @param qc_label Character identifying QC samples.
#' @param metabolite_cols Character vector of metabolite column names.
#' @return Named numeric vector of percent RSD values.
bc_calculate_rsd <- function(data, qc_label, metabolite_cols) {
  qc_data <- data[tolower(data$sample_type) == tolower(qc_label),
                   metabolite_cols, drop = FALSE]
  if (nrow(qc_data) == 0) {
    warning("batchCorrectR: No QC samples for RSD calculation.")
    return(stats::setNames(rep(NA_real_, length(metabolite_cols)), metabolite_cols))
  }
  vapply(metabolite_cols, function(col) {
    vals <- qc_data[[col]][!is.na(qc_data[[col]])]
    if (length(vals) < 2) return(NA_real_)
    if (abs(mean(vals)) < .RSD_ZERO_EPS) {
      # Near-zero mean: either the feature has no real signal in QCs (all values
      # are at or below the floating-point zero threshold) or all QC values were
      # imputed to zero. Either way %RSD is undefined; emit a message so the
      # caller can distinguish this from structural missingness (length < 2).
      message("bc_calculate_rsd: near-zero QC mean for feature '", col,
              "'; %RSD set to NA (possible zero-signal feature).")
      return(NA_real_)
    }
    (stats::sd(vals) / abs(mean(vals))) * 100
  }, numeric(1))
}

# PhenoFile Preparation ----
#' Prepare PhenoFile for statTarget
#' @keywords internal
#' @param data Data.frame with flagged sample types.
#' @param qc_label Character identifying QC samples.
#' @param st_dir Path to statTarget working directory.
#' @return Tibble with pheno mapping columns: sample, sample_name, batch, class, order.
bc_prepare_pheno_file <- function(data, qc_label, st_dir) {
  # bc_prepare_qc_boundaries keys on columns named `order` (see
  # R/batch_shared_utils.R). Historically this step kept the `run_order`
  # name, which made `bd$order` NULL inside the helper, silently assigned
  # every synthetic row `order = Inf`, and after `arrange(order)` caused
  # synthetic QCs from every batch to collapse to the start/end of the
  # global sequence -- which statTarget::shiftCor then crashed on with
  # "subscript out of bounds". Rename here so the helper sees the column.
  pheno <- data %>%
    dplyr::select(sample_name, batch, sample_type, run_order) %>%
    dplyr::mutate(class = ifelse(tolower(sample_type) == tolower(qc_label),
                                  "qc", "sample")) %>%
    dplyr::rename(order = run_order) %>%
    dplyr::arrange(order)
  qc_prep <- bc_prepare_qc_boundaries(pheno)
  pheno_ordered <- qc_prep$pheno
  pheno_ordered <- pheno_ordered %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sample = {
      lbl <- character(dplyr::n())
      qc_pos <- which(class == "qc")
      sp_pos <- which(class == "sample")
      lbl[qc_pos] <- paste0("QC", seq_along(qc_pos))
      lbl[sp_pos] <- paste0("sample", seq_along(sp_pos))
      lbl
    }) %>%
    dplyr::mutate(class_st = ifelse(class == "qc", NA_character_, class),
                   batch_num = as.numeric(factor(batch, levels = unique(batch))),
                   order_seq = seq_len(dplyr::n()))
  readr::write_csv(
    pheno_ordered %>% dplyr::select(sample, batch = batch_num,
                                     class = class_st, order = order_seq),
    file.path(st_dir, "PhenoFile.csv"))
  # Keep `synthetic_qc` in the returned pheno so bc_prepare_profile_file can
  # populate estimated metabolite values for synthetic boundary rows.
  if (!"synthetic_qc" %in% colnames(pheno_ordered)) {
    pheno_ordered$synthetic_qc <- FALSE
  }
  pheno_ordered %>% dplyr::select(sample, sample_name, batch, class,
                                    synthetic_qc,
                                    order = order_seq)
}

# ProfileFile Preparation ----
#' Prepare ProfileFile for statTarget
#' @keywords internal
#' @param data Data.frame with flagged sample types.
#' @param metabolite_cols Character vector of metabolite column names.
#' @param pheno Tibble returned by \code{bc_prepare_pheno_file}.
#' @param st_dir Path to statTarget working directory.
#' @return List with \code{metabolite_map} tibble.
bc_prepare_profile_file <- function(data, metabolite_cols, pheno, st_dir) {
  profile <- data %>%
    dplyr::select(sample_name, dplyr::all_of(metabolite_cols))
  # Carry batch / class / synthetic_qc / order through the join so we can
  # fill in synthetic boundary QC rows via per-batch extrapolation. Without
  # this, those rows stay NA for every metabolite and (a) bias the QCRFSC
  # model and (b) trigger all-feature filtering when Frule is high.
  join_keys <- c("sample", "sample_name", "batch", "class",
                 "synthetic_qc", "order")
  join_keys <- intersect(join_keys, colnames(pheno))
  ordered_full <- pheno %>%
    dplyr::select(dplyr::all_of(join_keys)) %>%
    dplyr::left_join(profile, by = "sample_name")

  if ("synthetic_qc" %in% colnames(ordered_full) &&
      any(ordered_full$synthetic_qc, na.rm = TRUE)) {
    ordered_full <- bc_populate_synthetic_qc_values(ordered_full, metabolite_cols)
  }

  ordered <- ordered_full %>%
    dplyr::select(sample, dplyr::all_of(metabolite_cols))
  sample_ids <- as.character(ordered$sample)
  numeric_mat <- as.matrix(ordered[, metabolite_cols, drop = FALSE])
  storage.mode(numeric_mat) <- "double"
  transposed_mat <- t(numeric_mat)
  profile_matrix <- tibble::as_tibble(
    cbind(nms = metabolite_cols, as.data.frame(transposed_mat, stringsAsFactors = FALSE)),
    .name_repair = "minimal"
  )
  colnames(profile_matrix) <- c("sample", sample_ids)
  if (anyDuplicated(colnames(profile_matrix))) {
    dupes <- colnames(profile_matrix)[duplicated(colnames(profile_matrix))]
    stop("Duplicate metabolite names detected after transpose: ",
         paste(head(dupes, 5), collapse = ", "), call. = FALSE)
  }
  profile_matrix <- profile_matrix %>%
    dplyr::rename(name = sample) %>%
    dplyr::filter(name != "sample") %>%
    dplyr::mutate(dplyr::across(-name, as.numeric))
  metabolite_map <- tibble::tibble(
    name = profile_matrix$name,
    metabolite_code = paste0("M", seq_len(nrow(profile_matrix))))
  profile_coded <- metabolite_map %>%
    dplyr::left_join(profile_matrix, by = "name") %>%
    dplyr::select(-name) %>%
    dplyr::rename(name = metabolite_code)
  readr::write_csv(profile_coded, file.path(st_dir, "ProfileFile.csv"))
  list(metabolite_map = metabolite_map)
}

# Batch Correction Execution ----
#' Run statTarget Batch Correction
#' @keywords internal
#' @param st_dir Path to statTarget working directory.
#' @param method,ntree,coCV,Frule,imputeM See \code{batchCorrectR}.
#' @return Tibble of raw corrected data from statTarget.
bc_run_batch_correction <- function(st_dir, method, ntree, coCV, Frule, imputeM) {
  samPeno <- file.path(st_dir, "PhenoFile.csv")
  samFile <- file.path(st_dir, "ProfileFile.csv")
  cur_wd <- getwd()
  on.exit(setwd(cur_wd), add = TRUE)
  setwd(st_dir)
  statTarget::shiftCor(samPeno = samPeno, samFile = samFile, Frule = Frule,
                        ntree = ntree, MLmethod = method, imputeM = imputeM,
                        plot = FALSE, coCV = coCV)
  message("    statTarget::shiftCor completed successfully.")
  corrected_path <- file.path(st_dir, "statTarget", "shiftCor",
                               "After_shiftCor", "shift_all_cor.csv")
  if (!file.exists(corrected_path))
    stop("batchCorrectR: statTarget output not found at:\n  ", corrected_path)
  message("    Reading corrected data from: ", corrected_path)
  readr::read_csv(corrected_path, show_col_types = FALSE)
}

# Output Cleaning ----
#' Clean statTarget Correction Output
#' @keywords internal
#' @param corrected_raw Tibble of raw statTarget output.
#' @param metabolite_map Tibble mapping metabolite codes to original names.
#' @param pheno Tibble with sample ID to sample_name mapping.
#' @return Tibble with sample_name and corrected metabolite columns.
bc_clean_correction_output <- function(corrected_raw, metabolite_map, pheno) {
  cleaned <- bc_detect_stattarget_format(corrected_raw)
  code_to_name <- stats::setNames(metabolite_map$name, metabolite_map$metabolite_code)
  cleaned$name <- ifelse(cleaned$name %in% names(code_to_name),
                          code_to_name[cleaned$name], cleaned$name)
  met_names <- cleaned$name
  # Validate all value columns are numeric before transpose
  non_numeric_cols <- names(cleaned)[!vapply(cleaned, is.numeric, logical(1))]
  non_numeric_cols <- setdiff(non_numeric_cols, c("name", "sample"))
  if (length(non_numeric_cols) > 0) {
    warning("Non-numeric columns found before transpose: ",
            paste(head(non_numeric_cols, 5), collapse = ", "),
            ". Attempting conversion.", call. = FALSE)
    # Convert non-numeric columns, then check whether any new NAs appeared
    # (i.e. values that could not be coerced), and warn specifically about those.
    for (col in non_numeric_cols) {
      original_na <- is.na(cleaned[[col]])
      cleaned[[col]] <- suppressWarnings(as.numeric(cleaned[[col]]))
      new_na <- is.na(cleaned[[col]]) & !original_na
      if (any(new_na)) {
        warning("bc_clean_correction_output: coercion of column '", col,
                "' introduced ", sum(new_na), " new NA value(s) for ",
                "unconvertible string(s).", call. = FALSE)
      }
    }
  }
  transposed <- cleaned %>% dplyr::select(-name) %>% t() %>% as.data.frame()
  colnames(transposed) <- met_names
  transposed$sample <- rownames(transposed)
  rownames(transposed) <- NULL
  result <- tibble::as_tibble(transposed, .name_repair = "minimal") %>%
    dplyr::relocate(sample, .before = 1) %>%
    dplyr::mutate(dplyr::across(-sample, as.numeric))
  sample_map <- pheno %>% dplyr::select(sample, sample_name) %>% dplyr::distinct()
  result %>%
    dplyr::left_join(sample_map, by = "sample") %>%
    dplyr::select(-sample) %>%
    dplyr::relocate(sample_name, .before = 1)
}

# Mean Adjustment ----
#' Adjust Corrected QC Means to Original Scale
#' @keywords internal
#' @param corrected_clean Tibble of corrected data.
#' @param data_flagged Original (flagged) data.frame.
#' @param qc_label Character identifying QC samples.
#' @param metabolite_cols Character vector of metabolite column names.
#' @return Tibble with mean-adjusted corrected values.
bc_adjust_corrected_means <- function(corrected_clean, data_flagged, qc_label,
                                      metabolite_cols) {
  available_mets <- intersect(metabolite_cols, colnames(corrected_clean))
  if (length(available_mets) == 0) {
    warning("batchCorrectR: No matching metabolite columns for mean adjustment.")
    return(corrected_clean)
  }
  qc_orig <- data_flagged[tolower(data_flagged$sample_type) == tolower(qc_label),
                           available_mets, drop = FALSE]
  orig_means <- colMeans(qc_orig, na.rm = TRUE)
  if (anyDuplicated(data_flagged$sample_name)) {
    dupes <- data_flagged$sample_name[duplicated(data_flagged$sample_name)]
    warning("Duplicate sample_name(s) in flagged data: ",
            paste(head(dupes, 3), collapse = ", "),
            ". Taking distinct rows.", call. = FALSE)
    data_flagged <- dplyr::distinct(data_flagged, sample_name, .keep_all = TRUE)
  }
  corrected_wt <- corrected_clean %>%
    dplyr::left_join(data_flagged %>% dplyr::select(sample_name, sample_type),
                      by = "sample_name")
  qc_corr <- corrected_wt[tolower(corrected_wt$sample_type) == tolower(qc_label),
                            available_mets, drop = FALSE]
  corr_means <- colMeans(qc_corr, na.rm = TRUE)
  ratios <- bc_compute_mean_ratios(orig_means, corr_means)
  bc_apply_mean_ratios(corrected_clean, ratios)
}

# Output Reconstruction ----
#' Reconstruct Output in Original Data Format
#' @keywords internal
#' @param corrected_adjusted Tibble with corrected metabolite values.
#' @param original_data The original input data.frame.
#' @param metabolite_cols Character vector of metabolite column names.
#' @return Tibble matching the structure of the original data.
bc_reconstruct_output <- function(corrected_adjusted, original_data, metabolite_cols) {
  # meta_cols is derived by exclusion (all columns NOT in the canonical
  # metabolite_cols list). This relies on string equality: if original_data
  # uses user-schema column names (e.g. sample_plate_id instead of batch) and
  # those names also appear in metabolite_cols, they would be mis-classified.
  # In practice the canonical metabolite list never contains metadata names,
  # so this is safe; but be aware when passing non-canonical original_data.
  meta_cols <- setdiff(colnames(original_data), metabolite_cols)
  output <- tibble::as_tibble(original_data[, meta_cols, drop = FALSE])
  available_mets <- intersect(metabolite_cols, colnames(corrected_adjusted))
  output <- output %>%
    dplyr::left_join(corrected_adjusted %>%
                       dplyr::select(sample_name, dplyr::all_of(available_mets)),
                     by = "sample_name")
  dropped_mets <- setdiff(metabolite_cols, available_mets)
  if (length(dropped_mets) > 0) {
    message("batchCorrectR: ", length(dropped_mets),
            " feature(s) were dropped by statTarget (exceeded coCV threshold)",
            " and reverted to original uncorrected values: ",
            paste(dropped_mets, collapse = ", "))
    warning("batchCorrectR: the following ", length(dropped_mets),
            " column(s) in corrected_data contain UNCORRECTED values ",
            "(statTarget dropped them): ",
            paste(utils::head(dropped_mets, 10), collapse = ", "),
            if (length(dropped_mets) > 10) paste0(" ... [", length(dropped_mets) - 10, " more]") else "",
            call. = FALSE)
  }
  for (met in dropped_mets)
    output[[met]] <- original_data[[met]]
  output %>% dplyr::select(dplyr::all_of(colnames(original_data)))
}

# Correction Summary ----
#' Build Correction Summary Table
#' @keywords internal
#' @param metabolite_cols Character vector of metabolite column names.
#' @param qc_rsd_before,qc_rsd_after Named numeric vectors of QC RSD.
#' @return Tibble with per-metabolite correction statistics.
bc_build_correction_summary <- function(metabolite_cols, qc_rsd_before, qc_rsd_after) {
  if (!is.null(names(qc_rsd_before)) && !setequal(names(qc_rsd_before), metabolite_cols)) {
    warning("bc_build_correction_summary: names(qc_rsd_before) do not match ",
            "metabolite_cols; indexing may produce NA values.", call. = FALSE)
  }
  if (!is.null(names(qc_rsd_after)) && !setequal(names(qc_rsd_after), metabolite_cols)) {
    warning("bc_build_correction_summary: names(qc_rsd_after) do not match ",
            "metabolite_cols; indexing may produce NA values.", call. = FALSE)
  }
  tibble::tibble(
    metabolite = metabolite_cols,
    rsd_before = qc_rsd_before[metabolite_cols],
    rsd_after = qc_rsd_after[metabolite_cols],
    rsd_change = qc_rsd_after[metabolite_cols] - qc_rsd_before[metabolite_cols],
    improved = qc_rsd_after[metabolite_cols] < qc_rsd_before[metabolite_cols])
}

# Report Generation ----
#' Generate Correction Report
#' @keywords internal
#' @param correction_summary Per-metabolite stats tibble.
#' @param failed_qc Character vector of failed QC sample names.
#' @param method,n_samples,n_batches,n_metabolites Scalar parameters.
#' @return List containing report sections.
bc_generate_correction_report <- function(correction_summary, failed_qc, method,
                                          n_samples, n_batches, n_metabolites) {
  list(
    title = "batchCorrectR Interbatch Correction Report",
    timestamp = Sys.time(),
    parameters = list(method = method, n_samples = n_samples,
                       n_batches = n_batches, n_metabolites = n_metabolites,
                       n_failed_qc = length(failed_qc)),
    results = list(
      n_improved = sum(correction_summary$improved, na.rm = TRUE),
      n_worsened = sum(!correction_summary$improved & !is.na(correction_summary$improved)),
      median_rsd_before = round(stats::median(correction_summary$rsd_before, na.rm = TRUE), 2),
      median_rsd_after = round(stats::median(correction_summary$rsd_after, na.rm = TRUE), 2)),
    failed_qc_samples = failed_qc,
    top_improved = correction_summary %>%
      dplyr::filter(improved) %>% dplyr::arrange(rsd_change) %>% utils::head(10),
    worst_affected = correction_summary %>%
      dplyr::filter(!improved) %>% dplyr::arrange(dplyr::desc(rsd_change)) %>%
      utils::head(10))
}

# Plotting ----
#' Generate Correction Visualisations
#' @keywords internal
#' @param original_data,corrected_data Data.frames before/after correction.
#' @param qc_label Character identifying QC samples.
#' @param metabolite_cols Character vector of metabolite column names.
#' @param qc_rsd_before,qc_rsd_after Named numeric vectors of QC RSD.
#' @return Named list of ggplot objects.
bc_plot_correction_results <- function(original_data, corrected_data, qc_label,
                                       metabolite_cols, qc_rsd_before,
                                       qc_rsd_after) {
  plots <- list()
  # RSD comparison
  rsd_df <- tibble::tibble(metabolite = metabolite_cols,
    Before = qc_rsd_before[metabolite_cols],
    After = qc_rsd_after[metabolite_cols]) %>%
    tidyr::pivot_longer(c("Before", "After"), names_to = "stage", values_to = "rsd") %>%
    dplyr::mutate(stage = factor(stage, levels = c("Before", "After")))
  plots$rsd_comparison <- ggplot2::ggplot(rsd_df,
    ggplot2::aes(x = stats::reorder(metabolite, rsd), y = rsd, fill = stage)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::geom_hline(yintercept = 30, linetype = "dashed", colour = "red") +
    ggplot2::labs(title = "QC %RSD Before vs After Batch Correction",
                   x = "Metabolite", y = "%RSD", fill = "Stage") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7))
  # Run-order plots
  n_plot <- min(6, length(metabolite_cols))
  top_mets <- names(sort(qc_rsd_before, decreasing = TRUE))[seq_len(n_plot)]
  ro_data <- dplyr::bind_rows(
    original_data %>%
      dplyr::select(sample_name, batch, sample_type, run_order,
                     dplyr::all_of(top_mets)) %>%
      tidyr::pivot_longer(dplyr::all_of(top_mets), names_to = "metabolite",
                           values_to = "value") %>%
      dplyr::mutate(stage = "Before"),
    corrected_data %>%
      dplyr::select(sample_name, batch, sample_type, run_order,
                     dplyr::all_of(top_mets)) %>%
      tidyr::pivot_longer(dplyr::all_of(top_mets), names_to = "metabolite",
                           values_to = "value") %>%
      dplyr::mutate(stage = "After")) %>%
    dplyr::mutate(stage = factor(stage, levels = c("Before", "After")),
                   is_qc = tolower(sample_type) == tolower(qc_label))
  plots$run_order <- ggplot2::ggplot(ro_data,
    ggplot2::aes(x = run_order, y = value, colour = is_qc)) +
    ggplot2::geom_point(size = 1.2, alpha = 0.7) +
    ggplot2::facet_wrap(~ metabolite + stage, scales = "free_y", ncol = 2) +
    ggplot2::scale_color_manual(values = c("TRUE" = "#E41A1C", "FALSE" = "#377EB8"),
      labels = c("TRUE" = "QC", "FALSE" = "Sample"), name = "Type") +
    ggplot2::labs(title = "Run Order: Before vs After Correction",
                   x = "Run Order", y = "Signal Intensity") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 7))
  # PCA
  plots$pca <- bc_plot_pca(original_data, corrected_data, metabolite_cols)
  plots
}

#' Generate PCA Score Plots
#' @keywords internal
#' @param original_data,corrected_data Data.frames before/after correction.
#' @param metabolite_cols Character vector of metabolite column names.
#' @return A ggplot object or NULL.
bc_plot_pca <- function(original_data, corrected_data, metabolite_cols) {
  run_pca <- function(data, label) {
    mat <- as.matrix(data[, metabolite_cols, drop = FALSE])
    col_vars <- apply(mat, 2, function(x) stats::var(x, na.rm = TRUE))
    mat <- mat[, !is.na(col_vars) & col_vars > 0, drop = FALSE]
    if (ncol(mat) < 2) return(NULL)
    n_na <- sum(is.na(mat))
    if (n_na > 0) {
      frac <- round(100 * n_na / length(mat), 1)
      message("    bc_plot_pca: imputing ", n_na, " NA value(s) (",
              frac, "%) with column means before PCA (", label, ").")
      col_means <- colMeans(mat, na.rm = TRUE)
      na_idx <- is.na(mat)
      mat[na_idx] <- col_means[col(mat)[na_idx]]
      # After imputation a previously variable column may have collapsed to a
      # constant (all-NA column imputed to its mean). Re-filter to avoid
      # passing a zero-SD column to scale(), which would produce NaN.
      col_vars_post <- apply(mat, 2, function(x) stats::var(x, na.rm = TRUE))
      mat <- mat[, !is.na(col_vars_post) & col_vars_post > 0, drop = FALSE]
      if (ncol(mat) < 2) return(NULL)
    }
    pca_res <- stats::prcomp(scale(mat), center = FALSE, scale. = FALSE)
    tibble::as_tibble(pca_res$x[, 1:min(2, ncol(pca_res$x))]) %>%
      dplyr::mutate(sample_name = data$sample_name, batch = as.factor(data$batch),
                     sample_type = data$sample_type, stage = label)
  }
  pca_before <- run_pca(original_data, "Before")
  pca_after <- run_pca(corrected_data, "After")
  if (is.null(pca_before) || is.null(pca_after)) return(NULL)
  pca_all <- dplyr::bind_rows(pca_before, pca_after) %>%
    dplyr::mutate(stage = factor(stage, levels = c("Before", "After")))
  ggplot2::ggplot(pca_all,
    ggplot2::aes(x = PC1, y = PC2, colour = batch, shape = sample_type)) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::facet_wrap(~ stage) +
    ggplot2::labs(title = "PCA: Before vs After Batch Correction",
                   x = "PC1", y = "PC2", colour = "Batch", shape = "Sample Type") +
    ggplot2::theme_bw()
}

# ComBat Batch Correction ----
# NOTE: bc_run_combat has been moved to batch_shared_utils.R.
# It is available package-wide and does not need to be redefined here.

# Save to Project Directory ----
#' Save Batch Correction Results to a Project Directory
#'
#' Writes corrected data and correction summary CSVs into a
#' \code{batch_correction} subfolder of the specified project directory.
#'
#' @param result The result list from \code{batchCorrectR}.
#' @param project_dir Character. Path to the project directory.
#' @return Invisibly returns the output directory path.
#' @keywords internal
bc_save_to_project <- function(result, project_dir) {
  bc_dir <- file.path(project_dir, "batch_correction")
  if (!dir.exists(bc_dir)) dir.create(bc_dir, recursive = TRUE)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  corrected_path <- file.path(bc_dir, paste0(timestamp, "_corrected_data.csv"))
  readr::write_csv(result$corrected_data, corrected_path)
  message("  Corrected data saved to: ", corrected_path)

  summary_path <- file.path(bc_dir, paste0(timestamp, "_correction_summary.csv"))
  readr::write_csv(result$correction_summary, summary_path)
  message("  Correction summary saved to: ", summary_path)

  invisible(bc_dir)
}

# HTML Report Export ----

#' Export batchCorrectR HTML Report
#'
#' Renders an interactive HTML report replicating the GUI Results Explorer,
#' including RSD comparison, PCA plots, run-order signal drift, heatmap,
#' and correction summary tables.
#'
#' @param result The result list returned by \code{\link{batchCorrectR}}.
#' @param original_data The original (uncorrected) data frame that was passed
#'   to \code{batchCorrectR}. Required for before-correction PCA and run-order
#'   plots. If a list of plates was used, pass the combined data frame.
#' @param qc_label Character. The label identifying QC samples (default \code{"qc"}).
#' @param output_file Character. Path for the output HTML file. Defaults to
#'   \code{"batchCorrectR_report.html"} in the current working directory.
#' @param open Logical. Whether to open the report in the browser after
#'   rendering. Default \code{TRUE} in interactive sessions.
#'
#' @return Invisibly returns the path to the rendered HTML file.
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' result <- batchCorrectR(my_data, qc_label = "PQC")
#' bc_export_html_report(result, original_data = my_data, qc_label = "PQC")
#' }
bc_export_html_report <- function(result,
                                  original_data = NULL,
                                  qc_label = "qc",
                                  output_file = "batchCorrectR_report.html",
                                  open = interactive()) {

  # Argument-shape checks first so users with bad input see a clear error
  # even when rmarkdown/DT are not installed. Render dependencies below.
  if (is.null(result$correction_summary)) {
    stop("'result' does not contain a correction_summary. ",
         "Pass the return value of batchCorrectR().", call. = FALSE)
  }

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render the report. ",
         "Install it with: install.packages('rmarkdown')", call. = FALSE)
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required for interactive tables. ",
         "Install it with: install.packages('DT')", call. = FALSE)
  }

  # Combine list input if needed
  if (!is.null(original_data) && is.list(original_data) && !is.data.frame(original_data)) {
    original_data <- dplyr::bind_rows(original_data)
  }

  template <- system.file("rmd", "batchCorrectR_report.Rmd", package = "MStargetR")
  if (!nzchar(template)) {
    stop("Report template not found. Reinstall MStargetR.", call. = FALSE)
  }

  # Resolve absolute output path
  output_dir <- normalizePath(dirname(output_file), mustWork = FALSE)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_name <- basename(output_file)

  # Render with self_contained = FALSE so pandoc never builds the giant
  # base64-embedded HTML in one go. On large cohorts pandoc's --embed-resources
  # pass can exhaust the Windows commit limit ("VirtualAlloc MEM_COMMIT
  # failed"); the streaming post-processor below produces an equivalent
  # single-file HTML without that peak memory pressure.
  rendered <- rmarkdown::render(
    input = template,
    output_file = output_name,
    output_dir = output_dir,
    output_options = list(self_contained = FALSE),
    params = list(
      result = result,
      original_data = original_data,
      qc_label = qc_label
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  if (is.character(rendered) && length(rendered) == 1L &&
      file.exists(rendered)) {
    tryCatch(
      embed_resources_streaming(rendered),
      error = function(e) {
        warning(
          "bc_export_html_report: resource embedding failed: ",
          conditionMessage(e),
          ". The report's _files/ sidecar directory has been kept and must ",
          "be distributed alongside the HTML.",
          call. = FALSE
        )
      }
    )
  }

  message("Report saved to: ", rendered)
  if (open) utils::browseURL(rendered)
  invisible(rendered)
}
