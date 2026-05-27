#' Standalone Interbatch Correction for Targeted LC-MS Data
#'
#' Performs signal drift and interbatch correction on targeted LC-MS data using
#' QC sample-based methods from the \code{statTarget} package. This function
#' operates independently of the \code{qcCheckR} pipeline and accepts a simple
#' data.frame input that any user can prepare.
#'
#' @param data A data.frame, tibble, or a \strong{list} of data.frames/tibbles
#'   to combine. Each data.frame must have samples as rows and must contain
#'   either the canonical columns (\code{sample_name}, \code{batch},
#'   \code{sample_type}, \code{run_order}) or the MStargetR column convention
#'   (\code{sample_name}, \code{sample_plate_id}, \code{sample_type_factor},
#'   \code{sample_run_index}). When a list is supplied the data.frames are
#'   row-bound before correction. Column \code{sample_type_factor} is used
#'   for QC matching against \code{qc_label} when present.
#' @param qc_label Character string identifying QC samples. Matched against
#'   \code{sample_type_factor} when present, otherwise \code{sample_type}.
#'   Default is \code{"qc"}.
#' @param method Correction method.
#'   One of \code{"QCRFSC"} (QC-based random forest signal correction, default),
#'   \code{"ComBat"} (empirical Bayes, QC-free), or \code{"QCRLSC"} (QC-based
#'   robust LOESS signal correction; Dunn et al. 2011, via the \code{qcrlscR}
#'   package). Like \code{"QCRFSC"}, \code{"QCRLSC"} requires QC samples.
#' @param ntree Integer. Number of trees for the random forest method. Default
#'   is \code{500}. Ignored when \code{method} is not \code{"QCRFSC"}.
#' @param coCV Numeric. Maximum percent RSD (coefficient of variation) cutoff
#'   passed to \code{statTarget::shiftCor}. Features whose QC RSD exceeds this
#'   cutoff are dropped by statTarget; those features are then reverted to their
#'   original uncorrected values and a message is emitted listing them. Default
#'   is \code{100}, meaning features with QC RSD greater than 100\% are dropped.
#'   Use \code{Inf} for truly no filtering (no features will be dropped).
#' @param Frule Numeric. Proportion in \code{[0, 1]} (e.g., \code{0.8} for
#'   80\%) used by statTarget as the filtering rule for missing values.
#'   Default is \code{0} (no filtering).
#' @param imputeM Character. Imputation method for missing values inside
#'   statTarget. One of \code{"minHalf"} (default), \code{"median"},
#'   \code{"mean"}, or \code{"knn"}.
#' @param combat_par.prior Logical. If TRUE (default), use parametric empirical
#'   Bayes adjustments in ComBat. If FALSE, use non-parametric. Only used when
#'   \code{method = "ComBat"}.
#' @param combat_mean.only Logical. If TRUE, only correct the mean of the batch
#'   effect (no scale/variance adjustment). Default is FALSE. Only used when
#'   \code{method = "ComBat"}.
#' @param combat_ref.batch Optional character string. If provided, use this
#'   batch as the reference for ComBat adjustment. Default is NULL. Only used
#'   when \code{method = "ComBat"}. Must match a value present in the column
#'   selected by \code{batch_column} (or in \code{sample_plate_id}/\code{batch}
#'   when \code{batch_column} is NULL).
#' @param qcrlsc_method Character. QC-RLSC scaling, one of \code{"subtract"}
#'   (default) or \code{"divide"}. \code{"subtract"} matches the Dunn et al.
#'   protocol but can yield small negative values for low-abundance features;
#'   \code{"divide"} preserves non-negativity (better for concentrations) but
#'   is less stable when the fitted QC trend nears zero. Only used when
#'   \code{method = "QCRLSC"}.
#' @param qcrlsc_intra Logical. If TRUE, correct within each batch
#'   (intra-batch); if FALSE (default), correct across batches (inter-batch).
#'   Only meaningful with two or more batches. Only used when
#'   \code{method = "QCRLSC"}.
#' @param qcrlsc_opti Logical. If TRUE (default), optimise the LOESS span by
#'   generalised cross-validation. Only used when \code{method = "QCRLSC"}.
#' @param qcrlsc_log10 Logical. If TRUE (default), log10-transform before
#'   fitting (zeros become missing). Only used when \code{method = "QCRLSC"}.
#' @param qcrlsc_outl Logical. If TRUE (default), perform QC outlier detection
#'   before fitting. Only used when \code{method = "QCRLSC"}.
#' @param qcrlsc_shift Logical. If TRUE (default), apply \code{batch.shift} to
#'   re-align batch means after signal correction. Only used when
#'   \code{method = "QCRLSC"}.
#' @param batch_column Optional character. Name of the column in \code{data}
#'   that holds the batch identifier. When \code{NULL} (default) the function
#'   uses \code{sample_plate_id} if present, otherwise \code{batch}. Set this
#'   to drive the correction off an arbitrary user-named column (e.g.
#'   \code{plate}, \code{run_batch}). The chosen column's values are also the
#'   valid choices for \code{combat_ref.batch}.
#' @param sample_tags Optional character vector of sample-type labels to
#'   include in the correction (in addition to the QC label). Matched
#'   case-insensitively against \code{sample_type_factor} when present,
#'   otherwise \code{sample_type}. Rows whose type does not match either
#'   \code{qc_label} or any of \code{sample_tags} are dropped before
#'   correction -- useful for excluding blanks, double blanks, or other
#'   low-signal sample types that would otherwise distort the QCRFSC model.
#'   Default is \code{NULL} (no filtering; every row is kept).
#' @param output_dir Character. Directory path where statTarget writes its
#'   intermediate files. Default is \code{tempdir()}.
#' @param project_dir Character or NULL. If provided, the corrected data CSV
#'   and correction summary are saved into a \code{batch_correction} subfolder
#'   inside this directory. Default is \code{NULL} (no file output).
#' @param plot Logical. Whether to populate \code{result$plots} with the base
#'   before/after correction ggplots (RSD comparison, run-order facet, PCA).
#'   Default \code{TRUE}.
#'
#'   \strong{Deprecated.} The argument is retained for backwards
#'   compatibility but will be removed in a future release. Use
#'   \code{advanced_plots = TRUE} to populate \code{result$plots} with the
#'   full GUI plot set AND save the figures to disk under
#'   \code{<project_dir>/all/figures/batch_corrector/}. Passing \code{plot}
#'   explicitly emits a deprecation warning.
#' @param advanced_plots Logical. When \code{TRUE}, every plot the GUI's
#'   Batch Correction tab renders (RSD comparison, run-order, PCA
#'   before/after, signal drift, RSD by class, per-metabolite RSD) is
#'   attached to \code{result$plots} \emph{and} -- if \code{project_dir}
#'   is supplied -- written to
#'   \code{<project_dir>/all/figures/batch_corrector/} as both a static
#'   \code{.pdf} and an interactive \code{.html}. Default \code{FALSE} --
#'   opt-in so existing scripts behave identically.
#' @param report Logical. Whether to generate an HTML summary report. Default
#'   is \code{TRUE}.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{corrected_data}{A tibble in the same structure as the input
#'     \code{data}, but with corrected metabolite values.}
#'   \item{correction_summary}{A tibble with per-metabolite correction
#'     statistics including RSD before and after correction.}
#'   \item{qc_rsd_before}{A named numeric vector of QC RSD values before
#'     correction for each metabolite.}
#'   \item{qc_rsd_after}{A named numeric vector of QC RSD values after
#'     correction for each metabolite.}
#'   \item{failed_qc}{A character vector of sample names flagged as failed
#'     QC injections (signal less than 10 percent of batch median).}
#'   \item{plots}{A list of ggplot objects (only if \code{plot = TRUE}).}
#'   \item{report}{Logical indicating whether an HTML report was requested.}
#'   \item{report_path}{Path to the rendered HTML report (only if
#'     \code{report = TRUE} and rendering succeeds).}
#' }
#'
#' @details
#' The correction pipeline proceeds as follows:
#' \enumerate{
#'   \item \strong{Input validation}: Checks that required columns exist,
#'     metabolite data is numeric, and QC samples are present in each batch.
#'   \item \strong{Metabolite detection}: Identifies all numeric columns that
#'     are not metadata columns as metabolite features. Excluded metadata columns
#'     are defined in \code{.METADATA_COLS} (see \code{R/batchCorrectR_Utils.R})
#'     and include \code{sample_name}, \code{batch}, \code{sample_type},
#'     \code{run_order}, \code{sample_plate_id}, \code{sample_timestamp},
#'     \code{sample_matrix}, \code{synthetic_qc}, and others.
#'   \item \strong{Row filtering}: If \code{sample_tags} is supplied, rows
#'     whose \code{sample_type} does not match \code{qc_label} or any of the
#'     provided tags are dropped before correction. This step runs before QC
#'     flagging and affects both the correction model and the returned
#'     \code{corrected_data}.
#'   \item \strong{QC flagging}: Flags QC injections where total signal is
#'     less than 10 percent of the batch median, reclassifying them as regular
#'     samples to prevent them from distorting the correction model.
#'   \item \strong{File preparation}: Creates PhenoFile.csv and ProfileFile.csv
#'     in the format required by \code{statTarget::shiftCor}, ensuring QC
#'     samples bookend each batch.
#'   \item \strong{Signal correction}: Runs \code{statTarget::shiftCor} with
#'     the specified method to model and remove systematic signal drift.
#'   \item \strong{Post-correction adjustment}: Adjusts corrected values so
#'     that QC means match their original pre-correction scale, preserving
#'     biological interpretation of absolute values.
#'   \item \strong{Reporting}: Optionally generates before/after RSD
#'     comparison tables and visualisations including run-order plots and PCA.
#' }
#'
#' \code{QCRFSC} (QC-based Random Forest Signal Correction) fits a random
#' forest model to QC samples across run order and applies the learned
#' correction to all samples. It is robust to non-linear drift patterns.
#'
#' @export
#' @importFrom dplyr filter select mutate arrange bind_rows left_join rename
#'   group_by ungroup relocate row_number case_when across all_of pull contains
#'   everything
#' @importFrom tibble tibble as_tibble rownames_to_column add_column
#' @importFrom readr write_csv read_csv
#' @importFrom stats median sd setNames
#' @importFrom statTarget shiftCor
#' @importFrom ggplot2 ggplot aes geom_point labs theme_bw facet_wrap
#'   scale_color_manual theme element_text geom_hline
#'
#' @examples
#' \dontrun{
#' library(MStargetR)
#'
#' # Prepare input data
#' my_data <- data.frame(
#'   sample_name = paste0("S", 1:30),
#'   batch = rep(c("plate1", "plate2"), each = 15),
#'   sample_type = rep(c("qc", "sample", "sample", "sample", "qc"), 6),
#'   run_order = 1:30,
#'   metabolite_A = rnorm(30, mean = 100, sd = 10),
#'   metabolite_B = rnorm(30, mean = 500, sd = 50),
#'   metabolite_C = rnorm(30, mean = 1000, sd = 100)
#' )
#'
#' # Run batch correction with default settings (QCRFSC)
#' result <- batchCorrectR(data = my_data)
#'
#' # Access corrected data
#' corrected <- result$corrected_data
#'
#' # View RSD improvement
#' result$correction_summary
#'
#' # Use ComBat (empirical Bayes, QC-free) instead of the default QCRFSC
#' result_combat <- batchCorrectR(my_data, method = "ComBat")
#'
#' # Use QC-RLSC (QC-based robust LOESS signal correction)
#' result_qcrlsc <- batchCorrectR(my_data, method = "QCRLSC")
#' }
batchCorrectR <- function(data,
                          qc_label = "qc",
                          method = "QCRFSC",
                          ntree = 500,
                          coCV = 100,
                          Frule = 0,
                          imputeM = "minHalf",
                          combat_par.prior = TRUE,
                          combat_mean.only = FALSE,
                          combat_ref.batch = NULL,
                          qcrlsc_method = c("subtract", "divide"),
                          qcrlsc_intra = FALSE,
                          qcrlsc_opti = TRUE,
                          qcrlsc_log10 = TRUE,
                          qcrlsc_outl = TRUE,
                          qcrlsc_shift = TRUE,
                          batch_column = NULL,
                          sample_tags = NULL,
                          output_dir = tempdir(),
                          project_dir = NULL,
                          plot = TRUE,
                          advanced_plots = FALSE,
                          report = TRUE) {

  # Soft deprecation for `plot`: warn only when the user typed it
  # explicitly (positional callers and unchanged defaults are silent).
  if ("plot" %in% names(match.call())) {
    warning(
      "batchCorrectR(): the 'plot' argument is deprecated and will be ",
      "removed in a future release. Use 'advanced_plots = TRUE' to ",
      "build the full GUI plot set and save it under ",
      "<project_dir>/all/figures/batch_corrector/.",
      call. = FALSE
    )
  }

  # Validate advanced_plots early so a typo'd value fails before the long
  # statTarget run, not after.
  if (!is.logical(advanced_plots) || length(advanced_plots) != 1L ||
      is.na(advanced_plots)) {
    stop("batchCorrectR: 'advanced_plots' must be TRUE or FALSE. Got: ",
         deparse(advanced_plots), call. = FALSE)
  }
  # advanced_plots = TRUE forces plot generation regardless of plot=.
  build_plots <- isTRUE(plot) || isTRUE(advanced_plots)

  message("batchCorrectR: Starting interbatch correction pipeline...")

  # Step 0: Preprocess input (combine list, map columns)
  if (is.list(data) && !is.data.frame(data)) {
    original_data_raw <- dplyr::bind_rows(data)
  } else {
    original_data_raw <- data
  }
  # NOTE on two-copy normalisation: original_data_raw is kept in user schema
  # (original column names) so bc_reconstruct_output can return corrected_data
  # in the same format the caller supplied. The canonical working copy `data`
  # is produced by bc_preprocess_input (which strips .mzML, parses timestamps,
  # and renames columns). Both copies must have the same sample_name values so
  # the left_join inside bc_reconstruct_output can match rows.  The raw-copy
  # normalisations below are therefore NOT redundant with bc_preprocess_input:
  # they target original_data_raw only. If you remove either set, stripped-vs-
  # unstripped sample_name keys will cause unmatched rows in the output.
  if ("sample_name" %in% colnames(original_data_raw)) {
    original_data_raw$sample_name <- strip_mzml_suffix(original_data_raw$sample_name)
  }
  if ("sample_timestamp" %in% colnames(original_data_raw)) {
    original_data_raw$sample_timestamp <-
      parse_sample_timestamp(original_data_raw$sample_timestamp)
  }
  if ("sample_timestamp" %in% colnames(original_data_raw) &&
      inherits(original_data_raw$sample_timestamp, "POSIXct")) {
    original_data_raw <- original_data_raw %>%
      dplyr::arrange(sample_timestamp)
    if ("sample_run_index" %in% colnames(original_data_raw)) {
      message("batchCorrectR: overwriting existing 'sample_run_index' column ",
              "with a fresh 1..N sequence derived from sample_timestamp order.")
      original_data_raw$sample_run_index <- seq_len(nrow(original_data_raw))
    }
  }
  data <- bc_preprocess_input(data, batch_column = batch_column)

  # Filter by sample_tags (if supplied). Rows whose sample_type does not
  # match qc_label or any of the tags (case-insensitive) are dropped and
  # also removed from original_data_raw so the reconstructed output stays
  # row-aligned. Lets users exclude blanks / double-blanks that would
  # otherwise contaminate the QCRFSC model with near-zero values.
  if (!is.null(sample_tags)) {
    if (!is.character(sample_tags) || length(sample_tags) == 0 ||
        any(is.na(sample_tags)) || any(!nzchar(sample_tags))) {
      stop("batchCorrectR: 'sample_tags' must be a non-empty character ",
           "vector with no empty strings and no NA values, or NULL. Got: ",
           paste(class(sample_tags), collapse = ", "),
           " of length ", length(sample_tags), call. = FALSE)
    }
    keep_tags <- unique(tolower(c(qc_label, sample_tags)))
    match_col <- if ("sample_type_factor" %in% colnames(data)) {
      data$sample_type_factor
    } else {
      data$sample_type
    }
    na_in_col <- is.na(match_col)
    if (any(na_in_col)) {
      warning("batchCorrectR: NA in sample_type column for ",
              sum(na_in_col), " row(s); these rows will be excluded from ",
              "correction.", call. = FALSE)
    }
    keep_rows <- tolower(match_col) %in% keep_tags
    n_dropped <- sum(!keep_rows)
    if (n_dropped > 0) {
      dropped_types <- unique(as.character(match_col[!keep_rows]))
      message("    sample_tags filter: dropping ", n_dropped,
              " row(s) with sample_type not in [",
              paste(keep_tags, collapse = ", "), "]. ",
              "Excluded types: ",
              paste(dropped_types, collapse = ", "))
    }
    data <- dplyr::ungroup(data)[keep_rows, , drop = FALSE]
    # Align original_data_raw on sample_name so bc_reconstruct_output later
    # only emits rows that survived filtering.
    if ("sample_name" %in% colnames(original_data_raw)) {
      original_data_raw <- dplyr::ungroup(tibble::as_tibble(
        original_data_raw[
          original_data_raw$sample_name %in% data$sample_name, , drop = FALSE]
      ))
    }
    if (nrow(data) == 0) {
      stop("batchCorrectR: sample_tags filter removed every row. ",
           "Check that 'qc_label' and 'sample_tags' match the values in ",
           "your sample_type column.", call. = FALSE)
    }
  }

  # Step 1: Input validation
  message("  [1/8] Validating input data...")

  # Validate output_dir
  if (!is.character(output_dir) || length(output_dir) != 1 || !nzchar(output_dir)) {
    stop("batchCorrectR: 'output_dir' must be a non-empty single character string. Got: ",
         paste(class(output_dir), collapse = ", "), call. = FALSE)
  }

  # Validate project_dir
  if (!is.null(project_dir)) {
    if (!is.character(project_dir) || length(project_dir) != 1 || !nzchar(project_dir)) {
      stop("batchCorrectR: 'project_dir' must be a non-empty single character string or NULL. Got: ",
           paste(class(project_dir), collapse = ", "), call. = FALSE)
    }
    if (!dir.exists(project_dir)) {
      stop("batchCorrectR: 'project_dir' does not exist: ", project_dir, call. = FALSE)
    }
  }

  # Validate plot
  if (!is.logical(plot) || length(plot) != 1 || is.na(plot)) {
    stop("batchCorrectR: 'plot' must be TRUE or FALSE. Got: ",
         deparse(plot), call. = FALSE)
  }

  # Validate report
  if (!is.logical(report) || length(report) != 1 || is.na(report)) {
    stop("batchCorrectR: 'report' must be TRUE or FALSE. Got: ",
         deparse(report), call. = FALSE)
  }

  # Resolve / validate QC-RLSC parameters (only consumed when method=="QCRLSC",
  # but match.arg() and the logical checks run unconditionally so a typo fails
  # fast regardless of the selected method).
  qcrlsc_method <- match.arg(qcrlsc_method)
  for (nm in c("qcrlsc_intra", "qcrlsc_opti", "qcrlsc_log10",
               "qcrlsc_outl", "qcrlsc_shift")) {
    val <- get(nm)
    if (!is.logical(val) || length(val) != 1 || is.na(val)) {
      stop("batchCorrectR: '", nm, "' must be TRUE or FALSE. Got: ",
           deparse(val), call. = FALSE)
    }
  }

  bc_validate_input(data, qc_label, method, ntree, coCV, Frule, imputeM)

  n_batches <- length(unique(data$batch))
  n_qc <- sum(tolower(data$sample_type) == tolower(qc_label))
  n_samples <- nrow(data)
  message("    Input: ", n_samples, " samples, ", n_batches,
          " batch(es), ", n_qc, " QC injections.")
  message("    Method: ", method, ", ntree: ", ntree,
          ", coCV: ", coCV, ", Frule: ", Frule, ", imputeM: ", imputeM)

  # Step 2: Identify metabolite columns
  message("  [2/8] Detecting metabolite columns...")
  metabolite_cols <- bc_detect_metabolite_columns(data)
  message(paste0("    Found ", length(metabolite_cols), " metabolite features."))

  # ComBat path — no QC required
  if (method == "ComBat") {
    message("  [3/8] Skipping QC flagging (ComBat is QC-free).")
    message("  [4/8] Calculating pre-correction QC RSD (if QC samples available)...")

    # Calculate pre-correction RSD if QC samples exist
    has_qc <- any(tolower(data$sample_type) == tolower(qc_label))
    if (has_qc) {
      qc_rsd_before <- bc_calculate_rsd(data, qc_label, metabolite_cols)
    } else {
      qc_rsd_before <- stats::setNames(rep(NA_real_, length(metabolite_cols)), metabolite_cols)
    }

    message("  [5/8] Preparing ComBat input...")
    message("  [6/8] Running sva::ComBat (par.prior = ", combat_par.prior,
            ", mean.only = ", combat_mean.only, ")...")

    corrected_data <- bc_run_combat(
      data = data,
      metabolite_cols = metabolite_cols,
      par.prior = combat_par.prior,
      mean.only = combat_mean.only,
      ref.batch = combat_ref.batch
    )

    message("  [7/8] ComBat correction complete.")

    # Calculate post-correction RSD if QC samples exist
    if (has_qc) {
      qc_rsd_after <- bc_calculate_rsd(corrected_data, qc_label, metabolite_cols)
    } else {
      qc_rsd_after <- stats::setNames(rep(NA_real_, length(metabolite_cols)), metabolite_cols)
    }

    correction_summary <- bc_build_correction_summary(
      metabolite_cols = metabolite_cols,
      qc_rsd_before = qc_rsd_before,
      qc_rsd_after = qc_rsd_after
    )

    # Tag corrected output so downstream consumers can distinguish it from
    # pre-correction data (matches qcCheckR's "concentration.ComBat" tag at
    # R/qcCheckR_Utils.R:563).
    if ("sample_data_source" %in% colnames(corrected_data)) {
      corrected_data$sample_data_source <- "concentration.ComBat"
    }

    # Round metabolite columns to 3 significant figures
    present_mc <- intersect(metabolite_cols, names(corrected_data))
    corrected_data[present_mc] <- lapply(corrected_data[present_mc], signif, digits = 3)

    result <- list(
      corrected_data = corrected_data,
      correction_summary = correction_summary,
      qc_rsd_before = qc_rsd_before,
      qc_rsd_after = qc_rsd_after,
      failed_qc = character(0)
    )

    if (build_plots && has_qc) {
      message("  [8/8] Generating correction plots...")
      result$plots <- bc_plot_correction_results(
        original_data = data,
        corrected_data = corrected_data,
        qc_label = qc_label,
        metabolite_cols = metabolite_cols,
        qc_rsd_before = qc_rsd_before,
        qc_rsd_after = qc_rsd_after
      )
    } else {
      message("  [8/8] Skipping plot generation",
              if (!has_qc) " (no QC samples for comparison)."
              else " (plot = FALSE and advanced_plots = FALSE).")
    }

    if (report) {
      result$report <- bc_generate_correction_report(
        correction_summary = correction_summary,
        failed_qc = character(0),
        method = method,
        n_samples = nrow(data),
        n_batches = length(unique(data$batch)),
        n_metabolites = length(metabolite_cols)
      )
      # Generate HTML report
      report_dir <- if (!is.null(project_dir)) {
        file.path(project_dir, "batch_correction")
      } else {
        output_dir
      }
      result$report_status <- "not_attempted"
      tryCatch({
        result$report_path <- bc_export_html_report(
          result = result,
          original_data = data,
          qc_label = qc_label,
          output_file = file.path(report_dir, "batchCorrectR_report.html"),
          open = FALSE
        )
        result$report_status <- "success"
      }, error = function(e) {
        result$report_status <<- "failed"
        message("  Note: HTML report generation failed: ", e$message)
      })
    }

    improved <- sum(qc_rsd_after < qc_rsd_before, na.rm = TRUE)
    if (has_qc) {
      message(
        "\nbatchCorrectR complete! ",
        improved, "/", length(metabolite_cols),
        " metabolites showed RSD improvement.",
        "\n  Median QC RSD: ",
        round(stats::median(qc_rsd_before, na.rm = TRUE), 1), "% -> ",
        round(stats::median(qc_rsd_after, na.rm = TRUE), 1), "%"
      )
    } else {
      message("\nbatchCorrectR complete! ComBat correction applied to ",
              length(metabolite_cols), " metabolites across ",
              length(unique(data$batch)), " batches.")
    }

    # Save to project directory if specified
    if (!is.null(project_dir)) {
      bc_save_to_project(result, project_dir)
    }

    if (isTRUE(advanced_plots) && has_qc) {
      result <- bc_apply_advanced_plots(result, data, qc_label, project_dir)
    } else if (isTRUE(advanced_plots) && !has_qc) {
      message("  advanced_plots: no QC samples available; skipping ",
              "advanced plot generation.")
    }

    return(result)
  }

  # QC-RLSC path -- requires QC samples (the LOESS trend is fit through them),
  # but mechanically closer to ComBat (a single shared helper, no statTarget
  # intermediate files, no synthetic QC boundaries). qc.rlsc re-centres each
  # feature on its QC mean internally, so NO QC-mean rescaling is applied here.
  if (method == "QCRLSC") {
    message("  [3/8] Flagging failed QC injections...")
    flagging <- bc_flag_failed_qc(data, qc_label, metabolite_cols)
    data <- flagging$data
    failed_qc <- flagging$failed_samples

    if (length(failed_qc) > 0) {
      warning(
        "Flagged ", length(failed_qc), " failed QC injection(s): ",
        paste(failed_qc, collapse = ", "),
        "\n  These will be treated as regular samples during correction."
      )
    }

    # Verify each batch still has >= 2 QC samples after flagging
    for (b in unique(data$batch)) {
      n_qc_b <- sum(data$batch == b &
                      tolower(data$sample_type) == tolower(qc_label))
      if (n_qc_b < 2) {
        stop("batchCorrectR: Batch '", b, "' has only ", n_qc_b,
             " QC sample(s) remaining after flagging failed injections. ",
             "At least 2 QC samples per batch are required for QC-RLSC.",
             call. = FALSE)
      }
    }

    message("  [4/8] Calculating pre-correction QC RSD...")
    qc_rsd_before <- bc_calculate_rsd(data, qc_label, metabolite_cols)

    message("  [5/8] Preparing QC-RLSC input (rows = samples; no transpose)...")
    message("  [6/8] Running qcrlscR::qc.rlsc.wrap (method = ", qcrlsc_method,
            ", intra = ", qcrlsc_intra, ", opti = ", qcrlsc_opti,
            ", log10 = ", qcrlsc_log10, ", outl = ", qcrlsc_outl,
            ", shift = ", qcrlsc_shift, ")...")
    corrected_data <- bc_run_qcrlsc(
      data = data,
      metabolite_cols = metabolite_cols,
      qc_label = qc_label,
      qcrlsc_method = qcrlsc_method,
      intra = qcrlsc_intra,
      opti = qcrlsc_opti,
      log10 = qcrlsc_log10,
      outl = qcrlsc_outl,
      shift = qcrlsc_shift
    )

    message("  [7/8] QC-RLSC correction complete.")
    qc_rsd_after <- bc_calculate_rsd(corrected_data, qc_label, metabolite_cols)

    correction_summary <- bc_build_correction_summary(
      metabolite_cols = metabolite_cols,
      qc_rsd_before = qc_rsd_before,
      qc_rsd_after = qc_rsd_after
    )

    # Tag corrected output so downstream consumers can distinguish it
    # (matches the "concentration.ComBat" convention).
    if ("sample_data_source" %in% colnames(corrected_data)) {
      corrected_data$sample_data_source <- "concentration.QCRLSC"
    }

    # Round metabolite columns to 3 significant figures
    present_mc <- intersect(metabolite_cols, names(corrected_data))
    corrected_data[present_mc] <- lapply(corrected_data[present_mc], signif, digits = 3)

    result <- list(
      corrected_data = corrected_data,
      correction_summary = correction_summary,
      qc_rsd_before = qc_rsd_before,
      qc_rsd_after = qc_rsd_after,
      failed_qc = failed_qc
    )

    if (build_plots) {
      message("  [8/8] Generating correction plots...")
      result$plots <- bc_plot_correction_results(
        original_data = data,
        corrected_data = corrected_data,
        qc_label = qc_label,
        metabolite_cols = metabolite_cols,
        qc_rsd_before = qc_rsd_before,
        qc_rsd_after = qc_rsd_after
      )
    } else {
      message("  [8/8] Skipping plot generation (plot = FALSE and ",
              "advanced_plots = FALSE).")
    }

    if (report) {
      result$report <- bc_generate_correction_report(
        correction_summary = correction_summary,
        failed_qc = failed_qc,
        method = method,
        n_samples = nrow(data),
        n_batches = length(unique(data$batch)),
        n_metabolites = length(metabolite_cols)
      )
      report_dir <- if (!is.null(project_dir)) {
        file.path(project_dir, "batch_correction")
      } else {
        output_dir
      }
      result$report_status <- "not_attempted"
      tryCatch({
        result$report_path <- bc_export_html_report(
          result = result,
          original_data = data,
          qc_label = qc_label,
          output_file = file.path(report_dir, "batchCorrectR_report.html"),
          open = FALSE
        )
        result$report_status <- "success"
      }, error = function(e) {
        result$report_status <<- "failed"
        message("  Note: HTML report generation failed: ", e$message)
      })
    }

    improved <- sum(qc_rsd_after < qc_rsd_before, na.rm = TRUE)
    message(
      "\nbatchCorrectR complete! ",
      improved, "/", length(metabolite_cols),
      " metabolites showed RSD improvement.",
      "\n  Median QC RSD: ",
      round(stats::median(qc_rsd_before, na.rm = TRUE), 1), "% -> ",
      round(stats::median(qc_rsd_after, na.rm = TRUE), 1), "%"
    )

    if (!is.null(project_dir)) {
      bc_save_to_project(result, project_dir)
    }

    if (isTRUE(advanced_plots)) {
      result <- bc_apply_advanced_plots(result, data, qc_label, project_dir)
    }

    return(result)
  }

  # Step 3: Flag failed QC injections
  message("  [3/8] Flagging failed QC injections...")
  flagging <- bc_flag_failed_qc(data, qc_label, metabolite_cols)
  data_flagged <- flagging$data
  failed_qc <- flagging$failed_samples

  if (length(failed_qc) > 0) {
    warning(
      "Flagged ", length(failed_qc), " failed QC injection(s): ",
      paste(failed_qc, collapse = ", "),
      "\n  These will be treated as regular samples during correction."
    )
  }

  # Verify each batch still has >= 2 QC samples after flagging
  for (b in unique(data_flagged$batch)) {
    n_qc <- sum(data_flagged$batch == b &
                  tolower(data_flagged$sample_type) == tolower(qc_label))
    if (n_qc < 2) {
      stop("batchCorrectR: Batch '", b, "' has only ", n_qc,
           " QC sample(s) remaining after flagging failed injections. ",
           "At least 2 QC samples per batch are required for signal drift correction.",
           call. = FALSE)
    }
  }

  # Step 4: Calculate pre-correction QC RSD
  message("  [4/8] Calculating pre-correction QC RSD...")
  qc_rsd_before <- bc_calculate_rsd(data_flagged, qc_label, metabolite_cols)

  # Step 5: Prepare statTarget files and run correction
  message("  [5/8] Preparing statTarget input files...")
  st_dir <- file.path(output_dir, paste0("batchCorrectR_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(st_dir, recursive = TRUE, showWarnings = FALSE)

  pheno <- bc_prepare_pheno_file(data_flagged, qc_label, st_dir)
  profile_result <- bc_prepare_profile_file(data_flagged, metabolite_cols, pheno, st_dir)

  message("  [6/8] Running statTarget::shiftCor (method = ", method, ")...")
  corrected_raw <- bc_run_batch_correction(
    st_dir = st_dir,
    method = method,
    ntree = ntree,
    coCV = coCV,
    Frule = Frule,
    imputeM = imputeM
  )

  # Step 6: Clean output and back-transform
  message("  [7/8] Cleaning correction output and adjusting QC means...")
  corrected_clean <- bc_clean_correction_output(
    corrected_raw = corrected_raw,
    metabolite_map = profile_result$metabolite_map,
    pheno = pheno
  )

  corrected_adjusted <- bc_adjust_corrected_means(
    corrected_clean = corrected_clean,
    data_flagged = data_flagged,
    qc_label = qc_label,
    metabolite_cols = metabolite_cols
  )

  # Reconstruct internal corrected data (canonical column names for RSD/plots)
  corrected_internal <- bc_reconstruct_output(
    corrected_adjusted = corrected_adjusted,
    original_data = data,
    metabolite_cols = metabolite_cols
  )

  # Reconstruct output in same format as input (using original column names)
  corrected_data <- bc_reconstruct_output(
    corrected_adjusted = corrected_adjusted,
    original_data = original_data_raw,
    metabolite_cols = metabolite_cols
  )

  # Tag corrected output so it is distinguishable from pre-correction data
  # (matches qcCheckR's ".peakAreaCorrected" tag at R/qcCheckR_Utils.R:461).
  if ("sample_data_source" %in% colnames(corrected_data)) {
    corrected_data$sample_data_source <- ".peakAreaCorrected"
  }

  # Step 7: Calculate post-correction QC RSD (using canonical column names)
  qc_rsd_after <- bc_calculate_rsd(corrected_internal, qc_label, metabolite_cols)

  # Build correction summary
  correction_summary <- bc_build_correction_summary(
    metabolite_cols = metabolite_cols,
    qc_rsd_before = qc_rsd_before,
    qc_rsd_after = qc_rsd_after
  )

  # Round metabolite columns to 3 significant figures
  present_mc <- intersect(metabolite_cols, names(corrected_data))
  corrected_data[present_mc] <- lapply(corrected_data[present_mc], signif, digits = 3)

  # Step 8: Generate plots and report
  result <- list(
    corrected_data = corrected_data,
    correction_summary = correction_summary,
    qc_rsd_before = qc_rsd_before,
    qc_rsd_after = qc_rsd_after,
    failed_qc = failed_qc
  )

  if (build_plots) {
    message("  [8/8] Generating correction plots...")
    result$plots <- bc_plot_correction_results(
      original_data = data,
      corrected_data = corrected_internal,
      qc_label = qc_label,
      metabolite_cols = metabolite_cols,
      qc_rsd_before = qc_rsd_before,
      qc_rsd_after = qc_rsd_after
    )
  } else {
    message("  [8/8] Skipping plot generation (plot = FALSE and ",
            "advanced_plots = FALSE).")
  }

  if (report) {
    result$report <- bc_generate_correction_report(
      correction_summary = correction_summary,
      failed_qc = failed_qc,
      method = method,
      n_samples = nrow(data),
      n_batches = length(unique(data$batch)),
      n_metabolites = length(metabolite_cols)
    )
    # Generate HTML report
    report_dir <- if (!is.null(project_dir)) {
      file.path(project_dir, "batch_correction")
    } else {
      output_dir
    }
    result$report_status <- "not_attempted"
    tryCatch({
      result$report_path <- bc_export_html_report(
        result = result,
        original_data = data,
        qc_label = qc_label,
        output_file = file.path(report_dir, "batchCorrectR_report.html"),
        open = FALSE
      )
      result$report_status <- "success"
    }, error = function(e) {
      result$report_status <<- "failed"
      message("  Note: HTML report generation failed: ", e$message)
    })
  }

  # Summary message
  improved <- sum(qc_rsd_after < qc_rsd_before, na.rm = TRUE)
  message(
    "\nbatchCorrectR complete! ",
    improved, "/", length(metabolite_cols),
    " metabolites showed RSD improvement.",
    "\n  Median QC RSD: ",
    round(stats::median(qc_rsd_before, na.rm = TRUE), 1), "% -> ",
    round(stats::median(qc_rsd_after, na.rm = TRUE), 1), "%"
  )

  # Save to project directory if specified
  if (!is.null(project_dir)) {
    bc_save_to_project(result, project_dir)
  }

  if (isTRUE(advanced_plots)) {
    result <- bc_apply_advanced_plots(result, data, qc_label, project_dir)
  }

  return(result)
}

# Internal: attach the extended plot bundle to result$plots and (when
# project_dir is supplied) write every plot to disk under
# <project_dir>/all/figures/batch_corrector/. Called from both the
# ComBat and QCRFSC return paths so the behaviour is identical.
bc_apply_advanced_plots <- function(result, data, qc_label, project_dir) {
  message("Collecting advanced batch correction plots ...")
  plots <- tryCatch(
    bc_collect_plots(result, original_data = data, qc_label = qc_label),
    error = function(e) {
      warning("batchCorrectR: advanced_plots collection failed: ",
              conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(plots) || !length(plots)) return(result)

  # Merge into result$plots so users get both ggplot objects (e.g.
  # plots$rsd_comparison$static) and the corresponding plotly widgets.
  result$plots <- plots

  if (!is.null(project_dir)) {
    message("Writing advanced plots to all/figures/batch_corrector/ ...")
    tryCatch(
      save_figure_list(plots, project_dir = project_dir,
                       module = "batch_corrector"),
      error = function(e) {
        warning("batchCorrectR: advanced_plots write failed: ",
                conditionMessage(e), call. = FALSE)
      }
    )
  } else {
    message("  advanced_plots: project_dir is NULL; plots attached to ",
            "result$plots but no files written.")
  }
  result
}
