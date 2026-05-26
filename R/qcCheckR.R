#' QC Assessment and Batch Correction for Targeted LC-MS Data
#'
#' This function performs a series of quality control checks on the data within a specified project directory.
#'
#' Capable of combining multiple cohort and methods if a common long term reference sample has been used throughout and target metabolite naming conventions have been preserved.
#' To allow this feature all methods must be included in the mrm_template_list.
#' Please note only matching metabolite feature names across cohorts/methods will be processed.
#'
#' If you have not used the MStargetR::PeakForgeR function to generate reports please ensure your report file names contains `_PeakForgeR_` to ensure the function can correctly identify the files in your project directory.
#'
#' @param project_directory A character string specifying the path to the project directory.
#' @param mrm_template_list A list of MRM templates and associated concentration guide. Must have specific column names. See examples for structure of mrm_template_list. Must include mrm_guide labelled as "SIL_guide" and associated concentration guide labelled as "conc_guide". Can contain multiple combinations stored as separate lists, see examples.
#' @param QC_sample_label A character string containing the key tags to filter QC samples from file names.  E.g. "qc".
#' @param sample_tags A character vector specifying the tags to filter sample types from file names. E.g. c("sample","control", "qc").
#' @param user_name A character string specifying the name of the user.
#' @param mv_threshold A numeric value  between 0 and 100 specifying the threshold for missing values in the data. Default is 50(50%).
#' @param batch_method Character string specifying the batch correction method.
#'   One of \code{"QCRFSC"} (random forest, default) or \code{"ComBat"}
#'   (empirical Bayes, QC-free).
#' @param batch_ntree Integer. Number of trees for the random forest method.
#'   Default is \code{500}. Ignored when \code{batch_method} is not \code{"QCRFSC"}.
#' @param batch_coCV Numeric. Coefficient of variation cutoff (percentage,
#'   1--100) for feature filtering inside statTarget. Features with QC CV above
#'   this threshold are removed. Default is \code{100} (effectively no filtering).
#' @param batch_Frule Numeric. Filtering rule (0-1) for missing values inside
#'   statTarget. Default is \code{0} (no filtering).
#' @param batch_imputeM Character string. Imputation method for missing values.
#'   One of \code{"minHalf"} (default), \code{"median"}, \code{"mean"}, or \code{"knn"}.
#' @param combat_par.prior Logical. If TRUE (default), use parametric empirical
#'   Bayes adjustments. Only used when \code{batch_method = "ComBat"}.
#' @param combat_mean.only Logical. If TRUE, only correct the mean of the batch
#'   effect. Default is FALSE. Only used when \code{batch_method = "ComBat"}.
#' @param combat_ref.batch Optional character string specifying a reference
#'   batch. Default is NULL. Only used when \code{batch_method = "ComBat"}.
#'   Must match a value in the column selected by \code{batch_column} (or in
#'   \code{sample_plate_id} when \code{batch_column} is NULL).
#' @param batch_column Optional character. Name of the column in the
#'   imputed concentration data that holds the batch identifier used by
#'   ComBat. When \code{NULL} (default) the canonical \code{sample_plate_id}
#'   column is used. Set this to drive the correction off an arbitrary
#'   user-named column (e.g. \code{plate}, \code{run_batch}); the chosen
#'   column's values become the valid choices for \code{combat_ref.batch}.
#'   Only used when \code{batch_method = "ComBat"}.
#' @param write_rda Logical. When \code{TRUE} (default) the master_list
#'   \code{.qs2} file is written synchronously as the final step of the
#'   pipeline. Set to \code{FALSE} when the caller intends to write it
#'   out-of-band — for example, the Shiny GUI passes \code{FALSE} so it
#'   can surface results to the user immediately and fire a separate
#'   background job that calls
#'   \code{\link{export_master_list_qs}} on the returned master_list. The
#'   XLSX and HTML exports are unaffected. The argument name is retained
#'   from the previous \code{.rda} API to avoid churning every caller;
#'   the underlying output is now \code{.qs2}.
#' @param qs_nthreads Integer worker threads forwarded to
#'   \code{\link[qs2]{qs_save}}. Defaults to
#'   \code{max(1L, parallel::detectCores() - 1L)}. Multi-threaded zstd is
#'   what makes large-cohort saves complete in reasonable time; the
#'   previous single-threaded gzip path via \code{base::save()} stalled
#'   for hours on a 54-plate cohort.
#' @param qs_compress_level Integer zstd compression level forwarded to
#'   \code{\link[qs2]{qs_save}}. Default \code{3L} (qs2 default; fast
#'   with good ratio). Higher values (up to 22) shrink the file further
#'   at the cost of CPU time; negative values trade ratio for more
#'   speed.
#' @param advanced_plots Logical. When \code{TRUE}, every plot the GUI's
#'   QC Check tab renders (PCA scores, run-order, per-metabolite control
#'   charts, %RSD histogram, missing values, sample-type pie, plate
#'   distribution) is also written to
#'   \code{<project_directory>/all/figures/qcCheckR/} as both a static
#'   \code{.pdf} (via \code{ggplot2::ggsave}) and an interactive
#'   \code{.html} (via \code{htmlwidgets::saveWidget} on the plotly
#'   widget). Default \code{FALSE} -- opt-in so existing scripts continue
#'   to behave identically.
#' @param date_order Controls how the \code{AcquiredTime} column from
#'   PeakForgeR reports (which Skyline exports in the OS locale of whoever
#'   ran the export) is parsed. One of \code{"auto"} (default; the
#'   pipeline inspects the cohort, prefers mzML \code{startTimeStamp}
#'   ISO 8601 headers where available, and chooses an unambiguous order
#'   from the cohort's parse pattern and any \code{_YYYYMMDD$} plate-name
#'   hints), \code{"dmy"} (day-first slash/dash formats), \code{"mdy"}
#'   (month-first slash/dash formats), or \code{"ymd"} / \code{"iso"}
#'   (ISO 8601 only). If \code{"auto"} cannot resolve the format
#'   unambiguously, the pipeline stops with a clear message asking you to
#'   set this argument explicitly rather than silently produce wrong
#'   dates.
#' @return A list containing the processed data and generated reports.
#' @export
#' @examples
#' \dontrun{
#'
#' library(MStargetR)
#'
#' #Load example mrm_template_list
#'   file_path <- system.file("extdata",
#'                            "LGW_lipid_mrm_template_v1.tsv",
#'                            package = "MStargetR")
#'
#'   sample_metadata_example <- readr::read_tsv(file_path)
#'
#' #Load example conc_guide
#'   file_path <- system.file("extdata",
#'                            "LGW_SIL_batch_Ultimate_2023_03_06.tsv",
#'                            package = "MStargetR")
#'
#'   conc_guide_example <- readr::read_tsv(file_path)
#'
#' #Load example report file
#'   file_path <- system.file("extdata",
#'                            "Example_PeakForgeR_report.csv",
#'                            package = "MStargetR")
#'
#'   report_file <- read.csv(file_path)
#'
#' # Using QCRFSC (default, requires QC samples)
#' qcCheckR(user_name = "user1",
#'          project_directory = "path/to/project_directory",
#'          mrm_template_list = list(v1 = list(
#'                                     SIL_guide = "path/to/mrm_guide1.tsv",
#'                                     conc_guide = "path/to/SIL_concentration_guide1.tsv")),
#'          QC_sample_label = "qc",
#'          sample_tags = c("sample", "control", "blank", "qc"),
#'          mv_threshold = 50,
#'          batch_method = "QCRFSC")
#'
#' # Using ComBat (does not require QC samples)
#' qcCheckR(user_name = "user1",
#'          project_directory = "path/to/project_directory",
#'          mrm_template_list = list(v1 = list(
#'                                     SIL_guide = "path/to/mrm_guide1.tsv",
#'                                     conc_guide = "path/to/SIL_concentration_guide1.tsv")),
#'          QC_sample_label = "qc",
#'          sample_tags = c("sample", "control", "blank", "qc"),
#'          mv_threshold = 50,
#'          batch_method = "ComBat")
#' }
#'
#' @note When \code{batch_method = "ComBat"} the \pkg{sva} Bioconductor package
#'   is required. Install it with
#'   \code{BiocManager::install("sva")} before use.
#'
#' @details
#' The steps below describe the pipeline in execution order. Input Validation
#' steps are enforced by explicit \code{stop()} calls. All other steps run
#' unconditionally; errors in any step propagate to the caller.
#' \itemize{
#'  \item \strong{Input Validation (enforced):}
#'   \itemize{
#'    \item Validate user_name
#'    \item Validate project_directory
#'    \item Validate mrm_template_list
#'    \item Validate QC_sample_label
#'    \item Validate sample_tags
#'    \item Validate mv_threshold
#'   }
#'  \item \strong{Project Setup:}
#'   \itemize{
#'    \item Initialise project structure
#'    \item Load and organise input data
#'   }
#'  \item \strong{Data Preparation:}
#'   \itemize{
#'    \item Transpose data
#'    \item Sort data
#'    \item Impute missing values
#'    \item Calculate response concentrations
#'    \item Apply batch correction using statTarget
#'   }
#'  \item \strong{Filtering:}
#'   \itemize{
#'    \item Set QC samples
#'    \item Filter samples
#'    \item Filter SIL internal standards
#'    \item Apply lipid-specific filters
#'    \item Filter based on RSD thresholds
#'   }
#'  \item \strong{Reporting and Visualisation:}
#'   \itemize{
#'    \item Generate summary report
#'    \item Create optional plots
#'    \item Perform PCA analysis
#'    \item Generate run order plots
#'    \item Create target control charts
#'   }
#'  \item \strong{Export:}
#'   \itemize{
#'    \item Export all processed data and reports
#'   }
#' }

qcCheckR <- function(user_name,
                     project_directory,
                     mrm_template_list = NULL,
                     QC_sample_label = "LTR",
                     sample_tags = NULL,
                     mv_threshold = 50,
                     batch_method = "QCRFSC",
                     batch_ntree = 500,
                     batch_coCV = 100,
                     batch_Frule = 0,
                     batch_imputeM = "minHalf",
                     combat_par.prior = TRUE,
                     combat_mean.only = FALSE,
                     combat_ref.batch = NULL,
                     batch_column = NULL,
                     write_rda = TRUE,
                     qs_nthreads = max(1L, parallel::detectCores() - 1L),
                     qs_compress_level = 3L,
                     date_order = c("auto", "dmy", "mdy", "ymd", "iso"),
                     advanced_plots = FALSE) {
  # validate advanced_plots early -- typo'd values would otherwise be
  # silently coerced by isTRUE() at the bottom of the pipeline.
  if (!is.logical(advanced_plots) || length(advanced_plots) != 1L ||
      is.na(advanced_plots)) {
    stop("qcCheckR: 'advanced_plots' must be TRUE or FALSE. Got: ",
         deparse(advanced_plots), call. = FALSE)
  }
  # validate write_rda early so a bad value fails fast rather than at
  # the export step at the end of a long pipeline.
  if (!is.logical(write_rda) || length(write_rda) != 1 || is.na(write_rda)) {
    stop("qcCheckR: 'write_rda' must be TRUE or FALSE. Got: ",
         deparse(write_rda), call. = FALSE)
  }
  # qs2::qs_save accepts integer nthreads >= 1 and compress_level in
  # the zstd-supported range [-22, 22]. Catch bad values up-front so the
  # user finds out before the long pipeline runs, not after.
  if (!is.numeric(qs_nthreads) || length(qs_nthreads) != 1L ||
      is.na(qs_nthreads) || qs_nthreads < 1) {
    stop("qcCheckR: 'qs_nthreads' must be a single integer >= 1. Got: ",
         deparse(qs_nthreads), call. = FALSE)
  }
  qs_nthreads <- as.integer(qs_nthreads)
  if (!is.numeric(qs_compress_level) || length(qs_compress_level) != 1L ||
      is.na(qs_compress_level) ||
      qs_compress_level < -22 || qs_compress_level > 22) {
    stop("qcCheckR: 'qs_compress_level' must be a single integer in ",
         "[-22, 22] (zstd range used by qs2). Got: ",
         deparse(qs_compress_level), call. = FALSE)
  }
  qs_compress_level <- as.integer(qs_compress_level)
  if (missing(date_order)) {
    date_order <- "auto"
  } else {
    date_order <- tryCatch(match.arg(date_order),
                           error = function(e) {
      stop("qcCheckR: 'date_order' must be one of 'auto', 'dmy', 'mdy', ",
           "'ymd', or 'iso'. Got: ", deparse(date_order), call. = FALSE)
    })
  }
  #validate user_name
  if (missing(user_name)) {
    stop("qcCheckR: 'user_name' parameter is required. Please see documentation for details.",
         call. = FALSE)
  }
  if (!is.character(user_name) || length(user_name) != 1 || nchar(user_name) == 0) {
    stop("qcCheckR: 'user_name' must be a non-empty single character string. Got: ",
         paste(class(user_name), collapse = ", "), call. = FALSE)
  }
  message("Welcome ", user_name, "!")

  # validate project_directory
  project_directory <- validate_project_directory(project_directory)

  # validate templates
  if (user_name != "ANPC" && missing(mrm_template_list)) {
    stop("qcCheckR: 'mrm_template_list' parameter is required for non-ANPC users.",
         call. = FALSE)
  }
  if (!is.null(mrm_template_list) && !is.list(mrm_template_list)) {
    stop("qcCheckR: 'mrm_template_list' must be a named list of lists (each with 'SIL_guide' and 'conc_guide'). Got: ",
         paste(class(mrm_template_list), collapse = ", "),
         call. = FALSE)
  }
  if (!is.null(mrm_template_list) && is.list(mrm_template_list) && length(mrm_template_list) == 0) {
    stop("qcCheckR: 'mrm_template_list' must not be an empty list.",
         call. = FALSE)
  }

  # validate QC_sample_label
  if (user_name != "ANPC" && missing(QC_sample_label)) {
    stop("qcCheckR: 'QC_sample_label' parameter is required for non-ANPC users.",
         call. = FALSE)
  }
  if (!is.null(QC_sample_label)) {
    if (!is.character(QC_sample_label) || length(QC_sample_label) != 1 || !nzchar(QC_sample_label)) {
      stop("qcCheckR: 'QC_sample_label' must be a non-empty single character string. Got: ",
           paste(class(QC_sample_label), collapse = ", "), " of length ", length(QC_sample_label),
           call. = FALSE)
    }
  }

  # validate sample_tags
  if (user_name != "ANPC" && (missing(sample_tags) || is.null(sample_tags))) {
    stop("qcCheckR: 'sample_tags' parameter is required for non-ANPC users.",
         call. = FALSE)
  }
  if (!is.null(sample_tags)) {
    if (!is.character(sample_tags) || length(sample_tags) == 0) {
      stop("qcCheckR: 'sample_tags' must be a character vector of length >= 1. Got: ",
           paste(class(sample_tags), collapse = ", "), " of length ", length(sample_tags),
           call. = FALSE)
    }
    if (any(!nzchar(sample_tags))) {
      stop("qcCheckR: 'sample_tags' must not contain empty strings.",
           call. = FALSE)
    }
  }

  # validate mv_threshold
  if (!is.numeric(mv_threshold) || length(mv_threshold) != 1 ||
      is.na(mv_threshold) || mv_threshold < 0 || mv_threshold > 100) {
    stop("qcCheckR: 'mv_threshold' must be a single numeric value between 0 and 100. Got: ",
         deparse(mv_threshold), call. = FALSE)
  }

  # validate batch_method
  if (!is.character(batch_method) || length(batch_method) != 1 ||
      !batch_method %in% c("QCRFSC", "ComBat")) {
    stop("qcCheckR: 'batch_method' must be one of 'QCRFSC' or 'ComBat'. Got: ",
         deparse(batch_method), call. = FALSE)
  }

  # validate batch_ntree
  if (!is.numeric(batch_ntree) || length(batch_ntree) != 1 ||
      is.na(batch_ntree) || batch_ntree < 1 || batch_ntree != as.integer(batch_ntree)) {
    stop("qcCheckR: 'batch_ntree' must be a positive integer. Got: ",
         deparse(batch_ntree), call. = FALSE)
  }

  # validate batch_coCV
  if (!is.numeric(batch_coCV) || length(batch_coCV) != 1 ||
      is.na(batch_coCV) || batch_coCV < 0) {
    stop("qcCheckR: 'batch_coCV' must be a non-negative numeric value. Got: ",
         deparse(batch_coCV), call. = FALSE)
  }

  # validate batch_Frule
  if (!is.numeric(batch_Frule) || length(batch_Frule) != 1 ||
      is.na(batch_Frule) || batch_Frule < 0 || batch_Frule > 1) {
    stop("qcCheckR: 'batch_Frule' must be a numeric value between 0 and 1. Got: ",
         deparse(batch_Frule), call. = FALSE)
  }

  # validate batch_imputeM
  if (!is.character(batch_imputeM) || length(batch_imputeM) != 1 ||
      !batch_imputeM %in% c("minHalf", "median", "mean", "knn")) {
    stop("qcCheckR: 'batch_imputeM' must be one of 'minHalf', 'median', 'mean', or 'knn'. Got: ",
         deparse(batch_imputeM), call. = FALSE)
  }

  # validate ComBat-specific parameters (only when ComBat is selected)
  if (batch_method == "ComBat") {
    if (!is.logical(combat_par.prior) || length(combat_par.prior) != 1 || is.na(combat_par.prior)) {
      stop("qcCheckR: 'combat_par.prior' must be TRUE or FALSE. Got: ",
           deparse(combat_par.prior), call. = FALSE)
    }
    if (!is.logical(combat_mean.only) || length(combat_mean.only) != 1 || is.na(combat_mean.only)) {
      stop("qcCheckR: 'combat_mean.only' must be TRUE or FALSE. Got: ",
           deparse(combat_mean.only), call. = FALSE)
    }
    if (!is.null(combat_ref.batch) && (!is.character(combat_ref.batch) || length(combat_ref.batch) != 1 || !nzchar(combat_ref.batch))) {
      stop("qcCheckR: 'combat_ref.batch' must be a non-empty character string or NULL. Got: ",
           deparse(combat_ref.batch), call. = FALSE)
    }
    if (!is.null(batch_column) && (!is.character(batch_column) || length(batch_column) != 1 || !nzchar(batch_column))) {
      stop("qcCheckR: 'batch_column' must be a non-empty character string or NULL. Got: ",
           deparse(batch_column), call. = FALSE)
    }
  }

  # process data----
  ##project setup----
  master_list <- qcCheckR_setup_project(
    user_name = user_name,
    project_directory = project_directory,
    mrm_template_list = mrm_template_list,
    QC_sample_label = QC_sample_label,
    sample_tags = sample_tags,
    mv_threshold = mv_threshold
  )
  # store batch correction parameters----
  master_list$project_details$batch_method <- batch_method
  master_list$project_details$batch_ntree <- as.integer(batch_ntree)
  master_list$project_details$batch_coCV <- batch_coCV
  master_list$project_details$batch_Frule <- batch_Frule
  master_list$project_details$batch_imputeM <- batch_imputeM
  master_list$project_details$combat_par.prior <- combat_par.prior
  master_list$project_details$combat_mean.only <- combat_mean.only
  master_list$project_details$combat_ref.batch <- combat_ref.batch
  master_list$project_details$batch_column <- batch_column

  ##data preparation----
  master_list <- qcCheckR_transpose_data(master_list)
  master_list <- qcCheckR_sort_data(master_list, date_order = date_order)
  master_list <- qcCheckR_impute_data(master_list)
  master_list <- qcCheckR_calculate_response_concentration(master_list)
  master_list <- qcCheckR_statTarget_batch_correction(master_list)
  #filtering----
  master_list <- qcCheckR_set_qc(master_list)
  master_list <- qcCheckR_sample_filter(master_list)
  master_list <- qcCheckR_sil_IntStd_filter(master_list)
  master_list <- qcCheckR_lipid_filter(master_list)
  master_list <- qcCheckR_RSD_filter(master_list)
  #summary report----
  master_list <- qcCheckR_summary_report(master_list)
  #plot generation----
  master_list <- qcCheckR_plot_options(master_list)
  master_list <- qcCheckR_PCA(master_list)
  master_list <- qcCheckR_run_order_plots(master_list)
  master_list <- qcCheckR_target_control_charts(master_list)
  #exports----
  master_list <- qcCheckR_export_all(master_list,
                                     write_rda = write_rda,
                                     qs_nthreads = qs_nthreads,
                                     qs_compress_level = qs_compress_level)
  #advanced plots (R-side parity with GUI)----
  if (isTRUE(advanced_plots)) {
    message("Writing advanced plots to all/figures/qcCheckR/ ...")
    tryCatch({
      plots <- qcCheckR_collect_plots(master_list)
      save_figure_list(
        plots,
        project_dir = master_list$project_details$project_dir,
        module = "qcCheckR"
      )
    }, error = function(e) {
      warning("qcCheckR: advanced_plots write failed: ",
              conditionMessage(e), call. = FALSE)
    })
  }
  invisible(master_list)
}#close of function
