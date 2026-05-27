# Results Explorer R API ----
# Exposes the Shiny "Results Explorer" tab as a callable function so R
# users get the same figures (and can opt into saving them as
# PDF + HTML under <project_dir>/all/figures/results_explorer/).

#' Generate Results Explorer plots and summary from R
#'
#' Recreates every plot the Shiny app's Results Explorer tab renders --
#' RSD distribution, pass/fail donut, quality-by-class, RSD scatter,
#' concentration-vs-RSD scatter, per-metabolite box plot, run-order
#' scatter (before/after when batch correction is supplied), and
#' concentration heatmap -- so they can be inspected from an R script
#' and (optionally) written to disk.
#'
#' Accepts a plain data frame, a `qcCheckR()` master_list, or a
#' `batchCorrectR()` result; dispatch picks the relevant tables. When
#' `advanced_plots = TRUE` and `project_dir` is supplied, every plot is
#' written to `<project_dir>/all/figures/results_explorer/` as both a
#' static `.pdf` (via `ggplot2::ggsave`) and an interactive `.html`
#' (via `htmlwidgets::saveWidget`).
#'
#' @param data One of:
#'   \itemize{
#'     \item A data frame of samples (rows) x metabolites (columns) plus
#'       metadata columns (e.g. `sample_type`, `sample_run_index`).
#'     \item A `qcCheckR()` master_list (auto-extracts
#'       `data$concentration$corrected` and `filters$rsd`).
#'     \item A `batchCorrectR()` result (auto-extracts `corrected_data`,
#'       `qc_rsd_before`, `qc_rsd_after`).
#'   }
#' @param project_dir Character or NULL. When `advanced_plots = TRUE`,
#'   plots are written to `file.path(project_dir, "all", "figures",
#'   "results_explorer")`. Required for disk writes; otherwise ignored.
#' @param advanced_plots Logical. When `TRUE` and `project_dir` is
#'   non-null, every plot is saved as PDF + HTML. Plots are returned
#'   in-memory either way. Default `FALSE`.
#' @param warn_thr,fail_thr Numeric thresholds for the RSD-based
#'   pass/warning/fail classification. Defaults `20` and `30` (mirrors
#'   the Shiny app defaults).
#' @param qc_label Character. Sample-type value identifying QC
#'   injections, used to compute RSDs from a bare data frame. Default
#'   `"qc"` (case-insensitive).
#' @param class_map Optional named character vector mapping metabolite
#'   name -> class. If `NULL`, classes are derived from metabolite
#'   names via the same regex the GUI uses.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`plots`}{Named list of plots; each entry is
#'       `list(static = <ggplot>, interactive = <plotly>)`.}
#'     \item{`summary`}{Tibble with one row per metabolite: `metabolite`,
#'       `class`, `rsd`, `status`.}
#'   }
#' @export
#' @examples
#' \dontrun{
#'   # From a batchCorrectR result, save figures to disk:
#'   bc <- batchCorrectR(my_data, project_dir = my_project)
#'   re <- resultsExplorerR(bc, project_dir = my_project,
#'                           advanced_plots = TRUE)
#'
#'   # From a plain data frame, return plots in memory only:
#'   re <- resultsExplorerR(my_data, qc_label = "PQC")
#'   re$plots$rsd_histogram$static  # ggplot
#'   re$plots$rsd_histogram$interactive  # plotly
#' }
resultsExplorerR <- function(data,
                             project_dir = NULL,
                             advanced_plots = FALSE,
                             warn_thr = 20,
                             fail_thr = 30,
                             qc_label = "qc",
                             class_map = NULL) {
  # --- input validation ---
  if (!is.logical(advanced_plots) || length(advanced_plots) != 1L ||
      is.na(advanced_plots)) {
    stop("resultsExplorerR: 'advanced_plots' must be TRUE or FALSE.",
         call. = FALSE)
  }
  if (isTRUE(advanced_plots) && is.null(project_dir)) {
    message("resultsExplorerR: advanced_plots = TRUE but project_dir ",
            "is NULL -- plots will be returned in memory only, no files ",
            "will be written.")
  }
  if (!is.numeric(warn_thr) || length(warn_thr) != 1L || is.na(warn_thr) ||
      warn_thr < 0) {
    stop("resultsExplorerR: 'warn_thr' must be a single non-negative number.",
         call. = FALSE)
  }
  if (!is.numeric(fail_thr) || length(fail_thr) != 1L || is.na(fail_thr) ||
      fail_thr < warn_thr) {
    stop("resultsExplorerR: 'fail_thr' must be a single number >= warn_thr.",
         call. = FALSE)
  }

  # --- input dispatch ---
  parts <- re_dispatch_input(data, qc_label = qc_label)
  df             <- parts$data
  qc_rsd_before  <- parts$qc_rsd_before
  qc_rsd_after   <- parts$qc_rsd_after
  rsd_values     <- parts$rsd_values
  before_data    <- parts$before_data

  if (is.null(df) || !nrow(df)) {
    stop("resultsExplorerR: no usable data frame extracted from 'data'. ",
         "Expected a data.frame, a qcCheckR master_list, or a ",
         "batchCorrectR result.", call. = FALSE)
  }

  # Compute per-metabolite RSDs from the data frame if dispatch didn't
  # supply them (typical for the bare-data-frame path).
  if (is.null(rsd_values)) {
    rsd_values <- re_compute_rsds(df, qc_label = qc_label)
  }

  status <- re_status_from_rsd(rsd_values, warn_thr = warn_thr,
                               fail_thr = fail_thr)

  if (is.null(class_map)) {
    nm <- names(rsd_values) %||% character()
    if (length(nm)) {
      class_map <- stats::setNames(re_extract_class(nm), nm)
    } else {
      class_map <- character()
    }
  }

  # --- build plots ---
  plots <- list(
    rsd_histogram        = re_plot_rsd_histogram(rsd_values,
                                                 warn_thr, fail_thr),
    passfail_distribution = re_plot_passfail_donut(status),
    class_summary        = re_plot_class_summary(status, class_map,
                                                 warn_thr, fail_thr),
    conc_vs_rsd          = re_plot_conc_vs_rsd(df, rsd_values, class_map,
                                               warn_thr, fail_thr),
    boxplot_by_class     = re_plot_boxplot_by_class(df,
                                                    rsd_values = rsd_values),
    runorder             = re_plot_runorder(df, rsd_values = rsd_values,
                                            title_prefix = "Run Order"),
    heatmap_correlations = re_plot_heatmap_correlations(df)
  )
  # Batch-correction-specific plots only when before/after are available.
  if (!is.null(qc_rsd_before) && !is.null(qc_rsd_after)) {
    plots$rsd_scatter <- re_plot_rsd_scatter(qc_rsd_before, qc_rsd_after,
                                              class_map)
  }
  if (!is.null(before_data) && !is.null(qc_rsd_before)) {
    plots$runorder_before <- re_plot_runorder(before_data,
                                              rsd_values = qc_rsd_before,
                                              title_prefix = "Before Correction")
    plots$runorder_after  <- re_plot_runorder(df,
                                              rsd_values = qc_rsd_after,
                                              title_prefix = "After Correction")
  }

  # --- summary tibble ---
  summary_df <- tibble::tibble(
    metabolite = names(rsd_values) %||% character(),
    class = unname(class_map[names(rsd_values)]),
    rsd = as.numeric(rsd_values),
    status = unname(status)
  )

  # --- optional save to disk ---
  if (isTRUE(advanced_plots) && !is.null(project_dir)) {
    message("Writing Results Explorer plots to ",
            "all/figures/results_explorer/ ...")
    tryCatch(
      save_figure_list(plots, project_dir = project_dir,
                       module = "results_explorer"),
      error = function(e) {
        warning("resultsExplorerR: figure write failed: ",
                conditionMessage(e), call. = FALSE)
      }
    )
  }

  invisible(list(plots = plots, summary = summary_df))
}

# `%||%` is defined once in R/zzz.R and shared across the figures
# export + advanced plot code paths.

# Internal: pick the right tables out of whatever the user passed in.
# Returns a list with up to: data (post-correction), before_data
# (pre-correction, for paired run-order plots), qc_rsd_before,
# qc_rsd_after, rsd_values (single source -- used when before/after pair
# is absent).
re_dispatch_input <- function(data, qc_label = "qc") {
  out <- list(data = NULL, before_data = NULL,
              qc_rsd_before = NULL, qc_rsd_after = NULL,
              rsd_values = NULL)

  # batchCorrectR result: has $corrected_data + $qc_rsd_*
  if (is.list(data) && !is.data.frame(data) &&
      !is.null(data$corrected_data)) {
    out$data <- data$corrected_data
    out$qc_rsd_before <- data$qc_rsd_before
    out$qc_rsd_after  <- data$qc_rsd_after
    if (length(out$qc_rsd_after)) {
      out$rsd_values <- out$qc_rsd_after
    }
    return(out)
  }
  # qcCheckR master_list: has $data$concentration$corrected + $filters$rsd
  if (is.list(data) && !is.data.frame(data) &&
      !is.null(data$data) && !is.null(data$data$concentration)) {
    corrected <- data$data$concentration$corrected
    if (!is.null(corrected) &&
        is.list(corrected) && !is.data.frame(corrected)) {
      corrected <- dplyr::bind_rows(corrected)
    }
    if (is.null(corrected) && !is.null(data$data$concentration$imputed)) {
      imp <- data$data$concentration$imputed
      corrected <- if (is.list(imp) && !is.data.frame(imp))
        dplyr::bind_rows(imp) else imp
    }
    out$data <- corrected
    # Reuse the internal RSD extractor we added for the qcCheckR collector
    # so the two surfaces share one truth source.
    out$rsd_values <- tryCatch(
      qc_rsd_vector(data,
                    stage = if (!is.null(data$filters$rsd) &&
                                "concentration[statTarget]" %in%
                                  data$filters$rsd$dataSource)
                      "concentration[statTarget]" else "concentration"),
      error = function(e) NULL
    )
    return(out)
  }
  # Plain data frame
  if (is.data.frame(data)) {
    out$data <- data
    return(out)
  }
  if (is.list(data) && !is.data.frame(data)) {
    # list of data frames -> bind
    out$data <- tryCatch(dplyr::bind_rows(data), error = function(e) NULL)
    return(out)
  }
  out
}

# Internal: compute per-metabolite QC %RSD from a bare data frame using
# `qc_label` to identify QC injections. Falls back to all-sample RSD if
# no QC samples match.
re_compute_rsds <- function(df, qc_label = "qc") {
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, .qc_meta_cols)
  if (!length(num_cols)) return(stats::setNames(numeric(0), character(0)))

  type_col <- if ("sample_type_factor" %in% names(df)) "sample_type_factor"
              else if ("sample_type" %in% names(df)) "sample_type"
              else NULL
  qc_rows <- if (!is.null(type_col)) {
    tolower(as.character(df[[type_col]])) == tolower(qc_label)
  } else {
    rep(TRUE, nrow(df))
  }
  if (sum(qc_rows) < 2) qc_rows <- rep(TRUE, nrow(df))

  qc_df <- df[qc_rows, , drop = FALSE]
  out <- vapply(num_cols, function(m) {
    v <- qc_df[[m]][!is.na(qc_df[[m]])]
    if (length(v) < 2 || mean(v) == 0) return(NA_real_)
    stats::sd(v) / mean(v) * 100
  }, numeric(1))
  names(out) <- num_cols
  out
}
