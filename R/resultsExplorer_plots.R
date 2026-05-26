# Results Explorer Plot Constructors ----
# Pure ggplot2 + plotly constructors that mirror the Shiny "Results
# Explorer" tab so R users get every figure the GUI shows -- callable
# directly and writable to disk via [resultsExplorerR()] when
# advanced_plots = TRUE.

# Local palette: matches the Shiny app's results_palette so saved
# figures look identical to the GUI.
.re_palette <- c("#0d9488", "#1e40af", "#7c3aed", "#db2777",
                 "#ea580c", "#059669", "#0284c7", "#6366f1")

# Threshold-based RSD status (pass / warning / fail).
re_status_from_rsd <- function(rsd, warn_thr = 20, fail_thr = 30) {
  out <- rep(NA_character_, length(rsd))
  not_na <- !is.na(rsd)
  fail_idx <- not_na & rsd > fail_thr
  warn_idx <- not_na & !fail_idx & rsd > warn_thr
  pass_idx <- not_na & rsd <= warn_thr
  out[fail_idx] <- "fail"
  out[warn_idx] <- "warning"
  out[pass_idx] <- "pass"
  names(out) <- names(rsd)
  out
}

# Tokenise "PC 36:2" -> "PC" using the same regex the Shiny batch tab uses.
re_extract_class <- function(met_names) {
  bc_extract_class(met_names)
}

#' RSD distribution histogram (advanced_plots)
#'
#' Histogram of per-metabolite %RSD with optional warn/fail reference
#' lines. Mirrors the Shiny `results_rsd_histogram`.
#'
#' @keywords internal
#' @param rsd_values Named numeric vector of metabolite -> RSD%.
#' @param warn_thr,fail_thr Numeric thresholds (default 20, 30) drawn as
#'   amber/red reference lines.
#' @return `list(static, interactive)`, or `NULL`.
re_plot_rsd_histogram <- function(rsd_values, warn_thr = 20, fail_thr = 30) {
  rsds <- rsd_values[!is.na(rsd_values)]
  if (!length(rsds)) return(NULL)
  rsd_df <- data.frame(rsd = as.numeric(rsds), stringsAsFactors = FALSE)

  static <- ggplot2::ggplot(rsd_df, ggplot2::aes(x = .data[["rsd"]])) +
    ggplot2::geom_histogram(bins = 30, fill = "#0d9488", colour = "white",
                            linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = warn_thr, colour = "#d97706",
                        linetype = "dotted", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = fail_thr, colour = "#e11d48",
                        linetype = "dashed", linewidth = 0.7) +
    ggplot2::labs(x = "RSD %", y = "Count",
                  title = "RSD Distribution (All Metabolites)") +
    ggplot2::theme_bw()

  bins <- seq(0, max(rsd_df$rsd, na.rm = TRUE) + 2, length.out = 31)
  bin_mids <- (utils::head(bins, -1) + utils::tail(bins, -1)) / 2
  bin_colors <- ifelse(bin_mids > fail_thr, "#e11d48",
                ifelse(bin_mids > warn_thr, "#d97706", "#059669"))
  interactive <- plotly::plot_ly(
    data = rsd_df, x = ~rsd, type = "histogram",
    marker = list(color = bin_colors,
                  line = list(color = "white", width = 0.5)),
    xbins = list(start = bins[1], end = utils::tail(bins, 1),
                 size = bins[2] - bins[1]),
    hovertemplate = "RSD: %{x:.1f}%<br>Count: %{y}<extra></extra>"
  ) |>
    plotly::layout(
      title = "RSD Distribution (All Metabolites)",
      xaxis = list(title = "RSD %"),
      yaxis = list(title = "Count"),
      shapes = list(
        list(type = "line", x0 = warn_thr, x1 = warn_thr,
             y0 = 0, y1 = 1, yref = "paper",
             line = list(color = "#d97706", dash = "dot", width = 2)),
        list(type = "line", x0 = fail_thr, x1 = fail_thr,
             y0 = 0, y1 = 1, yref = "paper",
             line = list(color = "#e11d48", dash = "dash", width = 2))
      )
    )

  list(static = static, interactive = interactive)
}

#' Pass/fail donut (advanced_plots)
#'
#' Donut chart of pass / warning / fail counts across metabolites.
#' Mirrors the Shiny `results_passfail_donut`.
#'
#' @keywords internal
#' @param status_vector Named character vector with values in
#'   `c("pass", "warning", "fail")` (typically from [re_status_from_rsd()]).
#' @return `list(static, interactive)`, or `NULL`.
re_plot_passfail_donut <- function(status_vector) {
  status <- status_vector[!is.na(status_vector)]
  if (!length(status)) return(NULL)
  counts <- c(Pass    = sum(status == "pass"),
              Warning = sum(status == "warning"),
              Fail    = sum(status == "fail"))
  counts <- counts[counts > 0]
  if (!length(counts)) return(NULL)
  colors <- c(Pass = "#059669", Warning = "#d97706", Fail = "#e11d48")

  pie_df <- data.frame(category = names(counts),
                       count = as.integer(counts),
                       stringsAsFactors = FALSE)

  static <- ggplot2::ggplot(
    pie_df,
    ggplot2::aes(x = 2, y = .data[["count"]], fill = .data[["category"]])
  ) +
    ggplot2::geom_col(width = 1, colour = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(0.5, 2.5) +
    ggplot2::scale_fill_manual(values = colors[names(counts)]) +
    ggplot2::labs(title = "Metabolite QC Status", fill = NULL,
                  x = NULL, y = NULL) +
    ggplot2::annotate("text", x = 0.5, y = 0,
                       label = format(sum(counts), big.mark = ","),
                       size = 6, colour = "#334155") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  interactive <- plotly::plot_ly(
    labels = names(counts), values = counts,
    type = "pie", hole = 0.55,
    marker = list(colors = colors[names(counts)],
                  line = list(color = "white", width = 2)),
    textinfo = "label+value",
    hovertemplate =
      "<b>%{label}</b><br>%{value} metabolites (%{percent})<extra></extra>"
  ) |>
    plotly::layout(
      title = "Metabolite QC Status",
      showlegend = FALSE,
      annotations = list(list(text = paste0(sum(counts)), x = 0.5, y = 0.5,
                               font = list(size = 22, color = "#334155"),
                               showarrow = FALSE))
    )

  list(static = static, interactive = interactive)
}

#' Quality-by-class stacked bar (advanced_plots)
#'
#' Stacked horizontal bar chart: per class, count of pass / warning /
#' fail metabolites. Mirrors the Shiny `results_class_summary`.
#'
#' @keywords internal
#' @param status_vector Named character (see [re_plot_passfail_donut()]).
#' @param class_map Named character; metabolite -> class.
#' @param warn_thr,fail_thr Numeric thresholds, used only for legend
#'   labels.
#' @return `list(static, interactive)`, or `NULL`.
re_plot_class_summary <- function(status_vector, class_map,
                                  warn_thr = 20, fail_thr = 30) {
  status <- status_vector[!is.na(status_vector)]
  if (!length(status)) return(NULL)
  mets <- names(status)
  classes <- if (!is.null(class_map) && length(class_map)) {
    ifelse(mets %in% names(class_map), class_map[mets], "Unknown")
  } else {
    re_extract_class(mets)
  }
  summ_df <- data.frame(metabolite = mets, class = classes,
                        status = unname(status), stringsAsFactors = FALSE)
  agg <- stats::aggregate(metabolite ~ class + status, data = summ_df,
                          FUN = length)
  names(agg)[3] <- "count"
  class_totals <- stats::aggregate(count ~ class, data = agg, FUN = sum)
  class_totals <- class_totals[order(class_totals$count, decreasing = TRUE), ]
  agg$class <- factor(agg$class, levels = rev(class_totals$class))
  agg$status <- factor(agg$status, levels = c("pass", "warning", "fail"))

  status_colors <- c(pass = "#059669", warning = "#d97706", fail = "#e11d48")
  status_labels <- c(
    pass    = paste0("Pass (<", warn_thr, "%)"),
    warning = paste0("Warning (", warn_thr, "-", fail_thr, "%)"),
    fail    = paste0("Fail (>", fail_thr, "%)")
  )

  static <- ggplot2::ggplot(
    agg,
    ggplot2::aes(x = .data[["count"]], y = .data[["class"]],
                 fill = .data[["status"]])
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = status_colors, labels = status_labels,
                                name = NULL) +
    ggplot2::labs(x = "Number of Metabolites", y = NULL,
                  title = "Quality by Class") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                    axis.text.y = ggplot2::element_text(size = 9))

  interactive <- plotly::ggplotly(static)

  list(static = static, interactive = interactive)
}

#' RSD scatter: before vs after batch correction (advanced_plots)
#'
#' One point per metabolite, x = pre-correction RSD, y = post-correction
#' RSD, coloured by class. y = x diagonal drawn for reference. Mirrors
#' the Shiny `results_rsd_scatter`.
#'
#' @keywords internal
#' @param qc_rsd_before,qc_rsd_after Named numeric vectors.
#' @param class_map Optional named character; metabolite -> class.
#' @return `list(static, interactive)`, or `NULL`.
re_plot_rsd_scatter <- function(qc_rsd_before, qc_rsd_after,
                                class_map = NULL) {
  if (is.null(qc_rsd_before) || is.null(qc_rsd_after) ||
      !length(qc_rsd_before) || !length(qc_rsd_after)) return(NULL)
  common <- intersect(names(qc_rsd_before), names(qc_rsd_after))
  if (!length(common)) return(NULL)

  classes <- if (!is.null(class_map) && length(class_map)) {
    ifelse(common %in% names(class_map), class_map[common], "Unknown")
  } else {
    re_extract_class(common)
  }
  scat_df <- data.frame(
    metabolite = common,
    before = as.numeric(qc_rsd_before[common]),
    after  = as.numeric(qc_rsd_after[common]),
    class  = classes,
    stringsAsFactors = FALSE
  )
  scat_df <- scat_df[!is.na(scat_df$before) & !is.na(scat_df$after), ,
                     drop = FALSE]
  if (!nrow(scat_df)) return(NULL)
  max_val <- max(c(scat_df$before, scat_df$after), na.rm = TRUE) * 1.1

  static <- ggplot2::ggplot(
    scat_df,
    ggplot2::aes(x = .data[["before"]], y = .data[["after"]],
                 colour = .data[["class"]], text = .data[["metabolite"]])
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", colour = "grey60") +
    ggplot2::geom_point(size = 2.5, alpha = 0.7) +
    ggplot2::coord_equal() +
    ggplot2::xlim(0, max_val) + ggplot2::ylim(0, max_val) +
    ggplot2::labs(x = "RSD Before (%)", y = "RSD After (%)",
                  title = "Batch Correction Effect (Before vs After RSD)",
                  colour = "Class") +
    ggplot2::theme_bw()

  interactive <- plotly::ggplotly(static, tooltip = c("text", "x", "y"))
  list(static = static, interactive = interactive)
}

#' Mean concentration vs RSD scatter (advanced_plots)
#'
#' Per-metabolite scatter: x = mean concentration (log scale), y = RSD,
#' coloured by class, with warn/fail horizontal reference lines.
#' Mirrors the Shiny `results_conc_vs_rsd`.
#'
#' @keywords internal
#' @param data Data frame of samples x metabolites.
#' @param rsd_values Named numeric vector of metabolite -> RSD%.
#' @param class_map Optional named character; metabolite -> class.
#' @param warn_thr,fail_thr Numeric thresholds for reference lines.
#' @return `list(static, interactive)`, or `NULL`.
re_plot_conc_vs_rsd <- function(data, rsd_values, class_map = NULL,
                                warn_thr = 20, fail_thr = 30) {
  if (is.null(data) || is.null(rsd_values) ||
      !length(rsd_values)) return(NULL)
  mets <- intersect(names(rsd_values), names(data))
  if (!length(mets)) return(NULL)

  mean_conc <- vapply(mets, function(m) mean(data[[m]], na.rm = TRUE),
                      numeric(1))
  rsd_vals <- as.numeric(rsd_values[mets])
  classes <- if (!is.null(class_map) && length(class_map)) {
    ifelse(mets %in% names(class_map), class_map[mets], "Unknown")
  } else {
    re_extract_class(mets)
  }
  scat_df <- data.frame(metabolite = mets, conc = mean_conc,
                        rsd = rsd_vals, class = classes,
                        stringsAsFactors = FALSE)
  scat_df <- scat_df[!is.na(scat_df$conc) & !is.na(scat_df$rsd) &
                       scat_df$conc > 0, , drop = FALSE]
  if (!nrow(scat_df)) return(NULL)

  static <- ggplot2::ggplot(
    scat_df,
    ggplot2::aes(x = .data[["conc"]], y = .data[["rsd"]],
                 colour = .data[["class"]], text = .data[["metabolite"]])
  ) +
    ggplot2::geom_point(size = 2.5, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = warn_thr, colour = "#d97706",
                        linetype = "dotted", linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = fail_thr, colour = "#e11d48",
                        linetype = "dashed", linewidth = 0.7) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Mean Concentration (log scale)", y = "RSD (%)",
                  title = "Mean Concentration vs RSD", colour = "Class") +
    ggplot2::theme_bw()

  interactive <- plotly::ggplotly(static, tooltip = c("text", "x", "y"))
  list(static = static, interactive = interactive)
}

#' Box plot by sample type (advanced_plots)
#'
#' Per-sample-type box plot of a single metabolite. Mirrors the Shiny
#' `results_boxplot` (deep-dive panel). When `metabolite` is NULL the
#' metabolite with the highest RSD in `rsd_values` is used (matches the
#' Shiny default selector).
#'
#' @keywords internal
#' @param data Data frame of samples x metabolites + sample-type column.
#' @param metabolite Character or NULL.
#' @param rsd_values Optional named numeric for default selection.
#' @return `list(static, interactive)`, or `NULL`.
re_plot_boxplot_by_class <- function(data, metabolite = NULL,
                                     rsd_values = NULL) {
  if (is.null(data) || !nrow(data)) return(NULL)
  if (is.null(metabolite)) metabolite <- re_default_metabolite(data, rsd_values)
  if (is.null(metabolite) || !metabolite %in% names(data)) return(NULL)

  type_col <- if ("sample_type_factor" %in% names(data)) "sample_type_factor"
              else if ("sample_type" %in% names(data)) "sample_type"
              else NULL
  if (is.null(type_col)) {
    static <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = factor(1), y = .data[[metabolite]])
    ) +
      ggplot2::geom_boxplot(fill = .re_palette[1], alpha = 0.4) +
      ggplot2::geom_jitter(width = 0.15, alpha = 0.4, size = 1.5,
                            colour = .re_palette[1]) +
      ggplot2::labs(title = paste0("Distribution: ", metabolite),
                    x = NULL, y = metabolite) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank())
    interactive <- plotly::ggplotly(static)
    return(list(static = static, interactive = interactive))
  }

  df <- data
  df$.sample_type_label <- as.character(df[[type_col]])
  type_levels <- sort(unique(df$.sample_type_label))
  pal <- .re_palette[seq_len(min(length(type_levels), length(.re_palette)))]
  type_palette <- stats::setNames(rep(pal, length.out = length(type_levels)),
                                  type_levels)

  static <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[".sample_type_label"]],
                 y = .data[[metabolite]],
                 fill = .data[[".sample_type_label"]])
  ) +
    ggplot2::geom_boxplot(alpha = 0.4, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.4, size = 1.5,
                          ggplot2::aes(colour = .data[[".sample_type_label"]])) +
    ggplot2::scale_fill_manual(values = type_palette, guide = "none") +
    ggplot2::scale_colour_manual(values = type_palette, guide = "none") +
    ggplot2::labs(title = paste0("Distribution: ", metabolite),
                  x = "Sample Type", y = metabolite) +
    ggplot2::theme_bw()

  interactive <- plotly::ggplotly(static)
  list(static = static, interactive = interactive)
}

#' Run-order scatter for a single metabolite (advanced_plots)
#'
#' Sample-type-coloured scatter of a metabolite vs run order (or
#' injection order). Mirrors the Shiny `results_runorder` /
#' `results_deep_before` / `results_deep_after` (depending on which
#' data frame is passed).
#'
#' @keywords internal
#' @param data Data frame.
#' @param metabolite Character or NULL (defaults to highest-RSD metabolite).
#' @param title_prefix Character (e.g. "Run Order" or "Before Correction").
#' @param rsd_values Optional, used to pick the default metabolite.
#' @return `list(static, interactive)`, or `NULL`.
re_plot_runorder <- function(data, metabolite = NULL,
                             title_prefix = "Run Order",
                             rsd_values = NULL) {
  if (is.null(data) || !nrow(data)) return(NULL)
  if (is.null(metabolite)) metabolite <- re_default_metabolite(data, rsd_values)
  if (is.null(metabolite) || !metabolite %in% names(data)) return(NULL)

  x_col <- if ("sample_run_index" %in% names(data)) "sample_run_index"
           else if ("run_order" %in% names(data)) "run_order"
           else if ("injection_order" %in% names(data)) "injection_order"
           else NULL
  if (is.null(x_col)) {
    data$.sample_index <- seq_len(nrow(data))
    x_col <- ".sample_index"
    x_lab <- "Sample Index"
  } else {
    x_lab <- if (x_col == "injection_order") "Injection Order" else "Run Order"
  }

  type_col <- if ("sample_type_factor" %in% names(data)) "sample_type_factor"
              else if ("sample_type" %in% names(data)) "sample_type"
              else NULL

  if (is.null(type_col)) {
    static <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = .data[[x_col]], y = .data[[metabolite]])
    ) +
      ggplot2::geom_point(size = 1.5, alpha = 0.6, colour = .re_palette[1]) +
      ggplot2::labs(title = paste0(title_prefix, ": ", metabolite),
                    x = x_lab, y = metabolite) +
      ggplot2::theme_bw()
    return(list(static = static, interactive = plotly::ggplotly(static)))
  }

  df <- data
  df$.sample_type_label <- as.character(df[[type_col]])
  type_levels <- sort(unique(df$.sample_type_label))
  pal <- .re_palette[seq_len(min(length(type_levels), length(.re_palette)))]
  type_palette <- stats::setNames(rep(pal, length.out = length(type_levels)),
                                  type_levels)

  static <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[x_col]], y = .data[[metabolite]],
                 colour = .data[[".sample_type_label"]])
  ) +
    ggplot2::geom_point(size = 1.8, alpha = 0.7) +
    ggplot2::scale_colour_manual(values = type_palette, name = "Sample Type") +
    ggplot2::labs(title = paste0(title_prefix, ": ", metabolite),
                  x = x_lab, y = metabolite) +
    ggplot2::theme_bw()

  interactive <- plotly::ggplotly(static)
  list(static = static, interactive = interactive)
}

#' Concentration correlation heatmap (advanced_plots)
#'
#' Z-scored sample x metabolite heatmap, capped at 200 samples and 100
#' metabolites to keep file size reasonable (matches Shiny limits).
#' Mirrors the Shiny `results_heatmap`.
#'
#' @keywords internal
#' @param data Data frame.
#' @param metabolites Optional character vector. Defaults to all numeric
#'   columns (minus metadata).
#' @param max_samples,max_metabolites Integer caps.
#' @return `list(static, interactive)`, or `NULL`.
re_plot_heatmap_correlations <- function(data, metabolites = NULL,
                                         max_samples = 200,
                                         max_metabolites = 100) {
  if (is.null(data) || !nrow(data)) return(NULL)
  if (is.null(metabolites)) {
    num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    metabolites <- setdiff(num_cols, .qc_meta_cols)
  }
  metabolites <- intersect(metabolites, names(data))
  if (length(metabolites) < 2) return(NULL)

  df <- data
  # Rank metabolites by variance so the top-N selection is meaningful.
  if (length(metabolites) > max_metabolites) {
    all_vars <- vapply(metabolites,
                       function(m) stats::var(df[[m]], na.rm = TRUE),
                       numeric(1))
    metabolites <- names(sort(all_vars,
                              decreasing = TRUE))[seq_len(max_metabolites)]
  }
  if (nrow(df) > max_samples) df <- df[seq_len(max_samples), , drop = FALSE]

  mat <- scale(as.matrix(df[, metabolites, drop = FALSE]))
  mat[!is.finite(mat)] <- 0
  row_labels <- if ("sample_name" %in% names(df)) df$sample_name
                else seq_len(nrow(df))

  long_df <- expand.grid(sample = row_labels, metabolite = colnames(mat),
                         stringsAsFactors = FALSE)
  long_df$z <- as.vector(mat)
  long_df$sample <- factor(long_df$sample, levels = row_labels)
  long_df$metabolite <- factor(long_df$metabolite, levels = colnames(mat))

  static <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = .data[["metabolite"]], y = .data[["sample"]],
                 fill = .data[["z"]])
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#1b3a5c", mid = "#2dd4bf",
                                    high = "#fef3c7", midpoint = 0,
                                    name = "Z-score") +
    ggplot2::labs(title = paste0("Concentration Heatmap (",
                                  length(metabolites), " Metabolites, ",
                                  nrow(df), " Samples)"),
                  x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = -45, hjust = 0, size = 7),
      axis.text.y = ggplot2::element_text(size = 6)
    )

  interactive <- plotly::plot_ly(
    x = colnames(mat), y = row_labels, z = mat, type = "heatmap",
    colorscale = list(
      list(0,   "#0d1b2a"), list(0.2, "#1b3a5c"),
      list(0.4, "#0d9488"), list(0.6, "#2dd4bf"),
      list(0.8, "#fbbf24"), list(1,   "#fef3c7")
    ),
    colorbar = list(title = list(text = "Z-score"), thickness = 12,
                    len = 0.6),
    hovertemplate =
      "<b>%{x}</b><br>Sample: %{y}<br>Z-score: %{z:.2f}<extra></extra>"
  ) |>
    plotly::layout(
      title = paste0("Concentration Heatmap (", length(metabolites),
                     " Metabolites)"),
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = ""),
      margin = list(t = 50, r = 30, b = 130, l = 140)
    )

  list(static = static, interactive = interactive)
}

# Internal: pick the metabolite with the highest RSD when the caller
# doesn't specify one (mirrors the Shiny GUI selector defaults).
re_default_metabolite <- function(data, rsd_values = NULL) {
  if (!is.null(rsd_values) && length(rsd_values)) {
    rsds <- rsd_values[!is.na(rsd_values)]
    if (length(rsds)) {
      cand <- names(rsds)[which.max(rsds)]
      if (cand %in% names(data)) return(cand)
    }
  }
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, .qc_meta_cols)
  if (!length(num_cols)) return(NULL)
  num_cols[1]
}
