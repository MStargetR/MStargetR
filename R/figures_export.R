# Figures Export ----
# Shared helpers used by qcCheckR(), batchCorrectR(), and resultsExplorerR()
# to write GUI-equivalent plots to <project_dir>/all/figures/<module>/ as
# both static PDF (ggplot2 via ggsave) and interactive HTML (plotly via
# htmlwidgets::saveWidget). Activated by advanced_plots = TRUE.

#' Build (and create) the figures output directory for a module
#'
#' Returns `<project_dir>/all/figures/<module>`, creating it if it does not
#' already exist. Mirrors the existing `all/xlsx_report/`, `all/html_report/`,
#' `all/data/qs2/` convention used by [qcCheckR_export_all()].
#'
#' @keywords internal
#' @param project_dir Character. Path to the project directory.
#' @param module Character. One of `"qcCheckR"`, `"batch_corrector"`,
#'   `"results_explorer"`.
#' @return The figures directory path (invisible).
figures_dir <- function(project_dir, module) {
  valid <- c("qcCheckR", "batch_corrector", "results_explorer")
  if (!isTRUE(module %in% valid)) {
    stop("figures_dir(): 'module' must be one of ",
         paste(shQuote(valid), collapse = ", "),
         "; got ", shQuote(as.character(module)),
         call. = FALSE)
  }
  if (is.null(project_dir) || !nzchar(project_dir)) {
    stop("figures_dir(): 'project_dir' is required.", call. = FALSE)
  }
  out <- file.path(project_dir, "all", "figures", module)
  if (!dir.exists(out)) {
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(out)
}

#' Save one figure to disk as both PDF and HTML
#'
#' Accepts a plot in one of three shapes:
#' - a `list(static = <ggplot>, interactive = <plotly>)` (preferred — both
#'   formats use their best source);
#' - a bare `ggplot` object (PDF from ggsave; HTML from `ggplotly()` wrap);
#' - a bare `plotly` object (HTML only; PDF skipped with a single message).
#'
#' Returns the paths written (without extension) invisibly.
#'
#' @keywords internal
#' @param plot A ggplot, plotly, or `list(static, interactive)`.
#' @param name Character. File basename (no extension).
#' @param project_dir,module Forwarded to [figures_dir()].
#' @param width,height Numeric. PDF dimensions (inches). Defaults 10 x 7.
#' @param dpi Numeric. PDF resolution. Default 300.
#' @return Character. The path stem (`<dir>/<name>`) invisibly.
save_figure <- function(plot, name, project_dir, module,
                        width = 10, height = 7, dpi = 300) {
  if (is.null(plot)) return(invisible(NULL))

  parts <- normalise_plot_pair(plot)
  static <- parts$static
  interactive <- parts$interactive

  out_dir <- figures_dir(project_dir, module)
  stem <- file.path(out_dir, name)

  # --- PDF (static) ---
  if (!is.null(static)) {
    pdf_path <- paste0(stem, ".pdf")
    # cairo_pdf gives better font embedding on Linux/Windows, but the Cairo
    # PDF device can crash the R process (SIGSEGV) in headless macOS
    # environments. A segfault is NOT catchable by the tryCatch below, so we
    # must avoid the call there: use the base pdf device on macOS (and
    # anywhere Cairo is unavailable) and keep cairo_pdf elsewhere.
    pdf_device <- if (isTRUE(capabilities("cairo")) &&
                      !identical(Sys.info()[["sysname"]], "Darwin")) {
      grDevices::cairo_pdf
    } else {
      grDevices::pdf
    }
    tryCatch(
      ggplot2::ggsave(
        filename = pdf_path,
        plot = static,
        width = width,
        height = height,
        units = "in",
        dpi = dpi,
        device = pdf_device
      ),
      error = function(e) {
        # Fall back to ggsave's own default device on any catchable error.
        ggplot2::ggsave(
          filename = pdf_path,
          plot = static,
          width = width,
          height = height,
          units = "in",
          dpi = dpi
        )
      }
    )
  } else {
    message("  save_figure(", name, "): no static ggplot provided; ",
            "skipping PDF (interactive HTML only).")
  }

  # --- HTML (interactive) ---
  html_widget <- interactive
  if (is.null(html_widget) && !is.null(static)) {
    html_widget <- tryCatch(plotly::ggplotly(static),
                            error = function(e) NULL)
  }
  if (!is.null(html_widget)) {
    html_path <- paste0(stem, ".html")
    tryCatch(
      htmlwidgets::saveWidget(
        widget = html_widget,
        file = html_path,
        selfcontained = TRUE
      ),
      error = function(e) {
        # pandoc may be missing on some HPC nodes; fall back to a
        # sidecar-deps HTML so the figure is still recoverable.
        htmlwidgets::saveWidget(
          widget = html_widget,
          file = html_path,
          selfcontained = FALSE
        )
      }
    )
  }

  message("  Saved figure: ", stem, ".{pdf,html}")
  invisible(stem)
}

#' Save a named list of figures
#'
#' Iterates a named list, calling [save_figure()] for each non-null entry.
#' Names are used as the file basename. NULL entries are skipped silently
#' (allows callers to build the list conditionally without `Filter()` noise).
#'
#' @keywords internal
#' @param plots Named list of plots; each entry suitable for [save_figure()].
#' @param project_dir,module,... Forwarded to [save_figure()].
#' @return Character vector of path stems (invisible).
save_figure_list <- function(plots, project_dir, module, ...) {
  if (is.null(plots) || !length(plots)) return(invisible(character()))
  nm <- names(plots)
  if (is.null(nm) || any(!nzchar(nm))) {
    stop("save_figure_list(): 'plots' must be a named list.", call. = FALSE)
  }
  out <- character()
  for (n in nm) {
    p <- plots[[n]]
    if (is.null(p)) next
    stem <- save_figure(p, name = n,
                        project_dir = project_dir,
                        module = module, ...)
    if (!is.null(stem)) out <- c(out, stem)
  }
  invisible(out)
}

# --- Internal helpers ---

#' Normalise a plot input to a (static, interactive) pair
#'
#' Accepts a ggplot, a plotly widget, or a `list(static, interactive)`.
#' @keywords internal
#' @param plot Plot input.
#' @return List with elements `static` (ggplot or NULL) and `interactive`
#'   (plotly or NULL).
normalise_plot_pair <- function(plot) {
  is_gg <- inherits(plot, "ggplot")
  is_plotly <- inherits(plot, c("plotly", "htmlwidget"))

  if (is.list(plot) && !is_gg && !is_plotly &&
      any(c("static", "interactive") %in% names(plot))) {
    static <- plot$static
    interactive <- plot$interactive
    if (!is.null(static) && !inherits(static, "ggplot")) {
      stop("save_figure(): 'static' must be a ggplot object.", call. = FALSE)
    }
    if (!is.null(interactive) &&
        !inherits(interactive, c("plotly", "htmlwidget"))) {
      stop("save_figure(): 'interactive' must be a plotly/htmlwidget object.",
           call. = FALSE)
    }
    return(list(static = static, interactive = interactive))
  }
  if (is_gg)     return(list(static = plot, interactive = NULL))
  if (is_plotly) return(list(static = NULL, interactive = plot))

  stop("save_figure(): 'plot' must be a ggplot, plotly, or ",
       "list(static, interactive); got class ",
       paste(class(plot), collapse = "/"), call. = FALSE)
}
