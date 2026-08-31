#qcCheckR Export Functions ----
# Output saving, report generation, file export
# Split from qcCheckR_Utils.R

#' Format Numeric Columns for Export
#'
#' Rounds numeric columns to 2 d.p. (>=1) or 3 significant figures (<1).
#' Columns whose names match "sample" are left untouched.
#' @keywords internal
#' @param df A data frame.
#' @return The data frame with numeric columns formatted.
format_numeric_columns <- function(df) {
  dplyr::mutate(df, dplyr::across(
    .cols = tidyselect::where(is.numeric) & !dplyr::matches("sample", ignore.case = TRUE),
    .fns = ~ ifelse(is.na(.), NA, ifelse(. < 1, signif(., 3), round(., 2)))
  ))
}

###Primary Function ----
#' Export All Project Outputs
#'
#' Exports the `master_list` to XLSX, HTML, and qs2 formats, including summary tables, QC metrics, and processed data.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param write_rda Logical. When `TRUE` (default) the master_list qs2 file is
#'   written synchronously as part of the export step. Set to `FALSE` when the
#'   caller intends to write it out-of-band (e.g. the Shiny GUI fires a
#'   detached background save so results can render before the qs2 write
#'   completes). When `FALSE`, callers are responsible for invoking
#'   [export_master_list_qs()] themselves. The name is retained from the
#'   prior `.rda` API to avoid churning every caller; the underlying
#'   output is now `.qs2`.
#' @param qs_nthreads Forwarded to [export_master_list_qs()]; see its
#'   documentation for the default.
#' @param qs_compress_level Forwarded to [export_master_list_qs()]; see
#'   its documentation for the default.
#' @return The updated `master_list` with exported files.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_export_all(master_list)
#' }
qcCheckR_export_all <- function(
    master_list,
    write_rda = TRUE,
    qs_nthreads = max(1L, parallel::detectCores() - 1L),
    qs_compress_level = 3L) {
  message("Exporting XLSX report...")
  master_list <- export_xlsx_file(master_list)
  message("Exporting HTML report...")
  master_list <- export_html_report(master_list)
  if (isTRUE(write_rda)) {
    message("Exporting master_list qs2 file...")
    master_list <- export_master_list_qs(
      master_list,
      qs_nthreads = qs_nthreads,
      qs_compress_level = qs_compress_level
    )
  } else {
    message("Skipping qs2 export (write_rda = FALSE); ",
            "caller will write the qs2 file out-of-band.")
  }
  return(master_list)
}

### Sub Functions ----
#' Export XLSX File
#'
#' This function exports the `master_list` data to an XLSX file, including user
#' guide, QC metrics, and processed data.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with the XLSX file exported.
export_xlsx_file <- function(master_list) {
  master_list$summary_tables$odsAreaOverview <- create_user_guide(master_list)

  output_path <- file.path(
    master_list$project_details$project_dir,
    "all",
    "xlsx_report",
    paste0(
      Sys.Date(),
      "_",
      master_list$project_details$user_name,
      "_",
      master_list$project_details$project_name,
      "_lipidData_qcCheckeR.xlsx"
    )
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(output_path)) {
    warning("export_xlsx_file: overwriting existing file: ", output_path, call. = FALSE)
  }
  message("  Writing XLSX to: ", output_path)
  openxlsx::write.xlsx(
    x = list(
      "userGuide" = master_list$summary_tables$odsAreaOverview,
      "QC.platePerformance" = master_list$summary_tables$projectOverview,
      "QC.sampleMV" = master_list$filters$samples.missingValues,
      "QC.lipidsMV" = dplyr::bind_rows(master_list$filters$lipid.missingValues$allPlates),
      "QC.lipidQcRsd" = format_rsd_table(master_list),
      "DATA.peakArea" = format_numeric_columns(
        dplyr::bind_rows(master_list$data$peakArea$sorted) %>%
          dplyr::select(-dplyr::contains("SIL"))
      ),
      "DATA.silPeakArea" = format_numeric_columns(
        dplyr::bind_rows(master_list$data$peakArea$sorted) %>%
          dplyr::select(dplyr::contains("sample") | dplyr::contains("SIL"))
      ),
      "DATA.all.concentration" = format_numeric_columns(
        dplyr::bind_rows(master_list$data$concentration$sorted)
      ),
      "DATA.preProcessed.concentration" = format_numeric_columns(
        filter_concentration(master_list, "concentration")
      ),
      "DATA.all.concentration.S.T." = format_numeric_columns(
        dplyr::bind_rows(master_list$data$concentration$statTargetProcessed)
      ),
      "DATA.preProcessed.conc.S.T." = format_numeric_columns(
        filter_concentration(master_list, "concentration[statTarget]")
      )),
    file = output_path,
    overwrite = TRUE
  )

  return(master_list)
}

#' Export HTML Report
#' This function exports the `master_list` data to an HTML report using a predefined R Markdown template.
#' It renders the report and opens it in the default web browser.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with the HTML report exported.
export_html_report <- function(master_list) {
  output_file <- file.path(
    master_list$project_details$project_dir,
    "all",
    "html_report",
    paste0(
      Sys.Date(),
      "_",
      master_list$project_details$user_name,
      "_",
      master_list$project_details$project_name,
      "_lipidExploreR_qcCheckeR_report.html"
    )
  )

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  chart_names <- names(master_list$control_charts)
  # Normalise NULL -> character(0) so the rest of the function uses a
  # single shape (empty character vector) for "no control charts". Without
  # this, stats::setNames(NULL, character(0)) errors with
  # "attempt to set an attribute on NULL".
  if (is.null(chart_names)) chart_names <- character(0)

  valid_pattern <- "^[A-Za-z0-9_.:()/\\[\\] +-]+$"
  bad_names <- chart_names[!grepl(valid_pattern, chart_names, perl = TRUE)]
  if (length(bad_names) > 0L) {
    stop(
      "export_html_report: control chart names contain disallowed characters: ",
      paste(sQuote(bad_names), collapse = ", "),
      call. = FALSE
    )
  }

  # Short-circuit for the "no control charts" case to avoid paste0()'s
  # scalar+empty-vector recycling quirk: paste0("cc_chunk_", integer(0))
  # returns character(1) ("cc_chunk_"), which setNames() then complains
  # about mismatching a length-0 values vector.
  if (length(chart_names) == 0L) {
    cc_name_map <- stats::setNames(character(0), character(0))
  } else {
    cc_name_map <- stats::setNames(
      chart_names,
      paste0("cc_chunk_", seq_along(chart_names))
    )
  }

  control_chart_code <- character()

  for (chunk_label in names(cc_name_map)) {
    original_name <- cc_name_map[[chunk_label]]
    text <- paste0(
      "#### ",
      original_name,
      "\n",
      "```{r ", chunk_label, ", echo=FALSE, message=FALSE, warning=FALSE, fig.width=14, fig.height=7}\n",
      "master_list$control_charts[[cc_name_map[[\"", chunk_label, "\"]]]]\n",
      "```\n\n"
    )
    control_chart_code <- c(control_chart_code, text)
  }


  template_path <- system.file("templates",
                               "qcCheckR_report_template_HS_V1.Rmd",
                               package = "MStargetR")
  if (!nzchar(template_path)) {
    stop(
      "qcCheckR: report template not found in the MStargetR package installation. ",
      "Re-install the package to restore the template.",
      call. = FALSE
    )
  }
  template_content <- readLines(template_path)


  filled_template <- gsub(
    "control_charts_custom_code_placeholder",
    paste(control_chart_code, collapse = "\n"),
    paste(template_content, collapse = "\n")
  )

  temp_file <- tempfile(fileext = ".Rmd")
  on.exit(unlink(temp_file), add = TRUE)
  writeLines(filled_template, temp_file)

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning(
      "qcCheckR: Package 'rmarkdown' is required to render HTML reports. ",
      "Install it with: install.packages('rmarkdown'). ",
      "Skipping HTML report generation.",
      call. = FALSE
    )
    unlink(temp_file)
    return(master_list)
  }

  if (!rmarkdown::pandoc_available("1.12.3")) {
    warning(
      "qcCheckR: pandoc >= 1.12.3 is required for HTML reports but was not found. ",
      "Skipping HTML report generation. All other outputs (XLSX, qs2) are unaffected.",
      call. = FALSE
    )
    unlink(temp_file)
    return(master_list)
  }

  render_env <- new.env(parent = globalenv())
  render_env$master_list <- master_list
  render_env$cc_name_map <- cc_name_map

  # Reset knitr chunk defaults before rendering. Workflow Rmd files set
  # eval=FALSE globally which persists in the R session and prevents all
  # chunk evaluation in subsequent renders.
  old_knitr_opts <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$set(old_knitr_opts), add = TRUE)
  knitr::opts_chunk$restore()

  tryCatch({
    # Render unembedded so pandoc never assembles the full base64-inflated
    # HTML in one allocation; on large cohorts that exhausts the Windows
    # commit limit. embed_resources_streaming() rebuilds the single-file
    # HTML in R with bounded per-resource memory.
    rendered <- rmarkdown::render(
      input = temp_file,
      output_dir = dirname(output_file),
      output_file = basename(output_file),
      output_options = list(self_contained = FALSE),
      envir = render_env
    )
    if (is.character(rendered) && length(rendered) == 1L &&
        file.exists(rendered)) {
      tryCatch(
        embed_resources_streaming(rendered),
        error = function(e) {
          warning(
            "qcCheckR: resource embedding failed: ", conditionMessage(e),
            ". The report's _files/ sidecar directory has been kept and ",
            "must be distributed alongside the HTML.",
            call. = FALSE
          )
        }
      )
    }
    if (interactive()) utils::browseURL(output_file)
  }, error = function(e) {
    warning(
      "qcCheckR: HTML report generation failed: ", conditionMessage(e),
      ". All other outputs (XLSX, qs2) are unaffected.",
      call. = FALSE
    )
  })
  unlink(temp_file)

  return(master_list)
}

#' Export Master List as QS2 File
#'
#' Exports the `master_list` to a `.qs2` file (qs2 package's
#' multi-threaded zstd serialization format) under
#' `<project_dir>/all/data/qs2/`, suitable for re-loading or sharing.
#' Load with [qs2::qs_read()].
#'
#' Exported (rather than internal) so that the Shiny GUI can fire this
#' as a detached background job after [qcCheckR()] returns
#' (`qcCheckR(..., write_rda = FALSE)` followed by a separate
#' `callr::r_bg()` running this function), letting users view results
#' immediately while the save continues in the background.
#'
#' @param master_list A list containing project details and data.
#' @param qs_nthreads Integer. Worker threads passed to
#'   [qs2::qs_save()]. Defaults to `max(1L, parallel::detectCores() - 1L)`.
#'   Multi-threaded zstd is what makes large-cohort saves complete in
#'   reasonable time; on a 54-plate cohort the previous single-threaded
#'   gzip pass via `base::save()` stalled for hours.
#' @param qs_compress_level Integer. zstd compression level forwarded
#'   to [qs2::qs_save()]. Default `3L` (qs2 default; fast with good
#'   ratio). Higher values (up to 22) shrink the file further at the
#'   cost of CPU time; negative values trade ratio for more speed.
#' @return The updated `master_list` with the qs2 file exported.
#' @keywords internal
#' @export
export_master_list_qs <- function(
    master_list,
    qs_nthreads = max(1L, parallel::detectCores() - 1L),
    qs_compress_level = 3L) {
  output_file <- file.path(
    master_list$project_details$project_dir,
    "all/data/qs2",
    paste0(
      Sys.Date(),
      "_",
      master_list$project_details$project_name,
      "_qcCheckR.qs2"
    )
  )
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  message("  Writing qs2 to: ", output_file)
  tryCatch({
    qs2::qs_save(
      master_list,
      file = output_file,
      compress_level = qs_compress_level,
      nthreads = qs_nthreads
    )
    message("  qs2 save completed: ", output_file)
  }, error = function(e) {
    warning("qs2 save failed: ", conditionMessage(e),
            ". File may be missing: ", output_file, call. = FALSE)
  })

  master_list <- update_script_log(
    master_list,
    "data_exports",
    "plot_generation",
    "Script Complete \n\n\n Thank you for choosing MStargetR"
  )

  return(master_list)
}

#' Create User Guide
#'
#' This function creates a user guide for the `master_list` project, summarizing key project details and metrics.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return A tibble containing the user guide with key metrics and descriptions.
create_user_guide <- function(master_list) {
  #Create sample metrics
  all_peakArea <- dplyr::bind_rows(master_list$data$peakArea$sorted)
  all_conc_st  <- dplyr::bind_rows(master_list$data$concentration$statTargetProcessed)

  sample_tags <- setdiff(
    as.character(unique(dplyr::pull(
      dplyr::select(all_peakArea, sample_type_factor)
    ))),
    "sample"
  )

  user_guide <- tibble::tibble(
    key = c(
      "projectName",
      "user",
      "qcType_preProcessing|filtering",
      "SIL_Int.Std_version",
      "total.StudyPlates",
      "total.Samples",
      "studySamples",
      paste0(sample_tags),
      "total.LipidFeatures",
      "total.MatchedLipidFeatures",
      "total.SIL.Int.Stds",
      "",
      "TAB_DESCRIPTION:",
      "QC.platePerformance",
      "QC.samplesMV",
      "QC.lipidsMV",
      "QC.lipidQcRsd",
      "DATA.lipidPeakArea",
      "DATA.silPeakArea",
      "DATA.all.concentration",
      "DATA.preProcessed.concentration",
      "DATA.all.concentration.S.T.",
      "DATA.preProcessed.conc.S.T."
    ),
    value = c(
      master_list$project_details$project_name,
      master_list$project_details$user_name,
      master_list$project_details$qc_type,
      paste(
        unique(master_list$templates$`Plate SIL version`),
        collapse = ","
      ),
      length(unique(all_peakArea$sample_plate_id)),
      nrow(all_peakArea),
      nrow(dplyr::filter(all_peakArea, sample_type_factor == "sample")),
      sapply(sample_tags, function(tag)
        nrow(dplyr::filter(all_peakArea, sample_type_factor == tag))),
      ncol(all_peakArea %>% dplyr::select(-dplyr::contains("sample"), -dplyr::contains("SIL"))),
      ncol(all_conc_st %>% dplyr::select(-dplyr::contains("sample"))),
      ncol(all_peakArea %>% dplyr::select(dplyr::contains("SIL"))),
      "",
      "",
      "overview of project quality performance (per plate)",
      "detailed overview of sample quality (missing values)",
      "detailed overview of lipid quality (missing values)",
      "detailed overview of lipid quality (% RSD in QC samples)",
      "lipid target peak area integrals (PeakForgeR)",
      "stable isotope labelled internal standard peak area integrals (PeakForgeR)",
      "peakArea >> SIL ratio >> concentration factor adjusted",
      "peakArea >> imputed >> SIL ratio >> concentration factor adjusted >> filtered",
      "peakArea >> imputed >> SIL ratio >> concentration factor adjusted >> statTarget correction",
      "same as above, but filtered"
    )
  )
  return(user_guide)
}

#' Format RSD Table
#'
#' This function formats the RSD table from the `master_list` filters.
#' It rounds the RSD values, adds a data column, and transposes the table for better readability.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return A tibble containing the formatted RSD table with rounded values and transposed structure.
format_rsd_table <- function(master_list) {
  rsd <- master_list$filters$rsd
  if (is.null(rsd) || nrow(rsd) == 0) {
    return(tibble::tibble())
  }

  # Metabolite columns become the rows of the exported sheet; the
  # dataSource.dataBatch keys become its columns.
  value_cols <- setdiff(names(rsd), c("dataSource", "dataBatch"))
  if (length(value_cols) == 0) {
    # Placeholder-only table (no batch produced usable %RSDs). Same contract as
    # the empty guard above: an empty sheet rather than a malformed one.
    return(tibble::tibble())
  }

  # QC-H8: build the header explicitly instead of transposing and promoting
  # row 1 to column names. The old approach inherited whatever keys
  # filters$rsd carried, so a duplicated dataSource.dataBatch pair aborted the
  # whole XLSX export with "Can't transform a data frame with duplicate names".
  # dedupe_rsd_keys() should have removed duplicates upstream; make.unique()
  # keeps this function total regardless of who calls it.
  labels <- make.unique(paste0(rsd$dataSource, ".", rsd$dataBatch))

  values <- as.matrix(rsd[, value_cols, drop = FALSE])
  storage.mode(values) <- "double"

  out <- tibble::as_tibble(round(t(values), 2), .name_repair = "minimal")
  names(out) <- labels
  tibble::add_column(out, data = value_cols, .before = 1)
}

#' Filter Concentration Data
#'
#' This function filters the concentration data from the `master_list` based on the specified source.
#' It removes failed samples and lipids, and applies RSD filters based on the specified source.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param source The data source to filter (e.g., "concentration").
#' @return A tibble containing the filtered concentration data.
filter_concentration <- function(master_list, source) {
  data <- dplyr::bind_rows(master_list$data$concentration[[if (source == "concentration")
    "imputed"
    else
      "statTargetProcessed"]])
  rsd_threshold <- if (!is.null(master_list$project_details$rsd_threshold)) {
    master_list$project_details$rsd_threshold
  } else {
    DEFAULT_RSD_THRESHOLD
  }

  # QC-H2: use strict `>=` here so that only metabolites with RSD strictly
  # less than the threshold are kept. This matches the summary counts
  # (`rsd < 30`) and the QC Checker histogram "pass" semantics.
  rsd_filter <- master_list$filters$rsd %>%
    dplyr::filter(dataSource == source, dataBatch == "allBatches") %>%
    dplyr::select(-dplyr::contains("data")) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ ifelse(. >= rsd_threshold, TRUE, FALSE))) %>%
    dplyr::select(tidyselect::where(~ any(., na.rm = TRUE))) %>%
    names()

  data %>%
    dplyr::filter(!sample_name %in% master_list$filters$failed_samples) %>%
    dplyr::select(-dplyr::any_of(master_list$filters$failed_lipids)) %>%
    dplyr::select(-dplyr::any_of(rsd_filter))
}
