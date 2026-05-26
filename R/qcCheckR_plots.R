#qcCheckR Plot Functions ----
# All plotting/visualization functions
# Split from qcCheckR_Utils.R

## Plot Options ----
###Primary Function ----
#' Set Plot Options
#'
#' Sets the plot color, fill, shape, and size options for the `master_list` data based on sample types and QC type.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with plot options set.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_plot_options(master_list)
#' }
qcCheckR_plot_options <- function(master_list) {

  # Use the actual factor levels present in the data so that scale_*_manual
  # values align with sample_type_factor (which includes "sample" plus all
  # original sample_tags).
  factor_levels <- unique(c("sample", master_list$project_details$sample_tags))
  sample_types  <- factor_levels
  colors <- viridis::viridis(n = length(sample_types))
  master_list$project_details$plot_fill <- stats::setNames(colors, sample_types)

  master_list$project_details$plot_colour <- purrr::set_names(rep("black", length(sample_types)), sample_types)

  master_list$project_details$plot_shape <- purrr::set_names(rep(21, length(sample_types)), sample_types)

  master_list$project_details$plot_size <- purrr::set_names(rep(2, length(sample_types)), sample_types)

  qc_type_raw <- master_list$project_details$qc_type

  # Case-insensitive key lookup
  qc_key <- names(master_list$project_details$plot_colour)[
    tolower(names(master_list$project_details$plot_colour)) == tolower(qc_type_raw)
  ]

  if (length(qc_key) > 0) {
    qc_key <- qc_key[1]
    master_list$project_details$plot_colour[[qc_key]] <- "red"
    master_list$project_details$plot_shape[[qc_key]] <- 23
    master_list$project_details$plot_size[[qc_key]] <- 3
  }

  return(master_list)
}

#.----

## PCA Analysis and Plotting ----
### Primary Function ----
#' PCA Analysis and Plotting
#'
#' Performs PCA analysis on the `master_list` data and generates PCA plots using `ggplot2`.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list`. The \code{pca} slot is populated with:
#'   \describe{
#'     \item{\code{pca$models}}{Named list of \code{ropls::opls} model objects, one per
#'       source/key/preprocessed combination (e.g. \code{"peakArea.imputed.raw"},
#'       \code{"concentration.statTargetProcessed.preprocessed"}).}
#'     \item{\code{pca$scores}}{Named list of score data frames (one per model),
#'       retaining all \code{sample_*} metadata columns alongside PC coordinates.}
#'     \item{\code{pca$plot}}{Named list of two \code{ggplot} objects, keyed by
#'       \code{"sample_type_factor"} and \code{"sample_plate_id"}.}
#'   }
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_PCA(master_list)
#' }
qcCheckR_PCA <- function(master_list) {
  message("  Running PCA analysis...")
  master_list$pca <- list(models = list(),
                          scores = list(),
                          plot = list())

  for (source in c("peakArea", "concentration")) {
    message("    PCA on ", source, " (raw)...")
    master_list <- run_pca_model(master_list, source, preprocessed = FALSE)
    message("    PCA on ", source, " (pre-processed)...")
    master_list <- run_pca_model(master_list, source, preprocessed = TRUE)
  }

  for (fill_var in c("sample_type_factor", "sample_plate_id")) {
    master_list$pca$plot[[fill_var]] <- generate_pca_ggplot(master_list, fill_var)
  }

  message("  PCA analysis complete. ", length(master_list$pca$models), " models fitted.")
  return(master_list)
}

###Sub Functions ----
#' Run PCA Model
#'
#' This function runs a PCA model on the specified data source from the `master_list`.
#' It preprocesses the data by filtering out failed samples and high RSD lipids if `preprocessed` is set to TRUE.
#' It then performs PCA using the `ropls` package and stores the model and scores in the `master_list`.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param source The data source to run PCA on (e.g., "peakArea", "concentration", "concentration.statTarget").
#' @param preprocessed Logical indicating whether to preprocess the data (default is FALSE).
#' @return The updated `master_list` with PCA models and scores.
run_pca_model <- function(master_list, source, preprocessed = FALSE) {
  # Determine the data key based on the source
  if (source == "peakArea") {
    data_keys <- "imputed"
  } else if (source == "concentration") {
    data_keys <- c("imputed", "statTargetProcessed")
  } else {
    stop("run_pca_model: unsupported source '", source,
         "'. Must be 'peakArea' or 'concentration'.", call. = FALSE)
  }

  for (data_key in data_keys) {
    #validate source and data_key -- skip missing or empty entries gracefully
    if (!source %in% names(master_list$data) ||
        !data_key %in% names(master_list$data[[source]]) ||
        length(master_list$data[[source]][[data_key]]) == 0) {
      message("qcCheckR PCA: skipping ", source, "/", data_key, " - not found or empty.")
      next
    }

    #Here we remove the cols that are not present in all three dataframes
    sorted_data <- master_list$data$concentration$sorted
    if (is.null(sorted_data) || length(sorted_data) == 0) {
      stop("qcCheckR: No sorted concentration data available for PCA.", call. = FALSE)
    }
    cols_to_keep <- Reduce(intersect, lapply(sorted_data, colnames))
    data <- dplyr::bind_rows(master_list$data[[source]][[data_key]]) %>%
      dplyr::select(dplyr::any_of(cols_to_keep))




    if (preprocessed) {
      data <- data %>%
        dplyr::filter(!sample_name %in% master_list$filters$failed_samples)

      failed_lipids <- master_list$filters$failed_lipids
      high_rsd_lipids <- master_list$filters$rsd %>%
        dplyr::filter(dataSource == source, dataBatch == "allBatches") %>%
        dplyr::select(-dplyr::contains("data")) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ ifelse(. > 30, TRUE, FALSE))) %>%
        dplyr::select(tidyselect::where(~ any(., na.rm = TRUE))) %>%
        names()

      data <- data %>%
        dplyr::select(-dplyr::any_of(c(failed_lipids, high_rsd_lipids)))
    }

    pca_matrix <- data %>%
      tibble::column_to_rownames("sample_name") %>%
      dplyr::select(-dplyr::contains("sample"), -dplyr::contains("SIL")) %>%
      as.matrix()

    model_name <- if (preprocessed)
      paste0(source, ".", data_key, ".preProcessed")
    else
      paste0(source, ".", data_key)

    safe_predI <- min(3, min(dim(pca_matrix)) - 1L)
    if (safe_predI < 1L) {
      message("qcCheckR PCA: skipping ", model_name, " - matrix too small for PCA.")
      next
    }
    pca_model <- tryCatch(
      ropls::opls(
        x = pca_matrix,
        y = NULL,
        crossvalI = 1,
        predI = safe_predI,
        algoC = "nipals",
        log10L = FALSE,
        scale = "pareto",
        plotSubC = NA,
        fig.pdfC = "none"
      ),
      error = function(e) {
        message("qcCheckR PCA: ropls::opls failed for ", model_name, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(pca_model)) next
    master_list$pca$models[[model_name]] <- pca_model

    score_cols <- colnames(pca_model@scoreMN)
    pc_names   <- c("PC1", "PC2", "PC3")[seq_along(score_cols)]
    rename_map <- stats::setNames(score_cols, pc_names)
    scores <- pca_model@scoreMN %>%
      tibble::as_tibble(.name_repair = "minimal") %>%
      dplyr::rename(!!!rename_map) %>%
      tibble::add_column(sample_name = rownames(pca_matrix), .before = 1) %>%
      dplyr::left_join(data %>% dplyr::select(dplyr::contains("sample")), by = "sample_name")
    # Ensure PC1/PC2/PC3 always present (NA when model has fewer components)
    for (.pc in c("PC1", "PC2", "PC3")) {
      if (!.pc %in% colnames(scores)) scores[[.pc]] <- NA_real_
    }
    rm(.pc)

    scores$sample_data_source <- paste0(
      if (preprocessed)
        paste0(source, ".", data_key, ".preProcessed")
      else
        paste0(source, ".", data_key),
      ": s=",
      nrow(pca_matrix),
      ", l=",
      ncol(pca_matrix)
    )

    master_list$pca$scores[[model_name]] <- scores
  }

  return(master_list)
}

#' Generate PCA ggplot
#'
#' This function generates a PCA plot using `ggplot2` for the PCA scores stored in the `master_list`.
#' It allows for coloring the points by a specified variable (e.g., sample type or plate ID).
#' @keywords internal
#' @param master_list A list containing project details and PCA scores.
#' @param fill_var The variable to color the points by (e.g., "sample_type_factor", "sample_plate_id").
#' @return A `plotly` object representing the PCA scores (the inner `ggplot`
#'   is exposed separately via [generate_pca_ggplot_static()] so the same
#'   figure can be written to disk as a static PDF for R users).
generate_pca_ggplot <- function(master_list, fill_var) {
  plotly::ggplotly(generate_pca_ggplot_static(master_list, fill_var))
}

#' Build the static PCA ggplot (no plotly wrap)
#'
#' Underlying ggplot used by [generate_pca_ggplot()] and by
#' [qcCheckR_collect_plots()] when `advanced_plots = TRUE`. Same data and
#' aesthetics; just stops before `plotly::ggplotly()` so the result is a
#' raw ggplot suitable for `ggsave()`.
#' @keywords internal
#' @inheritParams generate_pca_ggplot
#' @return A `ggplot` object.
generate_pca_ggplot_static <- function(master_list, fill_var) {
  all_scores <- dplyr::bind_rows(master_list$pca$scores)

  #Factor and arrange to ensure consistent ordering in the plot
  all_scores$source_prefix <- sub(":.*", "", all_scores$sample_data_source) %>%
    factor(
      levels = c(
        "peakArea.imputed",
        "peakArea.imputed.preProcessed",
        "concentration.imputed",
        "concentration.imputed.preProcessed",
        "concentration.statTargetProcessed",
        "concentration.statTargetProcessed.preProcessed"
      )
    )
  all_scores <- all_scores %>% dplyr::arrange(source_prefix)
  all_scores$source_suffix <- sub(".*: ", "", all_scores$sample_data_source)
  all_scores$facet_label <- paste0(all_scores$source_prefix, ":", all_scores$source_suffix)
  all_scores$facet_label <- factor(all_scores$facet_label, levels = unique(all_scores$facet_label))

  ggplot2::ggplot(
    data = all_scores,
    ggplot2::aes(
      x = PC1,
      y = PC2,
      group = sample_name,
      fill = .data[[fill_var]],
      color = sample_type_factor,
      shape = sample_type_factor,
      size = sample_type_factor
    )
  ) +
    ggplot2::geom_vline(xintercept = 0, colour = "darkgrey") +
    ggplot2::geom_hline(yintercept = 0, color = "darkgrey") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::scale_shape_manual(
      values = master_list$project_details$plot_shape[
        names(master_list$project_details$plot_shape) %in%
          unique(as.character(all_scores$sample_type_factor))
      ]
    ) +
    ggplot2::scale_color_manual(
      values = master_list$project_details$plot_colour[
        names(master_list$project_details$plot_colour) %in%
          unique(as.character(all_scores$sample_type_factor))
      ]
    ) +
    ggplot2::scale_size_manual(
      values = master_list$project_details$plot_size[
        names(master_list$project_details$plot_size) %in%
          unique(as.character(all_scores$sample_type_factor))
      ]
    ) +
    ggplot2::guides(
      shape = "none",
      size = "none",
      color = "none",
      fill = ggplot2::guide_legend(title = fill_var)
    ) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(facet_label),
      scales = "free",
      ncol = 2
    ) +
    ggplot2::labs(
      title = paste0(
        "PCA scores; coloured by ",
        fill_var,
        "; ",
        master_list$project_details$project_name
      )
    )
}

#.----

## Run Order Plots ----
###Primary Function ----
#' Run Order Plots
#'
#' Generates run order plots for the `master_list` data, showing PCA scores versus run order using `ggplot2`.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with run order plots stored in `master_list$pca$scoresRunOrder`.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_run_order_plots(master_list)
#' }
qcCheckR_run_order_plots <- function(master_list) {
  # Materialise once; reused for boundary computation and all three PC plots.
  all_scores <- dplyr::bind_rows(master_list$pca$scores)

  #Get plate boundaries and labels
  boundary_data <- all_scores %>%
    dplyr::select(sample_run_index, sample_plate_id) %>%
    dplyr::distinct()

  plate_ids <- unique(boundary_data$sample_plate_id)
  plate_ranges <- lapply(plate_ids, function(plate) {
    plate_data <- boundary_data[boundary_data$sample_plate_id == plate, ]
    list(
      boundaries = c(min(plate_data$sample_run_index) - 0.5,
                     max(plate_data$sample_run_index) + 0.5),
      label = stats::median(plate_data$sample_run_index)
    )
  })
  boundaries <- unique(c(0.5, unlist(lapply(plate_ranges, `[[`, "boundaries"))))
  labels     <- unlist(lapply(plate_ranges, `[[`, "label"))

  master_list$pca$scoresRunOrder <- list()

  for (pc in c("PC1", "PC2", "PC3")) {
    master_list$pca$scoresRunOrder[[pc]] <- plot_run_order(
      scores = all_scores,
      pc = pc,
      boundaries = boundaries,
      plot_settings = master_list$project_details
    )
  }

  return(master_list)
}

### Sub Functions ----
#' Plot Run Order
#'
#' This function generates a run order plot for PCA scores using `ggplot2`.
#' It plots the PCA scores against the sample run index, with vertical lines indicating plate boundaries and annotations for plate IDs.
#' @keywords internal
#' @param scores A tibble containing PCA scores and sample information.
#' @param pc The principal component to plot (e.g., "PC1", "PC2", "PC3").
#' @param boundaries A vector of boundaries for the plates.
#' @param plot_settings A list containing plot settings such as colors, shapes, and sizes.
#' @return A `plotly` object (the inner ggplot is exposed separately via
#'   [plot_run_order_static()] so the same figure can be written to disk as
#'   a static PDF for R users).
plot_run_order <- function(scores,
                           pc,
                           boundaries,
                           plot_settings) {
  plotly::ggplotly(
    plot_run_order_static(scores, pc, boundaries, plot_settings),
    tooltip = "text"
  )
}

#' Build the static run-order ggplot (no plotly wrap)
#'
#' Underlying ggplot used by [plot_run_order()] and by
#' [qcCheckR_collect_plots()] when `advanced_plots = TRUE`.
#' @keywords internal
#' @inheritParams plot_run_order
#' @return A `ggplot` object.
plot_run_order_static <- function(scores,
                                  pc,
                                  boundaries,
                                  plot_settings) {
  ggplot2::ggplot(
    scores,
    ggplot2::aes(
      x = sample_run_index,
      y = .data[[pc]],
      group = sample_name,
      fill = sample_type_factor,
      color = sample_type_factor,
      shape = sample_type_factor,
      size = sample_type_factor,
      text = paste0(sample_name, " (", sample_plate_id, ")")
    )
  ) +
    ggplot2::geom_vline(xintercept = boundaries, linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::scale_shape_manual(values = plot_settings$plot_shape) +
    ggplot2::scale_fill_manual(values = plot_settings$plot_fill) +
    ggplot2::scale_color_manual(values = plot_settings$plot_colour) +
    ggplot2::scale_size_manual(values = plot_settings$plot_size) +
    ggplot2::ylab(pc) +
    ggplot2::guides(
      shape = "none",
      size = "none",
      color = "none",
      fill = ggplot2::guide_legend(title = "sample_type_factor")
    ) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(sample_data_source),
      ncol = 1,
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = paste0(
        pc,
        "; run order (x) vs PCA scores (y); ",
        plot_settings$project_name
      )
    ) +
    ggplot2::theme(text = ggplot2::element_text(size = 12))
}

#.----

## Target Control Charts ----
### Primary Function ----
#' Target Control Charts
#'
#' Generates control charts for the `master_list` data, showing the values of target metabolites and SIL internal standards across different sample types and data sources.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @return The updated `master_list` with control charts stored in `master_list$control_charts`.
#' @examples
#' \dontrun{
#' master_list <- qcCheckR_target_control_charts(master_list)
#' }
qcCheckR_target_control_charts <- function(master_list) {
  master_list$control_charts <- list()

  common_control_charts <- get_common_control_metabolites(master_list)
  plate_boundaries <- get_plate_boundaries(master_list)

  # Hoist repeated bind_rows calls; each would otherwise be re-evaluated
  # once per metabolite inside plot_control_chart.
  area_imp   <- dplyr::bind_rows(master_list$data$peakArea$imputed)
  conc_imp   <- dplyr::bind_rows(master_list$data$concentration$imputed)
  conc_st    <- dplyr::bind_rows(master_list$data$concentration$statTargetProcessed)

  for (metabolite in common_control_charts) {
    master_list$control_charts[[metabolite]] <- plot_control_chart(
      master_list = master_list,
      metabolite = metabolite,
      plate_boundaries = plate_boundaries,
      area_imp = area_imp,
      conc_imp = conc_imp,
      conc_st  = conc_st
    )
  }

  master_list <- update_script_log(master_list,
                                   "plot_generation",
                                   "summary_report",
                                   "data_exports")

  return(master_list)
}

###Sub Functions ----
#' Get Common Control Metabolites
#' This function retrieves the common control metabolites across different SIL versions in the `master_list`.
#' It intersects the precursor names of control charts from each SIL version's guide.
#' @keywords internal
#' @param master_list A list containing project details and templates.
#' @return A vector of common control metabolites.
get_common_control_metabolites <- function(master_list) {
  SIL_versions <- unique(unlist(master_list$templates[["Plate SIL version"]]))
  precursor_lists <- list()
  for (ver in SIL_versions) {
    sil_guide <- master_list$templates$mrm_guides[[ver]]$SIL_guide
    if (!"control_chart" %in% names(sil_guide)) {
      stop("get_common_control_metabolites: 'control_chart' column is missing from SIL guide for version '",
           ver, "'. Check that the guide template matches the expected format.", call. = FALSE)
    }
    precursor_lists[[ver]] <- sil_guide$`Precursor Name`[sil_guide$control_chart == TRUE]
  }
  result <- Reduce(dplyr::intersect, precursor_lists)
  if (length(result) == 0) {
    warning("get_common_control_metabolites: intersection of control_chart metabolites across SIL versions is empty; no control charts will be generated.")
  }
  result
}

#' Get Plate Boundaries
#'
#' This function retrieves the boundaries for each plate in the `master_list` data.
#' It calculates the minimum and maximum run indices for each plate and creates a list of boundaries and labels for plotting.
#' @keywords internal
#' @param master_list A list containing project details and PCA scores.
#' @return A vector of unique boundaries for the plates.
get_plate_boundaries <- function(master_list) {
  boundary_data <- dplyr::bind_rows(master_list$pca$scores) %>%
    dplyr::select(sample_run_index, sample_plate_id) %>%
    dplyr::distinct()

  boundaries <- c(0.5)
  for (plate in unique(boundary_data$sample_plate_id)) {
    plate_data <- boundary_data %>% dplyr::filter(sample_plate_id == plate)
    boundaries <- c(
      boundaries,
      min(plate_data$sample_run_index) - 0.5,
      max(plate_data$sample_run_index) + 0.5
    )
  }
  unique(boundaries)
}

#' Get Plate Annotations
#'
#' This function retrieves the median run index for each plate in the `master_list` data.
#' It creates a tibble with sample data source, plate ID, run index, and placeholder values for PCA components and value.
#' @keywords internal
#' @param master_list A list containing project details and PCA scores.
#' @return A tibble containing plate annotations with median run indices.
get_plate_annotations <- function(master_list) {
  boundary_data <- dplyr::bind_rows(master_list$pca$scores) %>%
    dplyr::select(sample_run_index, sample_plate_id) %>%
    dplyr::distinct()

  coords <- sapply(unique(boundary_data$sample_plate_id), function(plate) {
    stats::median(boundary_data$sample_run_index[boundary_data$sample_plate_id == plate])
  })

  n_plates <- length(coords)
  y_offsets <- rep(c(0, 1), length.out = n_plates)

  tibble::tibble(
    sample_data_source = "x.plateID",
    sample_plate_id = names(coords),
    sample_run_index = coords,
    PC1 = y_offsets,
    PC2 = y_offsets,
    PC3 = y_offsets,
    value = y_offsets
  )
}

#' Plot Control Chart
#'
#' This function generates a control chart for a specific metabolite in the `master_list`.
#' It combines data from peak area, SIL peak area, concentration, and stat target concentration,
#' and plots the values against the sample run index.
#' @keywords internal
#' @param master_list A list containing project details and data.
#' @param metabolite The metabolite to plot in the control chart.
#' @param plate_boundaries A vector of plate boundaries for vertical lines in the plot.
#' @param area_imp Pre-bound peak area imputed tibble (optional; computed internally if NULL).
#' @param conc_imp Pre-bound concentration imputed tibble (optional; computed internally if NULL).
#' @param conc_st Pre-bound statTargetProcessed concentration tibble (optional; computed internally if NULL).
#' @return A `plotly` object representing the control chart for the
#'   specified metabolite (the inner ggplot is exposed separately via
#'   [plot_control_chart_static()] so the same figure can be written to
#'   disk as a static PDF for R users).
plot_control_chart <- function(master_list,
                               metabolite,
                               plate_boundaries,
                               area_imp = NULL,
                               conc_imp = NULL,
                               conc_st  = NULL) {
  plotly::ggplotly(
    plot_control_chart_static(master_list, metabolite, plate_boundaries,
                              area_imp, conc_imp, conc_st),
    tooltip = "text"
  )
}

#' Build the static control-chart ggplot (no plotly wrap)
#'
#' Underlying ggplot used by [plot_control_chart()] and by
#' [qcCheckR_collect_plots()] when `advanced_plots = TRUE`.
#' @keywords internal
#' @inheritParams plot_control_chart
#' @return A `ggplot` object.
plot_control_chart_static <- function(master_list,
                                      metabolite,
                                      plate_boundaries,
                                      area_imp = NULL,
                                      conc_imp = NULL,
                                      conc_st  = NULL) {
  # Allow callers to pass pre-bound tibbles to avoid O(metabolites) re-binds.
  if (is.null(area_imp)) area_imp <- dplyr::bind_rows(master_list$data$peakArea$imputed)
  if (is.null(conc_imp)) conc_imp <- dplyr::bind_rows(master_list$data$concentration$imputed)
  if (is.null(conc_st))  conc_st  <- dplyr::bind_rows(master_list$data$concentration$statTargetProcessed)

  sil_versions <- unique(unlist(master_list$templates[["Plate SIL version"]]))
  sil_notes <- unique(unlist(lapply(sil_versions, function(ver) {
    guide <- master_list$templates$mrm_guides[[ver]]$SIL_guide
    guide$Note[guide$`Precursor Name` == metabolite &
                 guide$control_chart == TRUE]
  })))

  data_combined <- dplyr::bind_rows(
    area_imp %>%
      dplyr::select(dplyr::contains("sample"), dplyr::any_of(metabolite)) %>%
      dplyr::mutate(sample_data_source = ".peakArea"),

    area_imp %>%
      dplyr::select(dplyr::contains("sample"), dplyr::any_of(sil_notes)) %>%
      dplyr::mutate(SIL = rowSums(
        dplyr::across(dplyr::any_of(sil_notes)), na.rm = TRUE
      )) %>%
      dplyr::rename(!!metabolite := SIL) %>%
      dplyr::select(-dplyr::any_of(sil_notes)) %>%
      dplyr::mutate(sample_data_source = ".SIL.peakArea"),

    conc_imp %>%
      dplyr::select(dplyr::contains("sample"), dplyr::any_of(metabolite)) %>%
      dplyr::mutate(sample_data_source = "concentration.preprocessed"),

    conc_st %>%
      dplyr::select(dplyr::contains("sample"), dplyr::any_of(metabolite)) %>%
      dplyr::mutate(sample_data_source = "statTargetConcentration.preprocessed")
  ) %>%
    dplyr::rename(value = !!metabolite)

  ggplot2::ggplot(
    data_combined,
    ggplot2::aes(
      x = sample_run_index,
      y = value,
      group = sample_name,
      fill = sample_type_factor,
      color = sample_type_factor,
      shape = sample_type_factor,
      size = sample_type_factor,
      text = paste0(sample_name, " (", sample_plate_id, ")")
    )
  ) +
    ggplot2::geom_vline(xintercept = plate_boundaries, linetype = "dashed") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::scale_shape_manual(values = master_list$project_details$plot_shape) +
    ggplot2::scale_fill_manual(values = master_list$project_details$plot_fill) +
    ggplot2::scale_color_manual(values = master_list$project_details$plot_colour) +
    ggplot2::scale_size_manual(values = master_list$project_details$plot_size) +
    ggplot2::ylab(metabolite) +
    ggplot2::guides(
      shape = "none",
      size = "none",
      color = "none",
      fill = ggplot2::guide_legend(title = "sample_type")
    ) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(sample_data_source),
      ncol = 1,
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = paste0(
        metabolite,
        "; control chart; ",
        master_list$project_details$project_name
      )
    )
}

#.----

## Advanced QC Plots (R-side parity with GUI) ----
# These mirror the inline plotly figures the Shiny QC Check tab renders
# (RSD histogram, missing values, sample type pie, plate distribution).
# Each constructor returns list(static = ggplot, interactive = plotly) so
# save_figure() can write both a publication-quality PDF and the
# interactive HTML the GUI shows. Wired through qcCheckR_collect_plots()
# and surfaced to users via qcCheckR(advanced_plots = TRUE).

# Metadata columns excluded when scanning numeric metabolite columns. Kept
# in sync with the Shiny app's `meta_cols` in inst/shiny/MStargetR_app/server.R.
.qc_meta_cols <- c("file_name", "sample_name", "sample_type", "batch",
                   "run_order", "injection_order", "group", "class", "type",
                   "sample_plate_id", "sample_run_index", "sample_type_factor",
                   "sample_type_factor_rev", "sample_plate_order",
                   "sample_matrix", "sample_data_source", "sample_timestamp")

# Internal: extract a per-metabolite RSD vector from filters$rsd. Mirrors
# the Shiny helper get_qc_rsd_values() so the GUI histogram and the R-side
# PDF share one source of truth.
qc_rsd_vector <- function(master_list,
                          stage = c("concentration",
                                    "concentration[statTarget]",
                                    "peakArea")) {
  stage <- match.arg(stage)
  tbl <- tryCatch(master_list$filters$rsd, error = function(e) NULL)
  if (is.null(tbl) || !nrow(tbl)) return(NULL)
  row <- tbl[tbl$dataSource == stage & tbl$dataBatch == "allBatches", ,
             drop = FALSE]
  if (!nrow(row)) return(NULL)
  met_cols <- setdiff(names(row), c("dataSource", "dataBatch"))
  if (!length(met_cols)) return(NULL)
  vals <- suppressWarnings(as.numeric(unlist(row[1, met_cols],
                                             use.names = FALSE)))
  names(vals) <- met_cols
  vals
}

#' RSD histogram (advanced_plots)
#'
#' Distribution of per-metabolite QC %RSD with a vertical reference line
#' at `fail_thr` (matches the Shiny QC Check tab's threshold annotation).
#'
#' @keywords internal
#' @param master_list A qcCheckR master_list.
#' @param fail_thr Numeric. RSD% threshold to mark. Default 30.
#' @return `list(static = <ggplot>, interactive = <plotly>)`, or `NULL`
#'   if no RSD values are available.
qc_plot_rsd_histogram <- function(master_list, fail_thr = 30) {
  rsd_vec <- qc_rsd_vector(master_list, stage = "concentration")
  if (is.null(rsd_vec) || !length(rsd_vec)) return(NULL)
  rsd_df <- data.frame(metabolite = names(rsd_vec),
                       rsd = as.numeric(rsd_vec),
                       stringsAsFactors = FALSE)
  rsd_df <- rsd_df[!is.na(rsd_df$rsd), , drop = FALSE]
  if (!nrow(rsd_df)) return(NULL)

  static <- ggplot2::ggplot(rsd_df, ggplot2::aes(x = .data[["rsd"]])) +
    ggplot2::geom_histogram(bins = 30, fill = "#377EB8",
                            colour = "white", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = fail_thr, colour = "red",
                        linetype = "dashed", linewidth = 0.7) +
    ggplot2::annotate("text", x = fail_thr, y = Inf,
                       label = paste0(fail_thr, "% threshold"),
                       colour = "red", hjust = -0.05, vjust = 1.4, size = 3.5) +
    ggplot2::labs(x = "RSD %", y = "Number of Metabolites",
                  title = "QC %RSD distribution") +
    ggplot2::theme_bw()

  interactive <- plotly::plot_ly(
    data = rsd_df, x = ~rsd, type = "histogram",
    nbinsx = 30,
    marker = list(color = "#377EB8",
                  line = list(color = "white", width = 0.5)),
    hovertemplate = "RSD: %{x:.1f}%<br>Count: %{y}<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = "RSD %"),
      yaxis = list(title = "Number of Metabolites"),
      shapes = list(list(type = "line", x0 = fail_thr, x1 = fail_thr,
                         y0 = 0, y1 = 1, yref = "paper",
                         line = list(color = "red", dash = "dash",
                                     width = 1.5))),
      annotations = list(list(x = fail_thr + 1, y = 1, yref = "paper",
                              text = paste0(fail_thr, "% threshold"),
                              showarrow = FALSE, xanchor = "left",
                              font = list(color = "red", size = 11)))
    )

  list(static = static, interactive = interactive)
}

#' Missing-value bar chart (advanced_plots)
#'
#' Top-40 metabolites by % missing values, computed from pre-imputation
#' peak-area data (falling back to imputed concentration if peak-area is
#' absent — mirrors the Shiny QC tab's lookup order).
#'
#' @keywords internal
#' @param master_list A qcCheckR master_list.
#' @return `list(static, interactive)`, or `NULL` if no missing values.
qc_plot_missing_values <- function(master_list) {
  df <- NULL
  if (!is.null(master_list$data$peakArea$sorted)) {
    df <- dplyr::bind_rows(master_list$data$peakArea$sorted)
  }
  if (is.null(df) || !nrow(df)) {
    if (!is.null(master_list$data$concentration$imputed)) {
      df <- dplyr::bind_rows(master_list$data$concentration$imputed)
    }
  }
  if (is.null(df) || !nrow(df)) return(NULL)

  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, .qc_meta_cols)
  if (!length(num_cols)) return(NULL)

  pct_missing <- vapply(num_cols, function(m) {
    sum(is.na(df[[m]])) / nrow(df) * 100
  }, numeric(1))
  mv_df <- data.frame(metabolite = names(pct_missing),
                      pct = as.numeric(pct_missing),
                      stringsAsFactors = FALSE)
  mv_df <- mv_df[mv_df$pct > 0, , drop = FALSE]
  if (!nrow(mv_df)) return(NULL)

  mv_df <- mv_df[order(mv_df$pct, decreasing = TRUE), , drop = FALSE]
  mv_df <- utils::head(mv_df, 40)
  mv_df$metabolite <- factor(mv_df$metabolite, levels = rev(mv_df$metabolite))
  mv_df$band <- ifelse(mv_df$pct > 50, ">50%",
                ifelse(mv_df$pct > 20, "20-50%", "<=20%"))
  band_cols <- c(">50%" = "#E41A1C", "20-50%" = "#FF7F00", "<=20%" = "#377EB8")

  static <- ggplot2::ggplot(
    mv_df,
    ggplot2::aes(x = .data[["pct"]], y = .data[["metabolite"]],
                 fill = .data[["band"]])
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = band_cols, name = "Missing band") +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::labs(x = "Missing Values (%)", y = NULL,
                  title = "Top 40 metabolites by missing values") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8))

  interactive <- plotly::plot_ly(
    data = mv_df, y = ~metabolite, x = ~pct,
    type = "bar", orientation = "h",
    marker = list(color = band_cols[as.character(mv_df$band)]),
    hovertemplate = "%{y}: %{x:.1f}% missing<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = "Missing Values (%)", range = c(0, 100)),
      yaxis = list(title = "", tickfont = list(size = 9)),
      margin = list(l = 150)
    )

  list(static = static, interactive = interactive)
}

#' Sample-type distribution pie (advanced_plots)
#'
#' Pie chart of counts per `sample_type_factor` (falling back to
#' `sample_type`). Mirrors the Shiny QC tab's `qc_sample_type_pie`.
#'
#' @keywords internal
#' @param master_list A qcCheckR master_list.
#' @return `list(static, interactive)`, or `NULL`.
qc_plot_sample_type_distribution <- function(master_list) {
  if (is.null(master_list$data$concentration$corrected)) return(NULL)
  df <- dplyr::bind_rows(master_list$data$concentration$corrected)
  if (is.null(df) || !nrow(df)) return(NULL)
  type_col <- if ("sample_type_factor" %in% names(df))
    "sample_type_factor" else if ("sample_type" %in% names(df))
    "sample_type" else return(NULL)
  counts <- as.data.frame(table(df[[type_col]]), stringsAsFactors = FALSE)
  names(counts) <- c("type", "count")

  static <- ggplot2::ggplot(
    counts,
    ggplot2::aes(x = "", y = .data[["count"]], fill = .data[["type"]])
  ) +
    ggplot2::geom_col(width = 1, colour = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::labs(title = "Sample Types", fill = NULL, x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  interactive <- plotly::plot_ly(
    data = counts, labels = ~type, values = ~count, type = "pie",
    textinfo = "label+percent",
    hovertemplate = "%{label}: %{value} samples (%{percent})<extra></extra>"
  ) |>
    plotly::layout(title = list(text = "Sample Types",
                                 font = list(size = 14)))

  list(static = static, interactive = interactive)
}

#' Plate distribution bar (advanced_plots)
#'
#' Bar chart of sample counts per `sample_plate_id`. Mirrors the Shiny
#' QC tab's `qc_plate_bar`.
#'
#' @keywords internal
#' @param master_list A qcCheckR master_list.
#' @return `list(static, interactive)`, or `NULL`.
qc_plot_plate_distribution <- function(master_list) {
  if (is.null(master_list$data$concentration$corrected)) return(NULL)
  df <- dplyr::bind_rows(master_list$data$concentration$corrected)
  if (is.null(df) || !nrow(df) ||
      !"sample_plate_id" %in% names(df)) return(NULL)
  counts <- as.data.frame(table(df$sample_plate_id), stringsAsFactors = FALSE)
  names(counts) <- c("plate", "count")

  static <- ggplot2::ggplot(
    counts,
    ggplot2::aes(x = .data[["plate"]], y = .data[["count"]])
  ) +
    ggplot2::geom_col(fill = "#377EB8") +
    ggplot2::labs(x = "Plate", y = "Number of Samples",
                  title = "Samples per Plate") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  interactive <- plotly::plot_ly(
    data = counts, x = ~plate, y = ~count, type = "bar",
    marker = list(color = "#377EB8"),
    hovertemplate = "Plate %{x}: %{y} samples<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = "Plate"),
      yaxis = list(title = "Number of Samples"),
      title = list(text = "Samples per Plate", font = list(size = 14))
    )

  list(static = static, interactive = interactive)
}

#' Collect all QC plots for advanced_plots = TRUE
#'
#' Builds the named list of plots written to
#' `<project_dir>/all/figures/qcCheckR/` when `qcCheckR(advanced_plots = TRUE)`.
#' Pairs the static ggplot (PDF) with the interactive plotly (HTML) for
#' every figure the GUI renders, including per-metabolite control charts.
#'
#' @keywords internal
#' @param master_list A qcCheckR master_list (post-pipeline).
#' @param fail_thr Numeric. RSD% threshold annotated on the histogram.
#' @return Named list; each entry is `list(static, interactive)` and the
#'   names become the saved-file basenames.
qcCheckR_collect_plots <- function(master_list, fail_thr = 30) {
  plots <- list()

  # PCA: rebuild ggplots via the *_static() sibling; pair with the
  # already-rendered plotly objects in master_list.
  for (fill_var in names(master_list$pca$plot)) {
    static <- tryCatch(generate_pca_ggplot_static(master_list, fill_var),
                       error = function(e) NULL)
    plots[[paste0("pca_", fill_var)]] <- list(
      static = static,
      interactive = master_list$pca$plot[[fill_var]]
    )
  }

  # Run-order: per-PC. Recompute boundaries the same way
  # qcCheckR_run_order_plots() did so the static and interactive lay out
  # identically.
  if (!is.null(master_list$pca$scoresRunOrder) &&
      !is.null(master_list$pca$scores)) {
    all_scores <- dplyr::bind_rows(master_list$pca$scores)
    boundary_data <- all_scores %>%
      dplyr::select(sample_run_index, sample_plate_id) %>%
      dplyr::distinct()
    plate_ranges <- lapply(unique(boundary_data$sample_plate_id), function(p) {
      pd <- boundary_data[boundary_data$sample_plate_id == p, , drop = FALSE]
      c(min(pd$sample_run_index) - 0.5, max(pd$sample_run_index) + 0.5)
    })
    boundaries <- unique(c(0.5, unlist(plate_ranges)))
    for (pc in names(master_list$pca$scoresRunOrder)) {
      static <- tryCatch(
        plot_run_order_static(all_scores, pc, boundaries,
                              master_list$project_details),
        error = function(e) NULL
      )
      plots[[paste0("runorder_", tolower(pc))]] <- list(
        static = static,
        interactive = master_list$pca$scoresRunOrder[[pc]]
      )
    }
  }

  # Control charts: one per metabolite.
  if (!is.null(master_list$control_charts) &&
      length(master_list$control_charts)) {
    plate_boundaries <- tryCatch(get_plate_boundaries(master_list),
                                 error = function(e) NULL)
    area_imp <- tryCatch(dplyr::bind_rows(master_list$data$peakArea$imputed),
                         error = function(e) NULL)
    conc_imp <- tryCatch(dplyr::bind_rows(master_list$data$concentration$imputed),
                         error = function(e) NULL)
    conc_st  <- tryCatch(
      dplyr::bind_rows(master_list$data$concentration$statTargetProcessed),
      error = function(e) NULL)
    for (met in names(master_list$control_charts)) {
      static <- tryCatch(
        plot_control_chart_static(master_list, met, plate_boundaries,
                                  area_imp, conc_imp, conc_st),
        error = function(e) NULL
      )
      plots[[paste0("controlchart_", fs_safe_name(met))]] <- list(
        static = static,
        interactive = master_list$control_charts[[met]]
      )
    }
  }

  # Shiny-only QC plots (now first-class R-callable).
  plots$rsd_histogram             <- qc_plot_rsd_histogram(master_list, fail_thr)
  plots$missing_values            <- qc_plot_missing_values(master_list)
  plots$sample_type_distribution  <- qc_plot_sample_type_distribution(master_list)
  plots$plate_distribution        <- qc_plot_plate_distribution(master_list)

  plots
}

# Internal: sanitise metabolite names for use as filenames. Lipid IDs
# routinely contain ':' and '/' (e.g. "PC 16:0/18:1") which are invalid
# on Windows and confusing on POSIX.
fs_safe_name <- function(x) {
  out <- gsub("[\\/:*?\"<>|]+", "_", x, perl = TRUE)
  out <- gsub("\\s+", "_", out)
  out <- gsub("_+", "_", out)
  sub("_$", "", sub("^_", "", out))
}
