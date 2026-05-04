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
#' @return A `ggplot` object representing the PCA scores.
generate_pca_ggplot <- function(master_list, fill_var) {
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

  plot <- plotly::ggplotly(
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
  )
  return(plot)
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
#' @return A `ggplot` object representing the run order plot.
plot_run_order <- function(scores,
                           pc,
                           boundaries,
                           plot_settings) {
  plotly::ggplotly(
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
      ggplot2::theme(text = ggplot2::element_text(size = 12)),
    tooltip = "text"
  )
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
#' @return A `ggplot` object representing the control chart for the specified metabolite.
plot_control_chart <- function(master_list,
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


  plotly::ggplotly(
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
      ),
    tooltip = "text"
  )
}
