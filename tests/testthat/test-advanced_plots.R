# End-to-end tests for the advanced_plots = TRUE wiring across
# qcCheckR, batchCorrectR, and the new resultsExplorerR API.

# --- Helpers -----------------------------------------------------------------

make_bc_result_for_explorer <- function() {
  df <- make_bc_data(n_samples = 24, n_batches = 2, n_qc_per_batch = 4)
  bc <- batchCorrectR(df, qc_label = "qc", method = "QCRFSC",
                      plot = FALSE, report = FALSE,
                      project_dir = NULL)
  list(df = df, bc = bc)
}

# --- batchCorrectR(advanced_plots) ------------------------------------------

test_that("batchCorrectR(advanced_plots = TRUE) writes PDFs and HTMLs", {
  skip_on_cran()
  skip_if_not_installed("statTarget")
  skip_if_not_installed("htmlwidgets")
  tmp <- withr::local_tempdir()

  df <- make_bc_data(n_samples = 24, n_batches = 2, n_qc_per_batch = 4)
  res <- batchCorrectR(df, qc_label = "qc", method = "QCRFSC",
                       project_dir = tmp,
                       advanced_plots = TRUE,
                       report = FALSE)

  fig_dir <- file.path(tmp, "all", "figures", "batch_corrector")
  expect_true(dir.exists(fig_dir))
  pdfs <- list.files(fig_dir, pattern = "\\.pdf$")
  htmls <- list.files(fig_dir, pattern = "\\.html$")
  # At minimum: rsd_comparison, rsd_by_class. Drift/PCA only when those
  # collectors succeed -- so we assert >= 2 of each rather than a fixed
  # list to keep the test robust.
  expect_gte(length(pdfs), 2L)
  expect_gte(length(htmls), 2L)
  expect_true("rsd_by_class.pdf" %in% pdfs)
  expect_true("rsd_by_class.html" %in% htmls)

  # advanced_plots also populates result$plots with the extended set.
  expect_true(!is.null(res$plots$rsd_by_class$static))
  expect_s3_class(res$plots$rsd_by_class$static, "ggplot")
})

test_that("batchCorrectR(advanced_plots = FALSE) writes no figure files", {
  skip_on_cran()
  skip_if_not_installed("statTarget")
  tmp <- withr::local_tempdir()

  df <- make_bc_data(n_samples = 24, n_batches = 2, n_qc_per_batch = 4)
  res <- batchCorrectR(df, qc_label = "qc", method = "QCRFSC",
                       project_dir = tmp,
                       advanced_plots = FALSE,
                       report = FALSE)
  expect_false(dir.exists(file.path(tmp, "all", "figures", "batch_corrector")))
})

test_that("batchCorrectR emits deprecation warning when 'plot' is passed", {
  skip_on_cran()
  skip_if_not_installed("statTarget")
  tmp <- withr::local_tempdir()

  df <- make_bc_data(n_samples = 24, n_batches = 2, n_qc_per_batch = 4)
  expect_warning(
    batchCorrectR(df, qc_label = "qc", method = "QCRFSC",
                  project_dir = tmp, plot = FALSE, report = FALSE),
    "deprecated"
  )
})

# --- resultsExplorerR -------------------------------------------------------

test_that("resultsExplorerR returns plots + summary from a bare data frame", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")

  df <- make_bc_data(n_samples = 24, n_batches = 2, n_qc_per_batch = 4)
  re <- resultsExplorerR(df, qc_label = "qc", advanced_plots = FALSE)
  expect_true(is.list(re$plots))
  expect_named(re$summary, c("metabolite", "class", "rsd", "status"))
  expect_true(nrow(re$summary) >= 2L)
  expect_true("rsd_histogram" %in% names(re$plots))
})

test_that("resultsExplorerR(advanced_plots = TRUE) writes figures", {
  skip_on_cran()
  skip_if_not_installed("statTarget")
  skip_if_not_installed("htmlwidgets")
  tmp <- withr::local_tempdir()

  bundle <- make_bc_result_for_explorer()
  re <- resultsExplorerR(bundle$bc, project_dir = tmp,
                         advanced_plots = TRUE)

  fig_dir <- file.path(tmp, "all", "figures", "results_explorer")
  expect_true(dir.exists(fig_dir))
  files <- list.files(fig_dir)
  expect_gte(sum(grepl("\\.pdf$", files)), 3L)
  expect_gte(sum(grepl("\\.html$", files)), 3L)
  expect_true("rsd_histogram.pdf" %in% files)
})

test_that("resultsExplorerR validates advanced_plots and thresholds", {
  df <- make_bc_data()
  expect_error(resultsExplorerR(df, advanced_plots = "yes"), "TRUE or FALSE")
  expect_error(resultsExplorerR(df, warn_thr = -5), "warn_thr")
  expect_error(resultsExplorerR(df, warn_thr = 30, fail_thr = 20),
               ">= warn_thr")
})

# --- qcCheckR(advanced_plots) wiring ----------------------------------------
# qcCheckR runs a long pipeline; full integration is exercised in the
# existing qcCheckR test suite. Here we just sanity-check that the new
# argument is accepted and the master_list collector function works on
# a hand-built fixture (cheap, no statTarget run).

test_that("qcCheckR_collect_plots returns a named list with the new plots", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")

  # Minimal master_list shape -- only the bits the collector reads.
  df <- make_bc_data(n_samples = 24, n_batches = 2, n_qc_per_batch = 4,
                     include_extras = TRUE)
  df$sample_type_factor <- df$sample_type
  # qcCheckR run-order plots select sample_run_index; the batchCorrectR-style
  # fixture only carries run_order, so mirror it under the qcCheckR name.
  df$sample_run_index <- df$run_order
  ml <- list(
    project_details = list(project_name = "test",
                           plot_shape = c(qc = 23, sample = 21),
                           plot_fill  = c(qc = "red", sample = "blue"),
                           plot_colour = c(qc = "red", sample = "black"),
                           plot_size  = c(qc = 3, sample = 2)),
    data = list(
      peakArea = list(sorted = list(df), imputed = list(df)),
      concentration = list(corrected = list(df), imputed = list(df),
                           statTargetProcessed = list(df))
    ),
    filters = list(rsd = NULL),
    pca = list(plot = list(), scoresRunOrder = list()),
    control_charts = list()
  )

  plots <- MStargetR:::qcCheckR_collect_plots(ml)
  expect_true(is.list(plots))
  # Shiny-only plots that don't depend on filters$rsd should always
  # appear (or be NULL when their inputs are missing). The plate
  # distribution / sample type ones should at least build given the
  # fixture above.
  expect_true("sample_type_distribution" %in% names(plots))
  expect_true("plate_distribution" %in% names(plots))
  if (!is.null(plots$plate_distribution)) {
    expect_s3_class(plots$plate_distribution$static, "ggplot")
  }
})

test_that("qcCheckR rejects non-logical advanced_plots", {
  # Cheaper: exercise the validation gate without running the pipeline.
  expect_error(
    qcCheckR(user_name = "test", project_directory = tempdir(),
             advanced_plots = "TRUE"),
    "advanced_plots"
  )
})
