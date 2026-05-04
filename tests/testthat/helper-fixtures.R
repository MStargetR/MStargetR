# Shared test fixtures for MStargetR test suite
# This file is automatically sourced by testthat before tests run.

#' Create test data for batchCorrectR tests
#'
#' Default schema is intentionally narrow (sample_name / batch / sample_type /
#' run_order + 2 metabolites) to match legacy tests. Pass
#' \code{include_extras = TRUE} to add the post-normalisation metadata columns
#' introduced by commits 3b7207c / 41a1724 (sample_timestamp, sample_class,
#' sample_plate_id, sample_plate_order, sample_matrix, sample_data_source,
#' synthetic_qc) so tests can exercise the canonical-metadata code paths.
#' @param n_samples Total number of samples
#' @param n_batches Number of batches
#' @param n_qc_per_batch Number of QC samples per batch
#' @param include_extras Logical. When TRUE, add canonical metadata columns.
#' @return A data.frame in batchCorrectR format
make_bc_data <- function(n_samples = 20, n_batches = 2, n_qc_per_batch = 4,
                         include_extras = FALSE) {
  withr::with_seed(42, {
  batches <- rep(paste0("plate", seq_len(n_batches)), each = n_samples / n_batches)
  n_per_batch <- n_samples / n_batches
  sample_types <- character(n_samples)
  for (b in seq_len(n_batches)) {
    idx <- ((b - 1) * n_per_batch + 1):(b * n_per_batch)
    qc_positions <- seq(1, n_per_batch, length.out = n_qc_per_batch)
    qc_positions <- round(qc_positions)
    sample_types[idx] <- "sample"
    sample_types[idx[qc_positions]] <- "qc"
  }
  df <- data.frame(
    sample_name = paste0("S", seq_len(n_samples)),
    batch       = batches,
    sample_type = sample_types,
    run_order   = seq_len(n_samples),
    metab_A     = rnorm(n_samples, 100, 10),
    metab_B     = rnorm(n_samples, 500, 50),
    stringsAsFactors = FALSE
  )
  if (include_extras) {
    # Interleave QC/sample timestamps at 1-minute intervals so run_order and
    # sample_timestamp encode the same ordering. ISO-8601 string form: tests
    # should round-trip this through parse_sample_timestamp() to POSIXct.
    base_ts <- as.POSIXct("2026-01-15 09:00:00", tz = "UTC")
    df$sample_timestamp <- format(
      base_ts + 60 * (df$run_order - 1L),
      "%Y-%m-%dT%H:%M:%SZ"
    )
    df$sample_class         <- ifelse(df$sample_type == "qc", "qc", "sample")
    df$sample_plate_id      <- df$batch
    df$sample_plate_order   <- ave(df$run_order, df$batch,
                                   FUN = function(x) seq_along(x))
    df$sample_matrix        <- "serum"
    df$sample_data_source   <- "concentration"
    df$synthetic_qc         <- FALSE
  }
  df
  }) # end withr::with_seed
}
