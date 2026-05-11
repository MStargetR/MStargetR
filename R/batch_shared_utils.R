# batch_shared_utils.R
# Shared batch-correction utilities used by both batchCorrectR and qcCheckR.
# The `bc_` prefixed functions are the canonical implementations.

# RSD denominator epsilon: a QC mean with absolute value below this threshold
# is treated as zero for the purpose of percent-RSD (sd / |mean| * 100).
# Single source of truth so qcCheckR and batchCorrectR stay in parity.
.RSD_ZERO_EPS <- .Machine$double.eps * 100

# Sample Name Normalisation ----
#' Strip .mzML suffix from sample_name
#'
#' qcCheckR strips the \code{.mzML} extension from \code{FileName} when it
#' builds \code{sample_name} (see \code{extract_run_order} in
#' \code{R/qcCheckR_dataprep.R}). The standalone batchCorrectR pipeline must
#' apply the same normalisation so that output \code{sample_name} values
#' align regardless of which pipeline was used.
#'
#' @keywords internal
#' @param x Character vector of sample names.
#' @return Character vector with a trailing \code{.mzML} removed.
strip_mzml_suffix <- function(x) {
  if (!is.character(x)) return(x)
  sub("\\.(mzML|mzXML|mzml\\.gz|mzML\\.gz)$", "", x, ignore.case = TRUE)
}

# Sample Timestamp Parsing ----
#' Parse sample_timestamp to POSIXct
#'
#' Canonical parser for the \code{sample_timestamp} column. Used by both the
#' standalone \code{batchCorrectR} pipeline and \code{qcCheckR::extract_run_order}
#' so both paths produce aligned POSIXct output.
#'
#' POSIXct input is returned unchanged. Character/factor input is parsed
#' against a tryFormats list covering ISO 8601, slash/dash day-first and
#' month-first formats, AM/PM variants, long month names, and date-only
#' forms. If every value fails to parse, the original vector is returned
#' with a warning so callers can decide how to handle it.
#'
#' Slash-format inputs like \code{"10/04/2021 12:08:03"} are inherently
#' locale-dependent: the same mzML files exported by Skyline on different
#' machines can yield either DMY or MDY strings. When the caller already
#' knows the format (e.g. it came from a cohort-level detection in
#' \code{qcCheckR_sort_data}), it can pass \code{date_order} to suppress the
#' wrong family of slash/dash formats so the parser cannot silently pick the
#' wrong interpretation. ISO 8601 formats are always tried first and are
#' format-unambiguous regardless of \code{date_order}.
#'
#' @keywords internal
#' @param x Character, factor, or POSIXct vector of timestamps.
#' @param date_order One of \code{"auto"} (default; current heuristic - tries
#'   DMY before MDY), \code{"dmy"} (only day-first slash/dash formats),
#'   \code{"mdy"} (only month-first slash/dash formats), or \code{"ymd"} /
#'   \code{"iso"} (only ISO 8601 / year-first formats). The qcCheckR pipeline
#'   sets this from a cohort-level decision; standalone batchCorrectR keeps
#'   the default.
#' @return A POSIXct vector the same length as \code{x} (or \code{x} itself
#'   when every value fails to parse).
parse_sample_timestamp <- function(x, date_order = c("auto", "dmy", "mdy", "ymd", "iso")) {
  if (inherits(x, "POSIXct")) return(x)
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) return(x)

  if (missing(date_order)) {
    date_order <- "auto"
  } else {
    date_order <- match.arg(date_order)
  }
  if (date_order == "iso") date_order <- "ymd"

  # ISO 8601 / year-first time-bearing formats are unambiguous and always
  # tried first. Date-only formats (last group) match more permissively, so
  # they must come *after* all slash time formats; otherwise a format like
  # "%Y/%m/%d" greedily mis-parses "10/04/2021 12:08:03" (year=0010,
  # day=20) before the correct DMY/MDY parser ever runs.
  iso_full <- c(
    "%Y-%m-%dT%H:%M:%SZ",           # 2021-03-13T18:12:31Z
    "%Y-%m-%dT%H:%M:%S",            # 2021-03-13T18:12:31
    "%Y-%m-%dT%H:%M:%S%z",          # 2021-03-13T18:12:31+0800
    "%Y-%m-%d %H:%M:%S",            # 2021-03-13 18:12:31
    "%Y/%m/%d %H:%M:%S",            # 2021/03/13 18:12:31
    "%Y-%m-%d %I:%M %p"             # 2021-03-13 6:12 PM
  )
  dmy_full <- c(
    "%d/%m/%Y %H:%M",               # 9/02/2022 16:00
    "%d/%m/%Y %H:%M:%S",            # 13/03/2021 18:12:31
    "%d-%m-%Y %H:%M:%S",            # 13-03-2021 18:12:31
    "%d/%m/%Y %I:%M %p"             # 13/03/2021 6:12 PM
  )
  mdy_full <- c(
    "%m/%d/%Y %H:%M:%S",            # 09/27/2024 10:41:28
    "%m/%d/%Y %H:%M",               # 9/27/2024 16:00
    "%m-%d-%Y %H:%M:%S",            # 09-27-2024 10:41:28
    "%m/%d/%Y %I:%M %p"             # 09/27/2024 6:12 PM
  )
  long_full <- c(
    "%B %d, %Y %H:%M",              # March 13, 2021 18:12
    "%b %d, %Y %I:%M %p"            # Mar 13, 2021 6:12 PM
  )
  iso_date <- c(
    "%Y-%m-%d",                     # 2021-03-13
    "%Y/%m/%d"                      # 2024/12/13
  )
  dmy_date <- "%d/%m/%Y"            # 13/03/2021
  mdy_date <- "%m/%d/%Y"            # 09/27/2024
  long_date <- "%B %d, %Y"          # March 13, 2021

  # In "auto" mode, mirror the historical tryFormats order so existing
  # callers see no behavioural change. Explicit "dmy"/"mdy" excludes the
  # conflicting slash family entirely so the parser cannot silently pick
  # the wrong interpretation.
  tryFormats <- switch(date_order,
    "auto" = c(iso_full, dmy_full[1], mdy_full[1], dmy_full[2], dmy_full[3],
               dmy_full[4], long_full, iso_date, dmy_date, long_date),
    "dmy"  = c(iso_full, dmy_full, long_full, iso_date, dmy_date, long_date),
    "mdy"  = c(iso_full, mdy_full, long_full, iso_date, mdy_date, long_date),
    "ymd"  = c(iso_full, iso_date)
  )

  # as.POSIXct(tryFormats = ...) applies one format to the whole vector, so
  # mixed formats within a single column will error out. Iterate per format
  # instead, filling values element-by-element from the first match.
  parsed <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  remaining <- !is.na(x) & nzchar(x)
  for (fmt in tryFormats) {
    if (!any(remaining)) break
    attempt <- suppressWarnings(as.POSIXct(x[remaining], format = fmt))
    hits <- !is.na(attempt)
    if (any(hits)) {
      idx <- which(remaining)[hits]
      parsed[idx] <- attempt[hits]
      remaining[idx] <- FALSE
    }
  }

  nonempty <- !is.na(x) & nzchar(x)
  if (any(nonempty) && all(is.na(parsed[nonempty]))) {
    warning("parse_sample_timestamp: could not parse any sample_timestamp ",
            "values; leaving column unchanged.", call. = FALSE)
    return(x)
  }
  parsed
}

# QC Distribution Assessment & Boundary Synthesis ----
#' Assess QC Distribution Within Batches
#'
#' Evaluates QC coverage within each batch without moving any samples.
#' Reports distribution quality and identifies batches needing synthetic boundary QCs.
#' @keywords internal
#' @param pheno Data.frame with at least columns \code{batch} and \code{class}.
#' @return A list with \code{assessment} (per-batch summary), \code{needs_leading}
#'   and \code{needs_trailing} (character vectors of batch IDs).
bc_assess_qc_distribution <- function(pheno) {
  assessment <- list()
  needs_leading <- character(0)
  needs_trailing <- character(0)

  for (b in unique(pheno$batch)) {
    bd <- pheno %>% dplyr::filter(batch == !!b)
    n_total <- nrow(bd)
    qc_idx <- which(bd$class == "qc")
    n_qc <- length(qc_idx)

    if (n_qc == 0) {
      message("  WARNING: Batch '", b, "' has ZERO QC samples. ",
              "Batch correction cannot anchor to QC drift in this batch.")
      assessment[[as.character(b)]] <- list(
        n_total = n_total, n_qc = 0,
        has_leading = FALSE, has_trailing = FALSE,
        max_gap = n_total, even_distribution = FALSE
      )
      needs_leading <- c(needs_leading, as.character(b))
      needs_trailing <- c(needs_trailing, as.character(b))
      next
    }

    has_leading <- qc_idx[1] == 1
    has_trailing <- qc_idx[n_qc] == n_total

    boundaries <- c(0, qc_idx, n_total + 1)
    gaps <- diff(boundaries) - 1
    max_gap <- max(gaps)
    ideal_gap <- n_total / (n_qc + 1)
    if (length(gaps) > 1 && mean(gaps) > 0) {
      gap_cv <- stats::sd(gaps) / mean(gaps)
    } else {
      gap_cv <- 0
      if (length(gaps) > 1) {
        message("  Note: Batch '", b, "' has all-zero inter-QC gaps ",
                "(QCs are adjacent); gap CV reported as 0 but distribution ",
                "may be pathological.")
      }
    }
    even_distribution <- gap_cv < 0.5

    if (!even_distribution) {
      message("  Note: Batch '", b, "' QC distribution is uneven ",
              "(", n_qc, " QCs across ", n_total, " samples, ",
              "max gap: ", max_gap, " samples, ",
              "ideal gap: ", round(ideal_gap, 1), " samples). ",
              "Consider spacing QCs more evenly in future runs.")
    } else {
      message("  Batch '", b, "': ", n_qc, " QCs across ", n_total,
              " samples (max gap: ", max_gap, ", distribution: OK)")
    }

    # statTarget::REGfit (called by shiftCor under QCRFSC) enforces both
    # "first sample must be QC" and "last sample must be QC" via stop(); a
    # batch lacking either boundary QC will halt the pipeline. Inject a
    # synthetic whenever the boundary is missing, regardless of how good
    # the interior QC spacing is. Distortion of post-correction QC means
    # caused by the synthetic is handled separately in adjust_qc_means(),
    # which excludes synthetic rows from the corrected_means calculation.
    if (!has_leading) {
      message("  Note: Batch '", b, "' has no QC at run start ",
              "(first QC at position ", qc_idx[1], " of ", n_total,
              "). statTarget::REGfit requires a QC at position 1; ",
              "a synthetic boundary QC will be generated.")
      needs_leading <- c(needs_leading, as.character(b))
    }

    if (!has_trailing) {
      message("  Note: Batch '", b, "' has no QC at run end ",
              "(last QC at position ", qc_idx[n_qc], " of ", n_total,
              "). statTarget::REGfit requires a QC at the final position; ",
              "a synthetic boundary QC will be generated.")
      needs_trailing <- c(needs_trailing, as.character(b))
    }

    assessment[[as.character(b)]] <- list(
      n_total = n_total, n_qc = n_qc,
      has_leading = has_leading, has_trailing = has_trailing,
      max_gap = max_gap, gap_cv = round(gap_cv, 3),
      even_distribution = even_distribution,
      qc_positions = qc_idx
    )
  }

  list(assessment = assessment,
       needs_leading = needs_leading,
       needs_trailing = needs_trailing)
}

#' Estimate Synthetic Boundary QC Values
#'
#' For batches missing a QC at the leading or trailing boundary, estimates
#' QC signal by linear back-extrapolation from the nearest 2-3 real QCs,
#' shrunk toward the batch QC median to prevent wild extrapolations.
#' @keywords internal
#' @param qc_values Numeric vector of QC values for one metabolite.
#' @param positions Integer vector of QC positions in run order.
#' @param target_pos The position to extrapolate to.
#' @param shrinkage Shrinkage factor toward median (default 0.5).
#' @return A single numeric estimated value.
#' @importFrom utils tail
bc_estimate_boundary_qc <- function(qc_values, positions, target_pos,
                                     shrinkage = 0.5) {
  valid <- !is.na(qc_values)
  qc_values <- qc_values[valid]
  positions <- positions[valid]

  if (length(qc_values) == 0) return(NA_real_)
  if (length(qc_values) == 1) return(qc_values[1])

  global_median <- stats::median(qc_values, na.rm = TRUE)

  # Use nearest 3 QCs max for extrapolation. Sort positions ascending
  # explicitly so head() always selects the earliest and tail() always
  # selects the latest, regardless of the order in which QCs were supplied.
  sorted_idx <- order(positions)
  if (target_pos < min(positions)) {
    use <- head(sorted_idx, min(3, length(positions)))
  } else {
    use <- tail(sorted_idx, min(3, length(positions)))
  }

  # Build a plain-named frame so predict() can match the formula's terms.
  # Using `qc_values[use] ~ positions[use]` as a formula produces term labels
  # containing `[`, which model.frame() cannot look up in newdata — the result
  # is a silent fallback to fitted values (no extrapolation).
  fit_df <- data.frame(pos = positions[use], val = qc_values[use])
  fit <- stats::lm(val ~ pos, data = fit_df)
  extrapolated <- as.numeric(stats::predict(
    fit, newdata = data.frame(pos = target_pos)
  ))

  result <- shrinkage * global_median + (1 - shrinkage) * extrapolated
  result <- max(result, 0)
  # Ceiling at 2× the batch QC max to prevent wild extrapolation.
  # For metabolites with a very small real QC maximum this may still be a
  # generous upper bound; a quantile-based clamp could be considered if
  # over-extrapolation is observed in practice.
  result <- min(result, max(qc_values) * 2)

  result
}

#' Prepare Pheno File With Synthetic Boundary QCs
#'
#' Replaces \code{bc_reorder_qc_within_batches}. Does NOT move any existing
#' samples. Instead, assesses QC distribution and inserts synthetic QC rows
#' at batch boundaries where real QCs are missing.
#' @keywords internal
#' @param pheno Data.frame with \code{batch}, \code{class}, \code{order} columns.
#' @return A list with \code{pheno} (data with synthetic rows, flagged with
#'   \code{synthetic_qc = TRUE}) and \code{qc_assessment}.
bc_prepare_qc_boundaries <- function(pheno) {
  qc_info <- bc_assess_qc_distribution(pheno)

  if (length(qc_info$needs_leading) == 0 &&
      length(qc_info$needs_trailing) == 0) {
    pheno$synthetic_qc <- FALSE
    return(list(pheno = pheno, qc_assessment = qc_info))
  }

  pheno$synthetic_qc <- FALSE
  result <- NULL

  for (b in unique(pheno$batch)) {
    bd <- pheno %>% dplyr::filter(batch == !!b)
    b_char <- as.character(b)
    qc_idx <- which(bd$class == "qc")

    if (b_char %in% qc_info$needs_leading && length(qc_idx) > 0) {
      synthetic_row <- bd[qc_idx[1], ]
      synthetic_row$sample_name <- paste0("SYNTHETIC_QC_leading_", b)
      synthetic_row$class <- "qc"
      synthetic_row$synthetic_qc <- TRUE
      synthetic_row$order <- min(bd$order) - 0.5

      message("  SYNTHETIC QC inserted at start of batch '", b, "' ",
              "(extrapolated from ", length(qc_idx), " real QC(s))")
      bd <- dplyr::bind_rows(synthetic_row, bd)
    }

    if (b_char %in% qc_info$needs_trailing && length(qc_idx) > 0) {
      synthetic_row <- bd[qc_idx[length(qc_idx)], ]
      synthetic_row$sample_name <- paste0("SYNTHETIC_QC_trailing_", b)
      synthetic_row$class <- "qc"
      synthetic_row$synthetic_qc <- TRUE
      synthetic_row$order <- max(bd$order) + 0.5

      message("  SYNTHETIC QC inserted at end of batch '", b, "' ",
              "(extrapolated from ", length(qc_idx), " real QC(s))")
      bd <- dplyr::bind_rows(bd, synthetic_row)
    }

    result <- dplyr::bind_rows(result, bd)
  }

  result <- result %>% dplyr::arrange(order)
  list(pheno = result, qc_assessment = qc_info)
}

#' Populate Synthetic QC Rows With Extrapolated Metabolite Values
#'
#' Given a tibble that joins a pheno (containing synthetic boundary QC
#' rows flagged with \code{synthetic_qc = TRUE}) to a metabolite profile,
#' fills each synthetic row's metabolite values by linear extrapolation
#' from the real QCs in the same batch via \code{bc_estimate_boundary_qc}.
#'
#' Without this, synthetic QC rows keep the NA values produced by the
#' \code{left_join} from the original data -- which biases QCRFSC and, at
#' high \code{Frule}, triggers statTarget to drop every feature (producing
#' "subscript out of bounds" from downstream indexing into an empty matrix).
#'
#' @keywords internal
#' @param ordered Data.frame with columns \code{sample}, \code{batch},
#'   \code{class}, \code{synthetic_qc}, \code{order}, and one column per
#'   metabolite in \code{metabolite_cols}.
#' @param metabolite_cols Character vector of metabolite column names.
#' @return The same tibble with synthetic rows filled in where possible.
bc_populate_synthetic_qc_values <- function(ordered, metabolite_cols) {
  if (!"synthetic_qc" %in% colnames(ordered)) return(ordered)
  ordered <- dplyr::ungroup(ordered)
  synth_idx <- which(as_logical_na_false(ordered$synthetic_qc))
  if (length(synth_idx) == 0) return(ordered)

  # Hoist class_lc and is_qc outside the loop: they depend only on `ordered`
  # which does not change between iterations, so recomputing per-i is wasted work.
  class_lc <- tolower(ordered$class)
  # The standalone pipeline always sets class = "qc" (string) on the pheno
  # returned by bc_prepare_pheno_file; the is.na() branch is dead code in that
  # path but harmless — it would only activate if a statTarget PhenoFile frame
  # (which encodes QC as class = NA) were passed directly. Both branches are
  # kept so the function remains correct under either caller.
  is_qc <- is.na(class_lc) | class_lc == "qc"

  for (i in synth_idx) {
    b <- ordered$batch[i]
    target <- ordered$order[i]
    real_mask <- ordered$batch == b &
      !as_logical_na_false(ordered$synthetic_qc) &
      is_qc
    if (!any(real_mask, na.rm = TRUE)) next
    real_positions <- ordered$order[real_mask]

    # Compute all metabolite estimates into a single vector, then perform
    # one row-level assignment instead of |metabolite_cols| separate
    # copy-on-modify column writes. On wide tibbles this turns an O(N*M)
    # full-column-copy pattern into O(N) single-row writes.
    est_vec <- vapply(metabolite_cols, function(met) {
      bc_estimate_boundary_qc(ordered[[met]][real_mask], real_positions, target)
    }, numeric(1))
    ordered[i, metabolite_cols] <- as.list(est_vec)
  }
  ordered
}

# NA-safe logical coercion: converts a vector to logical and replaces NA
# with FALSE. Used by bc_populate_synthetic_qc_values. A pheno column that
# has never had a synthetic row can arrive as all-FALSE or all-NA depending
# on upstream construction; treating NA as FALSE is the correct default.
as_logical_na_false <- function(x) {
  if (is.null(x)) return(logical(0))
  out <- as.logical(x)
  out[is.na(out)] <- FALSE
  out
}

#' Reorder QC Within Batches (Deprecated)
#'
#' Deprecated. Use \code{bc_prepare_qc_boundaries} instead.
#' @keywords internal
#' @param pheno Data.frame with batch, class, order columns.
#' @return A modified pheno data.frame.
bc_reorder_qc_within_batches <- function(pheno) {
  warning("bc_reorder_qc_within_batches() is deprecated. ",
          "Use bc_prepare_qc_boundaries() instead.", call. = FALSE)
  result <- bc_prepare_qc_boundaries(pheno)
  result$pheno
}

# statTarget Format Detection ----
#' Detect statTarget Output Format and Filter/Rename
#'
#' The raw output from statTarget can appear in several column layouts.
#' This helper detects the format, filters out header rows, and ensures
#' the first column is called \code{name} with the remaining columns
#' converted to numeric.
#'
#' @keywords internal
#' @param data A tibble or data.frame of raw statTarget output.
#' @return A cleaned tibble with a \code{name} column and numeric value columns.
bc_detect_stattarget_format <- function(data) {
  if ("sample1" %in% colnames(data)) {
    cleaned <- data %>%
      dplyr::filter(sample != "class") %>%
      dplyr::rename(name = sample) %>%
      dplyr::mutate(dplyr::across(-name, as.numeric))
  } else if (local({
    non_sample <- colnames(data)[colnames(data) != "sample"]
    length(non_sample) > 0 && all(grepl("^M\\d+$", non_sample))
  })) {
    cleaned <- data %>% t() %>% data.frame() %>%
      tibble::rownames_to_column() %>% stats::setNames(.[1, ]) %>%
      dplyr::filter(sample != "class" & sample != "sample") %>%
      dplyr::rename(name = sample) %>%
      dplyr::mutate(dplyr::across(-name, as.numeric))
  } else {
    cleaned <- data %>% dplyr::rename(name = 1) %>%
      dplyr::filter(!name %in% c("class", "sample")) %>%
      dplyr::mutate(dplyr::across(-name, as.numeric))
  }
  cleaned
}

# Mean Adjustment Ratios ----
#' Compute Per-Metabolite Mean-Adjustment Ratios
#'
#' Computes the per-metabolite ratio of corrected QC mean to original QC mean.
#' Both inputs are \strong{named numeric vectors indexed by metabolite} (one
#' value per metabolite, obtained via \code{colMeans()} over QC rows); there
#' is no batch dimension here. The returned ratios are used by
#' \code{bc_apply_mean_ratios} to rescale the corrected data column-wise so
#' that post-correction QC means match the pre-correction scale.
#'
#' The previous implementation clamped ratios outside \eqn{[10^{-2},\,10^{2}]}
#' on the assumption that such magnitudes always indicate a near-zero mean.
#' That assumption is wrong for the QCRFSC pipeline: \code{statTarget::REGfit}
#' normalises every value by dividing by the random-forest fit (\code{x[i,] /
#' rfP}), so post-correction QC values are anchored at ~1 by construction
#' regardless of the metabolite's concentration scale. The corrected/original
#' ratio is therefore approximately \eqn{1 / \mathrm{concentration}}, which
#' for a typical lipid panel routinely spans 0.001 to 1000. The earlier clamp
#' silently substituted a wrong rescaling factor (\eqn{10^{-2}} or
#' \eqn{10^{2}}) and mis-scaled the corrected concentrations for any feature
#' whose mean concentration was further than 100x from 1 -- which on real
#' ANPC plates was 90%+ of features. Only NA / non-finite / non-positive
#' ratios are now sanitised; legitimate large-magnitude rescaling is allowed
#' to pass through.
#'
#' @keywords internal
#' @param orig_means Named numeric vector of original QC means. Names must be
#'   metabolite identifiers.
#' @param corr_means Named numeric vector of corrected QC means. Names must
#'   match \code{orig_means} (names absent from either are dropped).
#' @return Named numeric vector of correction ratios
#'   (\code{corr_means / orig_means}), with names limited to metabolites
#'   present in both inputs. NA / non-finite / non-positive ratios are
#'   reported (warning) and left as-is; \code{bc_apply_mean_ratios} then
#'   skips those metabolites.
bc_compute_mean_ratios <- function(orig_means, corr_means) {
  common_mets <- intersect(names(orig_means), names(corr_means))
  # correction_factors: divide corrected values by this to restore original mean scale
  ratios <- stats::setNames(corr_means[common_mets] / orig_means[common_mets],
                             common_mets)

  # Flag truly degenerate ratios so the caller can skip those metabolites.
  # "Degenerate" here means NA, non-finite, or <= 0 -- the only cases where
  # dividing by the ratio produces nonsense. A ratio of, say, 1e-5 is NOT
  # degenerate in the QCRFSC pipeline; it's the legitimate inverse of a
  # high-concentration metabolite (~1e5 units) and undoing the QCRFSC
  # normalisation requires multiplying by 1e5.
  bad <- !is.finite(ratios) | ratios <= 0
  if (any(bad)) {
    bad_names <- names(ratios)[bad]
    warning(
      "bc_compute_mean_ratios: ", sum(bad),
      " metabolite(s) have NA, non-finite, or non-positive ",
      "corrected/original QC-mean ratio (will be left uncorrected). ",
      "This usually indicates a zero or all-NA QC mean. Offending ",
      "metabolite(s) (first 5): ",
      paste(utils::head(bad_names, 5), collapse = ", "),
      call. = FALSE
    )
  }

  ratios
}

#' Apply Per-Metabolite Mean-Adjustment Ratios
#'
#' Divides each metabolite column in \code{data} by the corresponding ratio,
#' skipping metabolites whose ratio is \code{NA}, zero, or non-finite.
#' \code{ratios} is a \strong{named numeric vector indexed by metabolite}
#' (one value per metabolite column in \code{data}); there is no batch
#' dimension. Names of \code{ratios} that do not appear as columns of
#' \code{data} are silently skipped.
#'
#' @keywords internal
#' @param data Data.frame or tibble to adjust. Metabolite columns are
#'   identified by name-matching against \code{names(ratios)}.
#' @param ratios Named numeric vector of correction ratios, as returned by
#'   \code{bc_compute_mean_ratios}.
#' @return The adjusted data.frame.
bc_apply_mean_ratios <- function(data, ratios) {
  valid <- names(ratios)[!is.na(ratios) & ratios != 0 & is.finite(ratios)]
  dropped <- setdiff(names(ratios), valid)
  if (length(dropped) > 0) {
    message("bc_apply_mean_ratios: ", length(dropped),
            " metabolite(s) left uncorrected (zero, NA, or non-finite ratio): ",
            paste(utils::head(dropped, 5), collapse = ", "))
  }
  valid <- intersect(valid, colnames(data))
  # Clamp negative ratios: a negative corrected/original mean ratio flips the
  # sign of every corrected value for that metabolite, which is physically
  # meaningless for intensity data. Clamp to a small positive floor and warn.
  neg_valid <- valid[ratios[valid] < 0]
  if (length(neg_valid) > 0) {
    warning("bc_apply_mean_ratios: ", length(neg_valid),
            " metabolite(s) have a negative correction ratio (negative ",
            "post-correction QC mean). Clamping ratio to 1e-6 to preserve ",
            "sign. Affected metabolite(s) (first 5): ",
            paste(utils::head(neg_valid, 5), collapse = ", "),
            call. = FALSE)
    ratios[neg_valid] <- 1e-6
  }
  data[valid] <- Map(`/`, data[valid], ratios[valid])
  data
}

# Core ComBat Correction ----

#' Prepare Feature Matrix for ComBat
#'
#' Transposes the metabolite columns into a features-by-samples matrix,
#' imputes NAs with row medians, and removes zero-variance features.
#'
#' @keywords internal
#' @param data Data.frame with metabolite value columns.
#' @param metabolite_cols Character vector of metabolite column names.
#' @return A list with elements \code{dat_combat} (the cleaned matrix),
#'   \code{zero_var} (logical vector indicating which features had zero
#'   variance), and \code{kept_features} (character vector of retained
#'   feature names).
bc_prepare_combat_matrix <- function(data, metabolite_cols) {
  dat_matrix <- t(as.matrix(data[, metabolite_cols, drop = FALSE]))

  rownames(dat_matrix) <- metabolite_cols

  # Handle any NA values -- ComBat does not tolerate NAs
  has_na <- any(is.na(dat_matrix))
  if (has_na) {
    message("    Imputing missing values with feature medians (rows in features-by-samples matrix) before ComBat...")
    all_na_rows <- apply(dat_matrix, 1, function(r) all(is.na(r)))
    if (any(all_na_rows)) {
      message("    Dropping ", sum(all_na_rows),
              " all-NA feature(s) before ComBat to avoid zero-fill bias.")
      dat_matrix <- dat_matrix[!all_na_rows, , drop = FALSE]
    }
    row_med <- apply(dat_matrix, 1, function(r) stats::median(r, na.rm = TRUE))
    na_mask <- is.na(dat_matrix)
    dat_matrix[na_mask] <- row_med[row(dat_matrix)[na_mask]]
  }

  # Remove zero-variance features (ComBat will fail on these).
  # Use a floating-point-safe threshold rather than == 0: features with
  # variance below .Machine$double.eps are numerically indistinguishable from
  # constant and will make ComBat numerically unstable.
  row_vars <- apply(dat_matrix, 1, stats::var, na.rm = TRUE)
  zero_var <- is.na(row_vars) | row_vars < .Machine$double.eps
  if (any(zero_var)) {
    message("    Removing ", sum(zero_var), " zero-variance feature(s) before ComBat.")
  }

  remaining_features <- rownames(dat_matrix)[!zero_var]
  list(
    dat_combat = dat_matrix[!zero_var, , drop = FALSE],
    zero_var = zero_var,
    kept_features = remaining_features
  )
}

#' Reconstruct Data.frame After ComBat Correction
#'
#' Takes the corrected matrix from \code{sva::ComBat} and writes the
#' corrected values back into the original data.frame.
#'
#' @keywords internal
#' @param data The original data.frame (used as a template).
#' @param corrected_matrix The features-by-samples corrected matrix from ComBat.
#' @param kept_features Character vector of feature names that were corrected.
#' @return The data.frame with corrected metabolite values.
bc_reconstruct_combat_output <- function(data, corrected_matrix, kept_features) {
  corrected_df <- data
  corrected_values <- t(corrected_matrix)
  colnames(corrected_values) <- kept_features
  corrected_df[kept_features] <- as.data.frame(
    corrected_values[, kept_features, drop = FALSE]
  )
  # Zero-variance features remain unchanged (already in corrected_df from data)
  corrected_df
}

#' Run ComBat Batch Correction
#'
#' Applies \code{sva::ComBat} to a data.frame that has a \code{batch} column
#' and numeric metabolite columns.
#'
#' Handles NA imputation (column-median fill), zero-variance feature removal,
#' and reconstruction of the corrected data.frame.
#'
#' @keywords internal
#' @param data Data.frame with a \code{batch} column and metabolite value columns.
#' @param metabolite_cols Character vector of metabolite column names.
#' @param par.prior Logical. Parametric (TRUE) or non-parametric (FALSE) priors.
#' @param mean.only Logical. If TRUE, only correct batch mean effect.
#' @param ref.batch Optional reference batch.
#' @return Data.frame in the same format as input with corrected metabolite values.
bc_run_combat <- function(data, metabolite_cols, par.prior = TRUE,
                          mean.only = FALSE, ref.batch = NULL) {
  if (!requireNamespace("sva", quietly = TRUE)) {
    stop("The 'sva' package is required for ComBat correction. ",
         "Install it with: BiocManager::install('sva')", call. = FALSE)
  }

  if (!is.null(ref.batch)) {
    available_batches <- unique(as.character(data$batch))
    if (!as.character(ref.batch) %in% available_batches) {
      stop("bc_run_combat: 'ref.batch' = '", ref.batch,
           "' is not present in the data's batch column. ",
           "Available batches: ",
           paste(shQuote(available_batches), collapse = ", "),
           ". Use NULL to adjust against the grand mean instead.",
           call. = FALSE)
    }
  }

  prep <- bc_prepare_combat_matrix(data, metabolite_cols)

  corrected_matrix <- sva::ComBat(
    dat = prep$dat_combat,
    batch = data$batch,
    mod = NULL,
    par.prior = par.prior,
    prior.plots = FALSE,
    mean.only = mean.only,
    ref.batch = ref.batch
  )

  message("    sva::ComBat completed successfully.")

  bc_reconstruct_combat_output(data, corrected_matrix, prep$kept_features)
}
