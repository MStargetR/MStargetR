#' Check MRM Transition List for Unique Q1/Q3 Pairs
#'
#' Validates that all precursor (Q1) and product (Q3) ion mass-to-charge
#' ratio pairs in an MRM transition list are unique. Duplicate Q1/Q3
#' combinations cause ambiguous peak assignments in downstream processing
#' by PeakForgeR and qcCheckR.
#'
#' @param transition_df A data.frame containing MRM transitions. Must include
#'   the columns \code{"Precursor Mz"} (numeric), \code{"Product Mz"}
#'   (numeric), and \code{"Precursor Name"} (character).
#' @param tolerance Numeric. Pairs whose Precursor Mz and Product Mz both
#'   differ by less than this value (in Da) are considered duplicates.
#'   Defaults to \code{0.001}. Using a tolerance-based comparison avoids
#'   false negatives caused by R's banker's rounding at half-unit boundaries
#'   (e.g. 500.0005 rounds down at precision 3 while 500.0015 rounds up).
#' @return If all transitions are unique, returns \code{NULL} invisibly and
#'   prints a success message. If duplicates are found, returns a data.frame
#'   of the non-unique transitions (first six columns) sorted by
#'   \code{Precursor Mz} and prints a warning message.
#' @export
#' @examples
#' \dontrun{
#' transition_checkR(transition_df)
#' transition_checkR(transition_df, tolerance = 0.005)
#' }
transition_checkR <- function(transition_df, tolerance = 0.001) {
  # Input validation

  if (!is.data.frame(transition_df)) {
    stop("transition_checkR: 'transition_df' must be a data.frame. Got: ",
         paste(class(transition_df), collapse = ", "), call. = FALSE)
  }

  if (nrow(transition_df) == 0) {
    stop("transition_checkR: 'transition_df' must not be empty (0 rows).",
         call. = FALSE)
  }

  required_cols <- c("Precursor Mz", "Product Mz", "Precursor Name")
  missing_cols <- setdiff(required_cols, colnames(transition_df))
  if (length(missing_cols) > 0) {
    stop("transition_checkR: 'transition_df' is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!is.numeric(transition_df$`Precursor Mz`)) {
    stop("transition_checkR: Column 'Precursor Mz' must be numeric. Got: ",
         class(transition_df$`Precursor Mz`), call. = FALSE)
  }

  if (anyNA(transition_df$`Precursor Mz`)) {
    stop("transition_checkR: Column 'Precursor Mz' must not contain NA values.",
         call. = FALSE)
  }

  if (!is.numeric(transition_df$`Product Mz`)) {
    stop("transition_checkR: Column 'Product Mz' must be numeric. Got: ",
         class(transition_df$`Product Mz`), call. = FALSE)
  }

  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    stop("transition_checkR: 'tolerance' must be a single non-negative number.",
         call. = FALSE)
  }

  df <- transition_df
  # Tolerance-based duplicate detection:
  #   tolerance == 0 → flag only exactly-identical Q1/Q3 pairs (|diff| == 0).
  #   tolerance  > 0 → flag when both |Q1_i - Q1_j| < tolerance AND
  #                    |Q3_i - Q3_j| < tolerance (strict less-than, matching
  #                    the function's documented semantics).
  #
  # For the `> 0` branch we round the *absolute difference* to the decimal
  # place implied by `tolerance` before comparing, so binary float wobble
  # at the boundary doesn't flip a pair between "duplicate" and "distinct".
  # Example: reading "329.249" vs "329.250" from a TSV yields stored
  # doubles whose difference is 0.00099999999997... — a naive `<` test
  # would treat them as a duplicate even though the authors intended them
  # as resolvable at their reported mass precision. After rounding, that
  # difference becomes exactly 0.001 which is NOT strictly less than 0.001.
  n <- nrow(df)
  q1 <- df$`Precursor Mz`
  q3 <- df$`Product Mz`
  dup_flag <- logical(n)
  if (tolerance == 0) {
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        if (q1[i] == q1[j] && q3[i] == q3[j]) {
          dup_flag[i] <- TRUE
          dup_flag[j] <- TRUE
        }
      }
    }
  } else {
    decimals <- max(0L, ceiling(-log10(tolerance)))
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        d_q1 <- round(abs(q1[i] - q1[j]), decimals)
        d_q3 <- round(abs(q3[i] - q3[j]), decimals)
        if (d_q1 < tolerance && d_q3 < tolerance) {
          dup_flag[i] <- TRUE
          dup_flag[j] <- TRUE
        }
      }
    }
  }
  non_unique_df <- df[dup_flag, , drop = FALSE]
  non_unique_metabolites <- unique(non_unique_df$`Precursor Name`)

  summary_cols <- intersect(MSTARGETR_TRANSITION_SUMMARY_COLS, colnames(non_unique_df))
  summary_df <- non_unique_df[, summary_cols, drop = FALSE]

  if (length(non_unique_metabolites) == 0) {
    message("Congratulations, all MRM transitions are unique.")
    return(invisible(NULL))
  } else {
    message("Please correct the following transition clashes:")
    summary_df <- summary_df[order(summary_df$`Precursor Mz`), ]
    return(summary_df)
  }
}

#' Compare MRM Template Internal Standards Against Concentration Guide
#'
#' Checks that every stable isotope-labelled (SIL) internal standard listed
#' in the \code{Note} column of an MRM transition template has a
#' corresponding entry in the concentration guide. Unmatched standards will
#' cause failures during response-concentration calculations in qcCheckR.
#'
#' @param mrm_template A data.frame of MRM transitions. Must contain a
#'   \code{Note} column whose non-NA values identify SIL internal standards.
#' @param concentration_guide A data.frame of SIL internal standard
#'   concentrations. Must contain a \code{SIL_name} column.
#' @return If all internal standards match, returns \code{NULL} invisibly and
#'   prints a success message. Otherwise, returns a character vector of
#'   unmatched \code{Note} values from the \code{mrm_template} that need to
#'   be corrected.
#' @export
#' @examples
#' \dontrun{
#' compare_mrm_template_with_guide(mrm_template, concentration_guide)
#' }
compare_mrm_template_with_guide <- function(mrm_template, concentration_guide) {
  # Input validation
  if (!is.data.frame(mrm_template)) {
    stop("compare_mrm_template_with_guide: 'mrm_template' must be a data.frame. Got: ",
         paste(class(mrm_template), collapse = ", "), call. = FALSE)
  }

  if (!is.data.frame(concentration_guide)) {
    stop("compare_mrm_template_with_guide: 'concentration_guide' must be a data.frame. Got: ",
         paste(class(concentration_guide), collapse = ", "), call. = FALSE)
  }

  if (nrow(mrm_template) == 0) {
    stop("compare_mrm_template_with_guide: 'mrm_template' must not be empty (0 rows).",
         call. = FALSE)
  }

  if (nrow(concentration_guide) == 0) {
    stop("compare_mrm_template_with_guide: 'concentration_guide' must not be empty (0 rows).",
         call. = FALSE)
  }

  if (!("Note" %in% names(mrm_template))) {
    stop("compare_mrm_template_with_guide: 'mrm_template' is missing required column: 'Note'.",
         call. = FALSE)
  }

  if (!("SIL_name" %in% names(concentration_guide))) {
    stop("compare_mrm_template_with_guide: 'concentration_guide' is missing required column: 'SIL_name'.",
         call. = FALSE)
  }

  if (!is.character(mrm_template$Note) && !is.factor(mrm_template$Note)) {
    stop("compare_mrm_template_with_guide: Column 'Note' must be character or factor. Got: ",
         class(mrm_template$Note), call. = FALSE)
  }
  note_values <- as.character(mrm_template$Note)
  note_values <- note_values[!is.na(note_values)]

  # Coerce empty strings ("") to NA — a frequent template-editing artefact where
  # a SIL cell is cleared but not set to NA. Warn so the user knows to fix it.
  empty_str_idx <- !is.na(note_values) & !nzchar(note_values)
  if (any(empty_str_idx)) {
    warning("compare_mrm_template_with_guide: ",
            sum(empty_str_idx),
            " empty-string Note value(s) found and treated as NA. ",
            "Set these cells to NA in the template to suppress this warning.",
            call. = FALSE)
    note_values <- note_values[nzchar(note_values)]
  }

  sil_values <- concentration_guide$SIL_name

  unmatched_notes <- note_values[!note_values %in% sil_values]

  if (length(unmatched_notes) == 0) {
    message("All internal standards in the MRM template have a matching entry in the concentration guide.")
    return(invisible(NULL))
  } else {
    message("Please fix the following metabolite targets with no matches")
    return(unmatched_notes)
  }
}
