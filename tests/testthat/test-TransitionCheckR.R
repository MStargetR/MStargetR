# Tests for TransitionCheckR.R functions ----

# ============================================================================
# Helper: build a minimal MRM template data.frame
# ============================================================================
make_mrm_template <- function(n = 5) {

  data.frame(
    `Molecule List Name` = paste0("Group", seq_len(n)),
    `Precursor Name`     = paste0("Met_", LETTERS[seq_len(n)]),
    `Precursor Mz`       = seq(100, by = 50, length.out = n),
    `Precursor Charge`   = rep(1, n),
    `Product Mz`         = seq(80, by = 50, length.out = n),
    `Product Charge`     = rep(1, n),
    `Explicit Retention Time`        = seq(1, by = 0.5, length.out = n),
    `Explicit Retention Time Window`  = rep(1, n),
    `Note`               = paste0("SIL_", LETTERS[seq_len(n)]),
    `control_chart`      = rep("yes", n),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# transition_checkR ----
# ============================================================================

test_that("transition_checkR reports all unique transitions with a message", {
  template <- make_mrm_template(5)
  expect_message(
    result <- transition_checkR(template),
    "all MRM transitions are unique"
  )
  # When all unique, function returns NULL (invisible)
  expect_null(result)
})

test_that("transition_checkR detects duplicate Q1/Q3 combinations", {
  template <- make_mrm_template(5)
  # Create a duplicate: make row 2 same Q1/Q3 as row 1
  template$`Precursor Mz`[2] <- template$`Precursor Mz`[1]
  template$`Product Mz`[2]   <- template$`Product Mz`[1]
  expect_message(
    result <- transition_checkR(template),
    "Please correct the following transition clashes"
  )
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)
  # Both clashing metabolite names should appear
  expect_true("Met_A" %in% result$`Precursor Name`)
  expect_true("Met_B" %in% result$`Precursor Name`)
})

test_that("transition_checkR returns duplicates sorted by Precursor Mz", {
  template <- make_mrm_template(4)
  # Make rows 3 and 4 duplicate
  template$`Precursor Mz`[4] <- template$`Precursor Mz`[3]
  template$`Product Mz`[4]   <- template$`Product Mz`[3]
  result <- suppressMessages(transition_checkR(template))
  # Result should be ordered by Precursor Mz
  expect_true(all(diff(result$`Precursor Mz`) >= 0))
})

test_that("transition_checkR handles single-row template", {
  template <- make_mrm_template(1)
  expect_message(
    result <- transition_checkR(template),
    "all MRM transitions are unique"
  )
  expect_null(result)
})

test_that("transition_checkR handles multiple groups of duplicates", {
  template <- make_mrm_template(6)
  # Group 1: rows 1 and 2 duplicate
  template$`Precursor Mz`[2] <- template$`Precursor Mz`[1]
  template$`Product Mz`[2]   <- template$`Product Mz`[1]
  # Group 2: rows 4 and 5 duplicate
  template$`Precursor Mz`[5] <- template$`Precursor Mz`[4]
  template$`Product Mz`[5]   <- template$`Product Mz`[4]
  result <- suppressMessages(transition_checkR(template))
  expect_true(nrow(result) >= 4)
})

test_that("transition_checkR returns first 6 columns of clashing rows", {
  template <- make_mrm_template(3)
  template$`Precursor Mz`[2] <- template$`Precursor Mz`[1]
  template$`Product Mz`[2]   <- template$`Product Mz`[1]
  result <- suppressMessages(transition_checkR(template))
  expect_equal(ncol(result), 6)
})

test_that("transition_checkR errors on NA Precursor Mz values", {
  template <- make_mrm_template(3)
  template$`Precursor Mz`[2] <- NA_real_
  expect_error(
    transition_checkR(template),
    "must not contain NA values"
  )
})

# ============================================================================
# compare_mrm_template_with_guide ----
# ============================================================================

test_that("compare_mrm_template_with_guide reports all matches", {
  template <- make_mrm_template(3)
  guide <- data.frame(
    SIL_name = c("SIL_A", "SIL_B", "SIL_C"),
    concentration = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  expect_message(
    result <- compare_mrm_template_with_guide(template, guide),
    "All internal standards in the MRM template have a matching entry"
  )
  expect_null(result)
})

test_that("compare_mrm_template_with_guide returns unmatched Notes", {
  template <- make_mrm_template(3)
  # Only provide SIL for first metabolite
  guide <- data.frame(
    SIL_name = c("SIL_A"),
    concentration = c(10),
    stringsAsFactors = FALSE
  )
  expect_message(
    result <- compare_mrm_template_with_guide(template, guide),
    "Please fix the following"
  )
  expect_true("SIL_B" %in% result)
  expect_true("SIL_C" %in% result)
  expect_false("SIL_A" %in% result)
})

test_that("compare_mrm_template_with_guide errors on missing Note column", {
  template <- data.frame(x = 1, check.names = FALSE)
  guide <- data.frame(SIL_name = "a", stringsAsFactors = FALSE)
  expect_error(
    compare_mrm_template_with_guide(template, guide),
    "missing required column.*Note"
  )
})

test_that("compare_mrm_template_with_guide errors on missing SIL_name column", {
  template <- data.frame(Note = "a", check.names = FALSE, stringsAsFactors = FALSE)
  guide <- data.frame(other = "a", stringsAsFactors = FALSE)
  expect_error(
    compare_mrm_template_with_guide(template, guide),
    "missing required column.*SIL_name"
  )
})

test_that("compare_mrm_template_with_guide ignores NA Notes", {
  template <- make_mrm_template(3)
  template$Note[2] <- NA
  guide <- data.frame(
    SIL_name = c("SIL_A", "SIL_C"),
    concentration = c(10, 30),
    stringsAsFactors = FALSE
  )
  expect_message(
    result <- compare_mrm_template_with_guide(template, guide),
    "All internal standards in the MRM template have a matching entry"
  )
  expect_null(result)
})

test_that("compare_mrm_template_with_guide handles empty Note values", {
  template <- make_mrm_template(2)
  template$Note <- NA_character_
  guide <- data.frame(SIL_name = "SIL_A", stringsAsFactors = FALSE)
  # All notes are NA so filtered out, empty vector matches everything
  expect_message(
    result <- compare_mrm_template_with_guide(template, guide),
    "All internal standards in the MRM template have a matching entry"
  )
})

test_that("compare_mrm_template_with_guide with empty guide errors", {
  template <- make_mrm_template(2)
  guide <- data.frame(SIL_name = character(0), stringsAsFactors = FALSE)
  expect_error(
    compare_mrm_template_with_guide(template, guide),
    "must not be empty"
  )
})

test_that("compare_mrm_template_with_guide errors on non-character non-factor Note", {
  template <- make_mrm_template(2)
  template$Note <- c(1L, 2L)
  guide <- data.frame(SIL_name = "SIL_A", stringsAsFactors = FALSE)
  expect_error(
    compare_mrm_template_with_guide(template, guide),
    "Column 'Note' must be character or factor"
  )
})

test_that("compare_mrm_template_with_guide accepts factor Note and matches by label", {
  template <- make_mrm_template(3)
  template$Note <- factor(template$Note, levels = c("SIL_C", "SIL_B", "SIL_A"))
  guide <- data.frame(
    SIL_name = c("SIL_A", "SIL_B", "SIL_C"),
    concentration = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  expect_message(
    result <- compare_mrm_template_with_guide(template, guide),
    "All internal standards in the MRM template have a matching entry"
  )
  expect_null(result)
})
