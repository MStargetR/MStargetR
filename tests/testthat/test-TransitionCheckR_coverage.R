# Additional coverage tests for TransitionCheckR.R ----
# Targets zero-coverage lines 111-112: empty mrm_template validation.

# ============================================================================
# compare_mrm_template_with_guide -- empty mrm_template (lines 110-112)
# ============================================================================

test_that("compare_mrm_template_with_guide rejects empty mrm_template", {
  empty_mrm <- data.frame(
    Note = character(0),
    other_col = character(0),
    stringsAsFactors = FALSE
  )
  conc_guide <- data.frame(
    SIL_name = c("SIL_1", "SIL_2"),
    concentration = c(10, 20),
    stringsAsFactors = FALSE
  )

  expect_error(
    compare_mrm_template_with_guide(empty_mrm, conc_guide),
    "mrm_template.*must not be empty"
  )
})

test_that("compare_mrm_template_with_guide rejects zero-row mrm_template with multiple columns", {
  empty_mrm <- data.frame(
    Note = character(0),
    Compound = character(0),
    Q1 = numeric(0),
    Q3 = numeric(0),
    stringsAsFactors = FALSE
  )
  conc_guide <- data.frame(
    SIL_name = "SIL_A",
    concentration = 5,
    stringsAsFactors = FALSE
  )

  expect_error(
    compare_mrm_template_with_guide(empty_mrm, conc_guide),
    "mrm_template.*must not be empty.*0 rows"
  )
})
