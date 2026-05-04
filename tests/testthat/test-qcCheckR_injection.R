library(mockery)

# Tests for precursor-name injection guard in export_html_report.
# The function must reject names that could break out of knitr chunk syntax.

# Helper: build the minimal master_list structure that reaches the validation
# code inside export_html_report, bypassing the project_dir/output setup by
# stubbing the parts that depend on a real filesystem.

make_ml_for_export <- function(chart_names) {
  named_charts <- stats::setNames(
    lapply(chart_names, function(x) NULL),
    chart_names
  )
  list(
    project_details = list(
      project_dir = tempfile("ml_export_"),
      user_name   = "testuser",
      project_name = "testproject"
    ),
    control_charts = named_charts
  )
}

# ---- Injection payloads must be rejected ----------------------------------

test_that("export_html_report rejects name with apostrophe (closes list-index string)", {
  ml <- make_ml_for_export(c("Glucose", "x'); system('echo pwned'); #"))
  expect_error(
    export_html_report(ml),
    "disallowed characters",
    fixed = TRUE
  )
})

test_that("export_html_report rejects name with backtick (closes chunk fence)", {
  ml <- make_ml_for_export(c("Alanine", "bad```name"))
  expect_error(
    export_html_report(ml),
    "disallowed characters",
    fixed = TRUE
  )
})

test_that("export_html_report rejects name with backslash", {
  ml <- make_ml_for_export(c("Glycine", "bad\\name"))
  expect_error(
    export_html_report(ml),
    "disallowed characters",
    fixed = TRUE
  )
})

test_that("export_html_report rejects name containing a newline", {
  ml <- make_ml_for_export(c("Serine", "bad\nname"))
  expect_error(
    export_html_report(ml),
    "disallowed characters",
    fixed = TRUE
  )
})

test_that("export_html_report rejects name containing a double-quote", {
  ml <- make_ml_for_export(c("Lysine", 'bad"name'))
  expect_error(
    export_html_report(ml),
    "disallowed characters",
    fixed = TRUE
  )
})

# Canonical injection payload from the audit specification
test_that("export_html_report rejects the canonical audit injection payload", {
  payload <- "x'); system('echo pwned'); #`"
  ml <- make_ml_for_export(c("Leucine", payload))
  expect_error(
    export_html_report(ml),
    "disallowed characters",
    fixed = TRUE
  )
})

# ---- Legitimate names must be accepted (no error before filesystem ops) ---

# We stub out everything after the validation/loop section so the test does
# not need a real project directory, rmarkdown, or pandoc.

test_that("export_html_report accepts legitimate metabolite names", {
  legitimate <- c(
    "Cysteine (Cys)",
    "1-Methylhistidine",
    "Phosphatidylcholine 34:1",
    "TAG 52:2 [NL 18:2]",
    "LPC 18:0/0:0",
    "Ala.Gly",
    "C18:1+O"
  )
  ml <- make_ml_for_export(legitimate)

  # Stub writeLines/tempfile so we never touch the filesystem, and stub
  # requireNamespace to trigger the early-return warning path.
  stub(export_html_report, "tempfile", function(...) tempfile(fileext = ".Rmd"))
  stub(export_html_report, "readLines", function(...) "control_charts_custom_code_placeholder")
  stub(export_html_report, "writeLines", function(...) invisible(NULL))
  stub(export_html_report, "requireNamespace", function(pkg, ...) FALSE)

  # Should warn about rmarkdown being absent, NOT error about names.
  expect_warning(
    export_html_report(ml),
    "rmarkdown"
  )
})
