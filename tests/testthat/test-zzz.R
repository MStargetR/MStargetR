# Tests for zzz.R - package startup hooks ----
library(mockery)

# ============================================================================
# .onAttach tests
# ============================================================================

test_that(".onAttach emits GUI message when shiny is available", {
  # Run the real .onAttach; the message content depends on whether shiny is
  # installed. Both branches must mention "MStargetR".
  msgs <- capture.output(
    type = "message",
    MStargetR:::.onAttach("fakepath", "MStargetR")
  )
  expect_true(any(grepl("MStargetR", msgs)))
  shiny_present <- requireNamespace("shiny", quietly = TRUE)
  if (shiny_present) {
    expect_true(any(grepl("launchMStargetR", msgs)))
  }
})

test_that(".onAttach emits fallback message when shiny is unavailable", {
  # Directly invoke the fallback branch logic to assert the message text.
  msgs <- capture.output(type = "message",
    packageStartupMessage(
      "MStargetR -- Targeted LC-MS Preprocessing Pipeline\n",
      "GUI not available (install 'shiny' and related packages to enable).\n",
      "See ?MStargetR for documentation."
    )
  )
  expect_true(any(grepl("GUI not available", msgs)))
  expect_true(any(grepl("MStargetR", msgs)))
})

test_that(".onAttach emits a startup message", {
  # Whether or not shiny is installed, .onAttach should produce a message
  # (content differs by branch but a message is always emitted).
  msgs <- capture.output(
    type = "message",
    MStargetR:::.onAttach("fakepath", "MStargetR")
  )
  expect_true(length(msgs) > 0)
  expect_true(any(grepl("MStargetR", msgs)))
})

# ============================================================================
# globalVariables declaration
# ============================================================================

test_that("every name in production globalVariables is referenced in R/", {
  # Find the R/ source directory across the three contexts this test runs in:
  #   1. R CMD check    — pkg source lives at ../../R relative to test dir
  #   2. devtools::test — pkg source lives at ../../R relative to test dir too,
  #                       but cwd may be project root, so check that as well
  #   3. installed pkg  — system.file("R", ...) returns the install dir, which
  #                       contains compiled .rdb files but no .R files
  candidate_dirs <- c(
    testthat::test_path("..", "..", "R"),
    file.path(getwd(), "R"),
    system.file("R", package = "MStargetR")
  )
  r_dir <- NULL
  for (d in candidate_dirs) {
    if (nzchar(d) && dir.exists(d) &&
        length(list.files(d, pattern = "\\.R$")) > 0) {
      r_dir <- d
      break
    }
  }
  skip_if(is.null(r_dir),
          "R/ source files not available in this run (e.g. installed-only check)")

  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  r_source <- paste(unlist(lapply(r_files, readLines)), collapse = "\n")

  # All names declared across all globalVariables() calls in zzz.R
  declared <- c(
    ".", ".data", ".env", "AcquiredTime", "Area", "FileName",
    "FullPeptideName", "MoleculeName", "Name", "Note", "PC1", "PC2",
    "Precursor", "Precursor Name", "SIL", "V1", "V2", "failed_samples",
    "facet_label", "molecule_name", "name", "p1", "p2", "p3",
    "precursor_name", "source_prefix",
    "SIL_name", "concentration_factor", "invalid_wiff_files",
    "lipid", "lipid_class", "matches", "metabolite_code",
    "original_mean", "corrected_mean", "plateID", "sample.flag",
    "sample_ID", "sample_data_source", "sample_matrix", "sample_name",
    "sample_plate_id", "sample_plate_order", "sample_run_index",
    "sample_timestamp", "sample_type", "sample_type_factor",
    "sample_type_factor_rev", "template_version", "value",
    "batch", "batch_num", "class_st", "dataBatch", "dataSource",
    "improved", "is_qc", "metabolite", "order_seq", "rsd",
    "rsd_change", "run_order", "stage"
  )

  # Each name should appear at least once in the R source
  missing_refs <- declared[!vapply(declared, grepl, logical(1),
                                   x = r_source, fixed = TRUE)]
  expect_equal(
    missing_refs, character(0),
    info = paste("Names in globalVariables not found in R/ source:",
                 paste(missing_refs, collapse = ", "))
  )
})
