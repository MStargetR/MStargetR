# Suppress R CMD check NOTEs for NSE (dplyr/tidy-eval) column references.
# Entries are grouped by the source file that uses them so stale entries can
# be identified and removed when the owning file changes.

#' @importFrom utils globalVariables

# --- R/PeakForgeR_Utils.R ---
utils::globalVariables(c(
  ".",
  ".data",
  ".env",
  "AcquiredTime",
  "Area",
  "FileName",
  "FullPeptideName",
  "MoleculeName",
  "Name",
  "Note",
  "PC1",
  "PC2",
  "Precursor",
  "Precursor Name",
  "SIL",
  "V1",
  "V2",
  "failed_samples",
  "facet_label",
  "molecule_name",
  "name",
  "p1",
  "p2",
  "p3",
  "precursor_name",
  "source_prefix"
))

# --- R/qcCheckR_Utils.R / R/qcCheckR_export.R / R/qcCheckR_dataprep.R ---
utils::globalVariables(c(
  ":=",            # rlang/dplyr NSE assignment used in plot helpers
  "SIL_name",
  "concentration_factor",
  "data",          # rlang::.data alias surfaced in format_rsd_table tidy-eval
  "invalid_wiff_files",
  "lipid",
  "lipid_class",
  "matches",
  "metabolite_code",
  "original_mean",
  "corrected_mean",
  "plateID",
  "sample.flag",
  "sample_ID",
  "sample_class",  # finalise_sorted_data dplyr column reference
  "sample_data_source",
  "sample_matrix",
  "sample_name",
  "sample_plate_id",
  "sample_plate_order",
  "sample_run_index",
  "sample_timestamp",
  "sample_type",
  "sample_type_factor",
  "sample_type_factor_rev",
  "template_version",
  "value"
))

# --- R/batchCorrectR_Utils.R ---
utils::globalVariables(c(
  "batch",
  "batch_num",
  "class_st",
  "dataBatch",
  "dataSource",
  "improved",
  "is_qc",
  "metabolite",
  "order_seq",
  "rsd",
  "rsd_change",
  "run_order",
  "stage",
  "synthetic_qc"   # bc_prepare_pheno_file dplyr column reference
))

.onAttach <- function(libname, pkgname) {
  if (requireNamespace("shiny", quietly = TRUE)) {
    packageStartupMessage(
      "MStargetR -- Targeted LC-MS Preprocessing Pipeline\n",
      "Run MStargetR::launchMStargetR() to open the GUI.\n",
      "See ?MStargetR for documentation."
    )
  } else {
    packageStartupMessage(
      "MStargetR -- Targeted LC-MS Preprocessing Pipeline\n",
      "GUI not available (install 'shiny' and related packages to enable).\n",
      "See ?MStargetR for documentation."
    )
  }
}

