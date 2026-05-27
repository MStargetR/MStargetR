# Standalone Interbatch Correction for Targeted LC-MS Data

Performs signal drift and interbatch correction on targeted LC-MS data
using QC sample-based methods from the `statTarget` package. This
function operates independently of the `qcCheckR` pipeline and accepts a
simple data.frame input that any user can prepare.

## Usage

``` r
batchCorrectR(
  data,
  qc_label = "qc",
  method = "QCRFSC",
  ntree = 500,
  coCV = 100,
  Frule = 0,
  imputeM = "minHalf",
  combat_par.prior = TRUE,
  combat_mean.only = FALSE,
  combat_ref.batch = NULL,
  qcrlsc_method = c("subtract", "divide"),
  qcrlsc_intra = FALSE,
  qcrlsc_opti = TRUE,
  qcrlsc_log10 = TRUE,
  qcrlsc_outl = TRUE,
  qcrlsc_shift = TRUE,
  batch_column = NULL,
  sample_tags = NULL,
  output_dir = tempdir(),
  project_dir = NULL,
  plot = TRUE,
  advanced_plots = FALSE,
  report = TRUE
)
```

## Arguments

- data:

  A data.frame, tibble, or a **list** of data.frames/tibbles to combine.
  Each data.frame must have samples as rows and must contain either the
  canonical columns (`sample_name`, `batch`, `sample_type`, `run_order`)
  or the MStargetR column convention (`sample_name`, `sample_plate_id`,
  `sample_type_factor`, `sample_run_index`). When a list is supplied the
  data.frames are row-bound before correction. Column
  `sample_type_factor` is used for QC matching against `qc_label` when
  present.

- qc_label:

  Character string identifying QC samples. Matched against
  `sample_type_factor` when present, otherwise `sample_type`. Default is
  `"qc"`.

- method:

  Correction method. One of `"QCRFSC"` (QC-based random forest signal
  correction, default), `"ComBat"` (empirical Bayes, QC-free), or
  `"QCRLSC"` (QC-based robust LOESS signal correction; Dunn et al. 2011,
  via the `qcrlscR` package). Like `"QCRFSC"`, `"QCRLSC"` requires QC
  samples.

- ntree:

  Integer. Number of trees for the random forest method. Default is
  `500`. Ignored when `method` is not `"QCRFSC"`.

- coCV:

  Numeric. Maximum percent RSD (coefficient of variation) cutoff passed
  to
  [`statTarget::shiftCor`](https://rdrr.io/pkg/statTarget/man/shiftCor.html).
  Features whose QC RSD exceeds this cutoff are dropped by statTarget;
  those features are then reverted to their original uncorrected values
  and a message is emitted listing them. Default is `100`, meaning
  features with QC RSD greater than 100\\ Use `Inf` for truly no
  filtering (no features will be dropped).

- Frule:

  Numeric. Proportion in `[0, 1]` (e.g., `0.8` for 80\\ Default is `0`
  (no filtering).

- imputeM:

  Character. Imputation method for missing values inside statTarget. One
  of `"minHalf"` (default), `"median"`, `"mean"`, or `"knn"`.

- combat_par.prior:

  Logical. If TRUE (default), use parametric empirical Bayes adjustments
  in ComBat. If FALSE, use non-parametric. Only used when
  `method = "ComBat"`.

- combat_mean.only:

  Logical. If TRUE, only correct the mean of the batch effect (no
  scale/variance adjustment). Default is FALSE. Only used when
  `method = "ComBat"`.

- combat_ref.batch:

  Optional character string. If provided, use this batch as the
  reference for ComBat adjustment. Default is NULL. Only used when
  `method = "ComBat"`. Must match a value present in the column selected
  by `batch_column` (or in `sample_plate_id`/`batch` when `batch_column`
  is NULL).

- qcrlsc_method:

  Character. QC-RLSC scaling, one of `"subtract"` (default) or
  `"divide"`. `"subtract"` matches the Dunn et al. protocol but can
  yield small negative values for low-abundance features; `"divide"`
  preserves non-negativity (better for concentrations) but is less
  stable when the fitted QC trend nears zero. Only used when
  `method = "QCRLSC"`.

- qcrlsc_intra:

  Logical. If TRUE, correct within each batch (intra-batch); if FALSE
  (default), correct across batches (inter-batch). Only meaningful with
  two or more batches. Only used when `method = "QCRLSC"`.

- qcrlsc_opti:

  Logical. If TRUE (default), optimise the LOESS span by generalised
  cross-validation. Only used when `method = "QCRLSC"`.

- qcrlsc_log10:

  Logical. If TRUE (default), log10-transform before fitting (zeros
  become missing). Only used when `method = "QCRLSC"`.

- qcrlsc_outl:

  Logical. If TRUE (default), perform QC outlier detection before
  fitting. Only used when `method = "QCRLSC"`.

- qcrlsc_shift:

  Logical. If TRUE (default), apply `batch.shift` to re-align batch
  means after signal correction. Only used when `method = "QCRLSC"`.

- batch_column:

  Optional character. Name of the column in `data` that holds the batch
  identifier. When `NULL` (default) the function uses `sample_plate_id`
  if present, otherwise `batch`. Set this to drive the correction off an
  arbitrary user-named column (e.g. `plate`, `run_batch`). The chosen
  column's values are also the valid choices for `combat_ref.batch`.

- sample_tags:

  Optional character vector of sample-type labels to include in the
  correction (in addition to the QC label). Matched case-insensitively
  against `sample_type_factor` when present, otherwise `sample_type`.
  Rows whose type does not match either `qc_label` or any of
  `sample_tags` are dropped before correction – useful for excluding
  blanks, double blanks, or other low-signal sample types that would
  otherwise distort the QCRFSC model. Default is `NULL` (no filtering;
  every row is kept).

- output_dir:

  Character. Directory path where statTarget writes its intermediate
  files. Default is [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- project_dir:

  Character or NULL. If provided, the corrected data CSV and correction
  summary are saved into a `batch_correction` subfolder inside this
  directory. Default is `NULL` (no file output).

- plot:

  Logical. Whether to populate `result$plots` with the base before/after
  correction ggplots (RSD comparison, run-order facet, PCA). Default
  `TRUE`.

  **Deprecated.** The argument is retained for backwards compatibility
  but will be removed in a future release. Use `advanced_plots = TRUE`
  to populate `result$plots` with the full GUI plot set AND save the
  figures to disk under `<project_dir>/all/figures/batch_corrector/`.
  Passing `plot` explicitly emits a deprecation warning.

- advanced_plots:

  Logical. When `TRUE`, every plot the GUI's Batch Correction tab
  renders (RSD comparison, run-order, PCA before/after, signal drift,
  RSD by class, per-metabolite RSD) is attached to `result$plots` *and*
  – if `project_dir` is supplied – written to
  `<project_dir>/all/figures/batch_corrector/` as both a static `.pdf`
  and an interactive `.html`. Default `FALSE` – opt-in so existing
  scripts behave identically.

- report:

  Logical. Whether to generate an HTML summary report. Default is
  `TRUE`.

## Value

A named list with the following elements:

- corrected_data:

  A tibble in the same structure as the input `data`, but with corrected
  metabolite values.

- correction_summary:

  A tibble with per-metabolite correction statistics including RSD
  before and after correction.

- qc_rsd_before:

  A named numeric vector of QC RSD values before correction for each
  metabolite.

- qc_rsd_after:

  A named numeric vector of QC RSD values after correction for each
  metabolite.

- failed_qc:

  A character vector of sample names flagged as failed QC injections
  (signal less than 10 percent of batch median).

- plots:

  A list of ggplot objects (only if `plot = TRUE`).

- report:

  Logical indicating whether an HTML report was requested.

- report_path:

  Path to the rendered HTML report (only if `report = TRUE` and
  rendering succeeds).

## Details

The correction pipeline proceeds as follows:

1.  **Input validation**: Checks that required columns exist, metabolite
    data is numeric, and QC samples are present in each batch.

2.  **Metabolite detection**: Identifies all numeric columns that are
    not metadata columns as metabolite features. Excluded metadata
    columns are defined in `.METADATA_COLS` (see
    `R/batchCorrectR_Utils.R`) and include `sample_name`, `batch`,
    `sample_type`, `run_order`, `sample_plate_id`, `sample_timestamp`,
    `sample_matrix`, `synthetic_qc`, and others.

3.  **Row filtering**: If `sample_tags` is supplied, rows whose
    `sample_type` does not match `qc_label` or any of the provided tags
    are dropped before correction. This step runs before QC flagging and
    affects both the correction model and the returned `corrected_data`.

4.  **QC flagging**: Flags QC injections where total signal is less than
    10 percent of the batch median, reclassifying them as regular
    samples to prevent them from distorting the correction model.

5.  **File preparation**: Creates PhenoFile.csv and ProfileFile.csv in
    the format required by
    [`statTarget::shiftCor`](https://rdrr.io/pkg/statTarget/man/shiftCor.html),
    ensuring QC samples bookend each batch.

6.  **Signal correction**: Runs
    [`statTarget::shiftCor`](https://rdrr.io/pkg/statTarget/man/shiftCor.html)
    with the specified method to model and remove systematic signal
    drift.

7.  **Post-correction adjustment**: Adjusts corrected values so that QC
    means match their original pre-correction scale, preserving
    biological interpretation of absolute values.

8.  **Reporting**: Optionally generates before/after RSD comparison
    tables and visualisations including run-order plots and PCA.

`QCRFSC` (QC-based Random Forest Signal Correction) fits a random forest
model to QC samples across run order and applies the learned correction
to all samples. It is robust to non-linear drift patterns.

## Examples

``` r
if (FALSE) { # \dontrun{
library(MStargetR)

# Prepare input data
my_data <- data.frame(
  sample_name = paste0("S", 1:30),
  batch = rep(c("plate1", "plate2"), each = 15),
  sample_type = rep(c("qc", "sample", "sample", "sample", "qc"), 6),
  run_order = 1:30,
  metabolite_A = rnorm(30, mean = 100, sd = 10),
  metabolite_B = rnorm(30, mean = 500, sd = 50),
  metabolite_C = rnorm(30, mean = 1000, sd = 100)
)

# Run batch correction with default settings (QCRFSC)
result <- batchCorrectR(data = my_data)

# Access corrected data
corrected <- result$corrected_data

# View RSD improvement
result$correction_summary

# Use ComBat (empirical Bayes, QC-free) instead of the default QCRFSC
result_combat <- batchCorrectR(my_data, method = "ComBat")

# Use QC-RLSC (QC-based robust LOESS signal correction)
result_qcrlsc <- batchCorrectR(my_data, method = "QCRLSC")
} # }
```
