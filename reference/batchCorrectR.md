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
  sample_tags = NULL,
  output_dir = tempdir(),
  project_dir = NULL,
  plot = TRUE,
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

  Correction method. One of `"QCRFSC"` (random forest, default) or
  `"ComBat"` (empirical Bayes, QC-free).

- ntree:

  Integer. Number of trees for the random forest method. Default is
  `500`. Ignored when `method` is not `"QCRFSC"`.

- coCV:

  Numeric. Coefficient of variation cutoff (percentage, 1–100) for
  feature filtering inside statTarget. Features with QC CV above this
  threshold are removed. Default is `100` (effectively no filtering).

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
  `method = "ComBat"`.

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

  Logical. Whether to generate before/after correction plots. Default is
  `TRUE`.

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

- report_path:

  Path to the rendered HTML report (only if `report = TRUE` and
  rendering succeeds).

## Details

The correction pipeline proceeds as follows:

1.  **Input validation**: Checks that required columns exist, metabolite
    data is numeric, and QC samples are present in each batch.

2.  **Metabolite detection**: Identifies all numeric columns that are
    not metadata (`sample_name`, `batch`, `sample_type`, `run_order`) as
    metabolite features.

3.  **QC flagging**: Flags QC injections where total signal is less than
    10 percent of the batch median, reclassifying them as regular
    samples to prevent them from distorting the correction model.

4.  **File preparation**: Creates PhenoFile.csv and ProfileFile.csv in
    the format required by
    [`statTarget::shiftCor`](https://rdrr.io/pkg/statTarget/man/shiftCor.html),
    ensuring QC samples bookend each batch.

5.  **Signal correction**: Runs
    [`statTarget::shiftCor`](https://rdrr.io/pkg/statTarget/man/shiftCor.html)
    with the specified method to model and remove systematic signal
    drift.

6.  **Post-correction adjustment**: Adjusts corrected values so that QC
    means match their original pre-correction scale, preserving
    biological interpretation of absolute values.

7.  **Reporting**: Optionally generates before/after RSD comparison
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
} # }
```
