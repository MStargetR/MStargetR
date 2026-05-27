# QC Assessment and Batch Correction for Targeted LC-MS Data

This function performs a series of quality control checks on the data
within a specified project directory.

## Usage

``` r
qcCheckR(
  user_name,
  project_directory,
  mrm_template_list = NULL,
  QC_sample_label = "LTR",
  sample_tags = NULL,
  mv_threshold = 50,
  batch_method = "QCRFSC",
  batch_ntree = 500,
  batch_coCV = 100,
  batch_Frule = 0,
  batch_imputeM = "minHalf",
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
  write_rda = TRUE,
  qs_nthreads = max(1L, parallel::detectCores() - 1L),
  qs_compress_level = 3L,
  date_order = c("auto", "dmy", "mdy", "ymd", "iso"),
  advanced_plots = FALSE
)
```

## Arguments

- user_name:

  A character string specifying the name of the user.

- project_directory:

  A character string specifying the path to the project directory.

- mrm_template_list:

  A list of MRM templates and associated concentration guide. Must have
  specific column names. See examples for structure of
  mrm_template_list. Must include mrm_guide labelled as "SIL_guide" and
  associated concentration guide labelled as "conc_guide". Can contain
  multiple combinations stored as separate lists, see examples.

- QC_sample_label:

  A character string containing the key tags to filter QC samples from
  file names. E.g. "qc".

- sample_tags:

  A character vector specifying the tags to filter sample types from
  file names. E.g. c("sample","control", "qc").

- mv_threshold:

  A numeric value between 0 and 100 specifying the threshold for missing
  values in the data. Default is 50(50%).

- batch_method:

  Character string specifying the batch correction method. One of
  `"QCRFSC"` (QC-based random forest signal correction, default),
  `"ComBat"` (empirical Bayes, QC-free), or `"QCRLSC"` (QC-based robust
  LOESS signal correction; Dunn et al. 2011, via the `qcrlscR` package).
  Like `"QCRFSC"`, `"QCRLSC"` requires QC samples.

- batch_ntree:

  Integer. Number of trees for the random forest method. Default is
  `500`. Ignored when `batch_method` is not `"QCRFSC"`.

- batch_coCV:

  Numeric. Coefficient of variation cutoff (percentage, 1–100) for
  feature filtering inside statTarget. Features with QC CV above this
  threshold are removed. Default is `100` (effectively no filtering).

- batch_Frule:

  Numeric. Filtering rule (0-1) for missing values inside statTarget.
  Default is `0` (no filtering).

- batch_imputeM:

  Character string. Imputation method for missing values. One of
  `"minHalf"` (default), `"median"`, `"mean"`, or `"knn"`.

- combat_par.prior:

  Logical. If TRUE (default), use parametric empirical Bayes
  adjustments. Only used when `batch_method = "ComBat"`.

- combat_mean.only:

  Logical. If TRUE, only correct the mean of the batch effect. Default
  is FALSE. Only used when `batch_method = "ComBat"`.

- combat_ref.batch:

  Optional character string specifying a reference batch. Default is
  NULL. Only used when `batch_method = "ComBat"`. Must match a value in
  the column selected by `batch_column` (or in `sample_plate_id` when
  `batch_column` is NULL).

- qcrlsc_method:

  Character. QC-RLSC scaling, one of `"subtract"` (default) or
  `"divide"`. `"subtract"` matches the Dunn et al. protocol but can
  yield small negative values for low-abundance features; `"divide"`
  preserves non-negativity (better for concentrations) but is less
  stable when the fitted QC trend nears zero. Only used when
  `batch_method = "QCRLSC"`.

- qcrlsc_intra:

  Logical. If TRUE, correct within each batch (intra-batch); if FALSE
  (default), correct across batches (inter-batch). Only meaningful with
  two or more batches. Only used when `batch_method = "QCRLSC"`.

- qcrlsc_opti:

  Logical. If TRUE (default), optimise the LOESS span by generalised
  cross-validation. Only used when `batch_method = "QCRLSC"`.

- qcrlsc_log10:

  Logical. If TRUE (default), log10-transform before fitting (zeros
  become missing). Only used when `batch_method = "QCRLSC"`.

- qcrlsc_outl:

  Logical. If TRUE (default), perform QC outlier detection before
  fitting. Only used when `batch_method = "QCRLSC"`.

- qcrlsc_shift:

  Logical. If TRUE (default), apply `batch.shift` to re-align batch
  means after signal correction. Only used when
  `batch_method = "QCRLSC"`.

- batch_column:

  Optional character. Name of the column in the imputed concentration
  data that holds the batch identifier used by ComBat. When `NULL`
  (default) the canonical `sample_plate_id` column is used. Set this to
  drive the correction off an arbitrary user-named column (e.g. `plate`,
  `run_batch`); the chosen column's values become the valid choices for
  `combat_ref.batch`. Used when `batch_method = "ComBat"` or `"QCRLSC"`.

- write_rda:

  Logical. When `TRUE` (default) the master_list `.qs2` file is written
  synchronously as the final step of the pipeline. Set to `FALSE` when
  the caller intends to write it out-of-band — for example, the Shiny
  GUI passes `FALSE` so it can surface results to the user immediately
  and fire a separate background job that calls
  [`export_master_list_qs`](https://mstargetr.github.io/MStargetR/reference/export_master_list_qs.md)
  on the returned master_list. The XLSX and HTML exports are unaffected.
  The argument name is retained from the previous `.rda` API to avoid
  churning every caller; the underlying output is now `.qs2`.

- qs_nthreads:

  Integer worker threads forwarded to
  [`qs_save`](https://rdrr.io/pkg/qs2/man/qs_save.html). Defaults to
  `max(1L, parallel::detectCores() - 1L)`. Multi-threaded zstd is what
  makes large-cohort saves complete in reasonable time; the previous
  single-threaded gzip path via
  [`base::save()`](https://rdrr.io/r/base/save.html) stalled for hours
  on a 54-plate cohort.

- qs_compress_level:

  Integer zstd compression level forwarded to
  [`qs_save`](https://rdrr.io/pkg/qs2/man/qs_save.html). Default `3L`
  (qs2 default; fast with good ratio). Higher values (up to 22) shrink
  the file further at the cost of CPU time; negative values trade ratio
  for more speed.

- date_order:

  Controls how the `AcquiredTime` column from PeakForgeR reports (which
  Skyline exports in the OS locale of whoever ran the export) is parsed.
  One of `"auto"` (default; the pipeline inspects the cohort, prefers
  mzML `startTimeStamp` ISO 8601 headers where available, and chooses an
  unambiguous order from the cohort's parse pattern and any `_YYYYMMDD$`
  plate-name hints), `"dmy"` (day-first slash/dash formats), `"mdy"`
  (month-first slash/dash formats), or `"ymd"` / `"iso"` (ISO 8601
  only). If `"auto"` cannot resolve the format unambiguously, the
  pipeline stops with a clear message asking you to set this argument
  explicitly rather than silently produce wrong dates.

- advanced_plots:

  Logical. When `TRUE`, every plot the GUI's QC Check tab renders (PCA
  scores, run-order, per-metabolite control charts, %RSD histogram,
  missing values, sample-type pie, plate distribution) is also written
  to `<project_directory>/all/figures/qcCheckR/` as both a static `.pdf`
  (via
  [`ggplot2::ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html))
  and an interactive `.html` (via
  [`htmlwidgets::saveWidget`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)
  on the plotly widget). Default `FALSE` – opt-in so existing scripts
  continue to behave identically.

## Value

A list containing the processed data and generated reports.

## Details

Capable of combining multiple cohort and methods if a common long term
reference sample has been used throughout and target metabolite naming
conventions have been preserved. To allow this feature all methods must
be included in the mrm_template_list. Please note only matching
metabolite feature names across cohorts/methods will be processed.

If you have not used the MStargetR::PeakForgeR function to generate
reports please ensure your report file names contains `_PeakForgeR_` to
ensure the function can correctly identify the files in your project
directory.

The steps below describe the pipeline in execution order. Input
Validation steps are enforced by explicit
[`stop()`](https://rdrr.io/r/base/stop.html) calls. All other steps run
unconditionally; errors in any step propagate to the caller.

- **Input Validation (enforced):**

  - Validate user_name

  - Validate project_directory

  - Validate mrm_template_list

  - Validate QC_sample_label

  - Validate sample_tags

  - Validate mv_threshold

- **Project Setup:**

  - Initialise project structure

  - Load and organise input data

- **Data Preparation:**

  - Transpose data

  - Sort data

  - Impute missing values

  - Calculate response concentrations

  - Apply batch correction using statTarget

- **Filtering:**

  - Set QC samples

  - Filter samples

  - Filter SIL internal standards

  - Apply lipid-specific filters

  - Filter based on RSD thresholds

- **Reporting and Visualisation:**

  - Generate summary report

  - Create optional plots

  - Perform PCA analysis

  - Generate run order plots

  - Create target control charts

- **Export:**

  - Export all processed data and reports

## Note

When `batch_method = "ComBat"` the sva Bioconductor package is required.
Install it with `BiocManager::install("sva")` before use.

## Examples

``` r
if (FALSE) { # \dontrun{

library(MStargetR)

#Load example mrm_template_list
  file_path <- system.file("extdata",
                           "LGW_lipid_mrm_template_v1.tsv",
                           package = "MStargetR")

  sample_metadata_example <- readr::read_tsv(file_path)

#Load example conc_guide
  file_path <- system.file("extdata",
                           "LGW_SIL_batch_Ultimate_2023_03_06.tsv",
                           package = "MStargetR")

  conc_guide_example <- readr::read_tsv(file_path)

#Load example report file
  file_path <- system.file("extdata",
                           "Example_PeakForgeR_report.csv",
                           package = "MStargetR")

  report_file <- read.csv(file_path)

# Using QCRFSC (default, requires QC samples)
qcCheckR(user_name = "user1",
         project_directory = "path/to/project_directory",
         mrm_template_list = list(v1 = list(
                                    SIL_guide = "path/to/mrm_guide1.tsv",
                                    conc_guide = "path/to/SIL_concentration_guide1.tsv")),
         QC_sample_label = "qc",
         sample_tags = c("sample", "control", "blank", "qc"),
         mv_threshold = 50,
         batch_method = "QCRFSC")

# Using ComBat (does not require QC samples)
qcCheckR(user_name = "user1",
         project_directory = "path/to/project_directory",
         mrm_template_list = list(v1 = list(
                                    SIL_guide = "path/to/mrm_guide1.tsv",
                                    conc_guide = "path/to/SIL_concentration_guide1.tsv")),
         QC_sample_label = "qc",
         sample_tags = c("sample", "control", "blank", "qc"),
         mv_threshold = 50,
         batch_method = "ComBat")
} # }
```
