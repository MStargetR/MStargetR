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
  batch_column = NULL,
  write_rda = TRUE
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
  `"QCRFSC"` (random forest, default) or `"ComBat"` (empirical Bayes,
  QC-free).

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

- batch_column:

  Optional character. Name of the column in the imputed concentration
  data that holds the batch identifier used by ComBat. When `NULL`
  (default) the canonical `sample_plate_id` column is used. Set this to
  drive the correction off an arbitrary user-named column (e.g. `plate`,
  `run_batch`); the chosen column's values become the valid choices for
  `combat_ref.batch`. Only used when `batch_method = "ComBat"`.

- write_rda:

  Logical. When `TRUE` (default) the master_list RDA file is written
  synchronously as the final step of the pipeline. Set to `FALSE` when
  the caller intends to write the RDA out-of-band — for example, the
  Shiny GUI passes `FALSE` so it can surface results to the user
  immediately and fire a separate background job that calls
  [`export_master_list_rda`](https://mstargetr.github.io/MStargetR/reference/export_master_list_rda.md)
  on the returned master_list. The XLSX and HTML exports are unaffected.

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
