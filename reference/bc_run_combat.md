# Run ComBat Batch Correction

Applies [`sva::ComBat`](https://rdrr.io/pkg/sva/man/ComBat.html) to a
data.frame that has a `batch` column and numeric metabolite columns.

## Usage

``` r
bc_run_combat(
  data,
  metabolite_cols,
  par.prior = TRUE,
  mean.only = FALSE,
  ref.batch = NULL
)
```

## Arguments

- data:

  Data.frame with a `batch` column and metabolite value columns.

- metabolite_cols:

  Character vector of metabolite column names.

- par.prior:

  Logical. Parametric (TRUE) or non-parametric (FALSE) priors.

- mean.only:

  Logical. If TRUE, only correct batch mean effect.

- ref.batch:

  Optional reference batch.

## Value

Data.frame in the same format as input with corrected metabolite values.

## Details

Handles NA imputation (column-median fill), zero-variance feature
removal, and reconstruction of the corrected data.frame.
