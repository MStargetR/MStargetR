# Run ComBat Batch Correction

Applies empirical Bayes batch correction using `sva::ComBat`. Unlike
statTarget methods, ComBat does not require QC samples.

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

  Data.frame with sample metadata and metabolite columns.

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
