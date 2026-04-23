# Generate Correction Report

Generate Correction Report

## Usage

``` r
bc_generate_correction_report(
  correction_summary,
  failed_qc,
  method,
  n_samples,
  n_batches,
  n_metabolites
)
```

## Arguments

- correction_summary:

  Per-metabolite stats tibble.

- failed_qc:

  Character vector of failed QC sample names.

- method, n_samples, n_batches, n_metabolites:

  Scalar parameters.

## Value

List containing report sections.
