# Build Correction Summary Table

Build Correction Summary Table

## Usage

``` r
bc_build_correction_summary(metabolite_cols, qc_rsd_before, qc_rsd_after)
```

## Arguments

- metabolite_cols:

  Character vector of metabolite column names.

- qc_rsd_before, qc_rsd_after:

  Named numeric vectors of QC RSD.

## Value

Tibble with per-metabolite correction statistics.
