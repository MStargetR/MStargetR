# Adjust Corrected QC Means to Original Scale

Adjust Corrected QC Means to Original Scale

## Usage

``` r
bc_adjust_corrected_means(
  corrected_clean,
  data_flagged,
  qc_label,
  metabolite_cols
)
```

## Arguments

- corrected_clean:

  Tibble of corrected data.

- data_flagged:

  Original (flagged) data.frame.

- qc_label:

  Character identifying QC samples.

- metabolite_cols:

  Character vector of metabolite column names.

## Value

Tibble with mean-adjusted corrected values.
