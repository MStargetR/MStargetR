# Generate Correction Visualisations

Generate Correction Visualisations

## Usage

``` r
bc_plot_correction_results(
  original_data,
  corrected_data,
  qc_label,
  metabolite_cols,
  qc_rsd_before,
  qc_rsd_after
)
```

## Arguments

- original_data, corrected_data:

  Data.frames before/after correction.

- qc_label:

  Character identifying QC samples.

- metabolite_cols:

  Character vector of metabolite column names.

- qc_rsd_before, qc_rsd_after:

  Named numeric vectors of QC RSD.

## Value

Named list of ggplot objects.
