# Missing-value bar chart (advanced_plots)

Top-40 metabolites by % missing values, computed from pre-imputation
peak-area data (falling back to imputed concentration if peak-area is
absent — mirrors the Shiny QC tab's lookup order).

## Usage

``` r
qc_plot_missing_values(master_list)
```

## Arguments

- master_list:

  A qcCheckR master_list.

## Value

`list(static, interactive)`, or `NULL` if no missing values.
