# Sample-type distribution pie (advanced_plots)

Pie chart of counts per `sample_type_factor` (falling back to
`sample_type`). Mirrors the Shiny QC tab's `qc_sample_type_pie`.

## Usage

``` r
qc_plot_sample_type_distribution(master_list)
```

## Arguments

- master_list:

  A qcCheckR master_list.

## Value

`list(static, interactive)`, or `NULL`.
