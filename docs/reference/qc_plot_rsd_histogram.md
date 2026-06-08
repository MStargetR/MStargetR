# RSD histogram (advanced_plots)

Distribution of per-metabolite QC %RSD with a vertical reference line at
`fail_thr` (matches the Shiny QC Check tab's threshold annotation).

## Usage

``` r
qc_plot_rsd_histogram(master_list, fail_thr = 30)
```

## Arguments

- master_list:

  A qcCheckR master_list.

- fail_thr:

  Numeric. RSD% threshold to mark. Default 30.

## Value

`list(static = <ggplot>, interactive = <plotly>)`, or `NULL` if no RSD
values are available.
