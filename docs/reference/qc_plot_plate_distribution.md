# Plate distribution bar (advanced_plots)

Bar chart of sample counts per `sample_plate_id`. Mirrors the Shiny QC
tab's `qc_plate_bar`.

## Usage

``` r
qc_plot_plate_distribution(master_list)
```

## Arguments

- master_list:

  A qcCheckR master_list.

## Value

`list(static, interactive)`, or `NULL`.
