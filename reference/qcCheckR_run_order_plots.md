# Run Order Plots

Generates run order plots for the `master_list` data, showing PCA scores
versus run order using `ggplot2`.

## Usage

``` r
qcCheckR_run_order_plots(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with run order plots stored in
`master_list$pca$scoresRunOrder`.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_run_order_plots(master_list)
} # }
```
