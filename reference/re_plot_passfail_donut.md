# Pass/fail donut (advanced_plots)

Donut chart of pass / warning / fail counts across metabolites. Mirrors
the Shiny `results_passfail_donut`.

## Usage

``` r
re_plot_passfail_donut(status_vector)
```

## Arguments

- status_vector:

  Named character vector with values in `c("pass", "warning", "fail")`
  (typically from `re_status_from_rsd()`).

## Value

`list(static, interactive)`, or `NULL`.
