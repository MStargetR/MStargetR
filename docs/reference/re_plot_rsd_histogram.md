# RSD distribution histogram (advanced_plots)

Histogram of per-metabolite %RSD with optional warn/fail reference
lines. Mirrors the Shiny `results_rsd_histogram`.

## Usage

``` r
re_plot_rsd_histogram(rsd_values, warn_thr = 20, fail_thr = 30)
```

## Arguments

- rsd_values:

  Named numeric vector of metabolite -\> RSD%.

- warn_thr, fail_thr:

  Numeric thresholds (default 20, 30) drawn as amber/red reference
  lines.

## Value

`list(static, interactive)`, or `NULL`.
