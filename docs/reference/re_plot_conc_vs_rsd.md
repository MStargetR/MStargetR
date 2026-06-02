# Mean concentration vs RSD scatter (advanced_plots)

Per-metabolite scatter: x = mean concentration (log scale), y = RSD,
coloured by class, with warn/fail horizontal reference lines. Mirrors
the Shiny `results_conc_vs_rsd`.

## Usage

``` r
re_plot_conc_vs_rsd(
  data,
  rsd_values,
  class_map = NULL,
  warn_thr = 20,
  fail_thr = 30
)
```

## Arguments

- data:

  Data frame of samples x metabolites.

- rsd_values:

  Named numeric vector of metabolite -\> RSD%.

- class_map:

  Optional named character; metabolite -\> class.

- warn_thr, fail_thr:

  Numeric thresholds for reference lines.

## Value

`list(static, interactive)`, or `NULL`.
