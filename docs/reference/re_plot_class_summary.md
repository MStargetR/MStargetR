# Quality-by-class stacked bar (advanced_plots)

Stacked horizontal bar chart: per class, count of pass / warning / fail
metabolites. Mirrors the Shiny `results_class_summary`.

## Usage

``` r
re_plot_class_summary(status_vector, class_map, warn_thr = 20, fail_thr = 30)
```

## Arguments

- status_vector:

  Named character (see
  [`re_plot_passfail_donut()`](https://mstargetr.github.io/MStargetR/reference/re_plot_passfail_donut.md)).

- class_map:

  Named character; metabolite -\> class.

- warn_thr, fail_thr:

  Numeric thresholds, used only for legend labels.

## Value

`list(static, interactive)`, or `NULL`.
