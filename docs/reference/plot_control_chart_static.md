# Build the static control-chart ggplot (no plotly wrap)

Underlying ggplot used by
[`plot_control_chart()`](https://mstargetr.github.io/MStargetR/reference/plot_control_chart.md)
and by
[`qcCheckR_collect_plots()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR_collect_plots.md)
when `advanced_plots = TRUE`.

## Usage

``` r
plot_control_chart_static(
  master_list,
  metabolite,
  plate_boundaries,
  area_imp = NULL,
  conc_imp = NULL,
  conc_st = NULL
)
```

## Arguments

- master_list:

  A list containing project details and data.

- metabolite:

  The metabolite to plot in the control chart.

- plate_boundaries:

  A vector of plate boundaries for vertical lines in the plot.

- area_imp:

  Pre-bound peak area imputed tibble (optional; computed internally if
  NULL).

- conc_imp:

  Pre-bound concentration imputed tibble (optional; computed internally
  if NULL).

- conc_st:

  Pre-bound statTargetProcessed concentration tibble (optional; computed
  internally if NULL).

## Value

A `ggplot` object.
