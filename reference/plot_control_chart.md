# Plot Control Chart

This function generates a control chart for a specific metabolite in the
`master_list`. It combines data from peak area, SIL peak area,
concentration, and stat target concentration, and plots the values
against the sample run index.

## Usage

``` r
plot_control_chart(
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

A `ggplot` object representing the control chart for the specified
metabolite.
