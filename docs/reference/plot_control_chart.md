# Plot Control Chart

This function generates a control chart for a specific metabolite in the
`master_list`. It combines data from peak area, SIL peak area,
concentration, and stat target concentration, and plots the values
against the sample run index.

## Usage

``` r
plot_control_chart(master_list, metabolite, plate_boundaries, annotate_label)
```

## Arguments

- master_list:

  A list containing project details and data.

- metabolite:

  The metabolite to plot in the control chart.

- plate_boundaries:

  A vector of plate boundaries for vertical lines in the plot.

- annotate_label:

  A tibble containing annotations for the plates.

## Value

A `ggplot` object representing the control chart for the specified
metabolite.
