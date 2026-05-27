# Plot Run Order

This function generates a run order plot for PCA scores using `ggplot2`.
It plots the PCA scores against the sample run index, with vertical
lines indicating plate boundaries and annotations for plate IDs.

## Usage

``` r
plot_run_order(scores, pc, boundaries, plot_settings)
```

## Arguments

- scores:

  A tibble containing PCA scores and sample information.

- pc:

  The principal component to plot (e.g., "PC1", "PC2", "PC3").

- boundaries:

  A vector of boundaries for the plates.

- plot_settings:

  A list containing plot settings such as colors, shapes, and sizes.

## Value

A `plotly` object (the inner ggplot is exposed separately via
[`plot_run_order_static()`](https://mstargetr.github.io/MStargetR/reference/plot_run_order_static.md)
so the same figure can be written to disk as a static PDF for R users).
