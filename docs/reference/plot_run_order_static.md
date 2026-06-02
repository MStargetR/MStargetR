# Build the static run-order ggplot (no plotly wrap)

Underlying ggplot used by
[`plot_run_order()`](https://mstargetr.github.io/MStargetR/reference/plot_run_order.md)
and by
[`qcCheckR_collect_plots()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR_collect_plots.md)
when `advanced_plots = TRUE`.

## Usage

``` r
plot_run_order_static(scores, pc, boundaries, plot_settings)
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

A `ggplot` object.
