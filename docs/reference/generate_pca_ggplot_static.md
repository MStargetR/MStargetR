# Build the static PCA ggplot (no plotly wrap)

Underlying ggplot used by
[`generate_pca_ggplot()`](https://mstargetr.github.io/MStargetR/reference/generate_pca_ggplot.md)
and by
[`qcCheckR_collect_plots()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR_collect_plots.md)
when `advanced_plots = TRUE`. Same data and aesthetics; just stops
before
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html) so
the result is a raw ggplot suitable for `ggsave()`.

## Usage

``` r
generate_pca_ggplot_static(master_list, fill_var)
```

## Arguments

- master_list:

  A list containing project details and PCA scores.

- fill_var:

  The variable to color the points by (e.g., "sample_type_factor",
  "sample_plate_id").

## Value

A `ggplot` object.
