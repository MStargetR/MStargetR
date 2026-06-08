# Concentration correlation heatmap (advanced_plots)

Z-scored sample x metabolite heatmap, capped at 200 samples and 100
metabolites to keep file size reasonable (matches Shiny limits). Mirrors
the Shiny `results_heatmap`.

## Usage

``` r
re_plot_heatmap_correlations(
  data,
  metabolites = NULL,
  max_samples = 200,
  max_metabolites = 100
)
```

## Arguments

- data:

  Data frame.

- metabolites:

  Optional character vector. Defaults to all numeric columns (minus
  metadata).

- max_samples, max_metabolites:

  Integer caps.

## Value

`list(static, interactive)`, or `NULL`.
