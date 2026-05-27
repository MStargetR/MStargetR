# Box plot by sample type (advanced_plots)

Per-sample-type box plot of a single metabolite. Mirrors the Shiny
`results_boxplot` (deep-dive panel). When `metabolite` is NULL the
metabolite with the highest RSD in `rsd_values` is used (matches the
Shiny default selector).

## Usage

``` r
re_plot_boxplot_by_class(data, metabolite = NULL, rsd_values = NULL)
```

## Arguments

- data:

  Data frame of samples x metabolites + sample-type column.

- metabolite:

  Character or NULL.

- rsd_values:

  Optional named numeric for default selection.

## Value

`list(static, interactive)`, or `NULL`.
