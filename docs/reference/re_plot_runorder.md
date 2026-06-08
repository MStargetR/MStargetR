# Run-order scatter for a single metabolite (advanced_plots)

Sample-type-coloured scatter of a metabolite vs run order (or injection
order). Mirrors the Shiny `results_runorder` / `results_deep_before` /
`results_deep_after` (depending on which data frame is passed).

## Usage

``` r
re_plot_runorder(
  data,
  metabolite = NULL,
  title_prefix = "Run Order",
  rsd_values = NULL
)
```

## Arguments

- data:

  Data frame.

- metabolite:

  Character or NULL (defaults to highest-RSD metabolite).

- title_prefix:

  Character (e.g. "Run Order" or "Before Correction").

- rsd_values:

  Optional, used to pick the default metabolite.

## Value

`list(static, interactive)`, or `NULL`.
