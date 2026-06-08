# QC %RSD bar for a single metabolite (advanced_plots)

Two-bar chart (Before / After) for one metabolite, with the % value
labelled above each bar. Mirrors the GUI's `batch_rsd_plot`. When the
caller does not specify `metabolite`, the metabolite with the highest
pre-correction RSD is used (matches the GUI selector default).

## Usage

``` r
bc_plot_rsd_metabolite(result, metabolite = NULL)
```

## Arguments

- result:

  A
  [`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
  result list.

- metabolite:

  Character or NULL. Metabolite name.

## Value

`list(static, interactive)`, or `NULL`.
