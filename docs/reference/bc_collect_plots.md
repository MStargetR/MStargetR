# Collect all batch correction plots for advanced_plots = TRUE

Builds the named list of plots written to
`<project_dir>/all/figures/batch_corrector/` when
`batchCorrectR(advanced_plots = TRUE)`. Combines the existing
before/after ggplots
([`bc_plot_correction_results()`](https://mstargetr.github.io/MStargetR/reference/bc_plot_correction_results.md))
with the GUI-only figures (signal drift, RSD-by-class, per-metabolite
RSD).

## Usage

``` r
bc_collect_plots(result, original_data, qc_label = "qc")
```

## Arguments

- result:

  A
  [`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
  result list.

- original_data:

  Data frame passed as `data` to
  [`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md).

- qc_label:

  Character. QC label used during correction.

## Value

Named list; each entry is `list(static, interactive)` and the names
become the saved-file basenames.
