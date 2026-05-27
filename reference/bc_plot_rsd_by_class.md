# Median QC %RSD by metabolite class (advanced_plots)

Class-level dumbbell plot: red point = median %RSD before correction,
blue = after. Mirrors the GUI's `batch_rsd_class_plot`.

## Usage

``` r
bc_plot_rsd_by_class(result)
```

## Arguments

- result:

  A
  [`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
  result list.

## Value

`list(static, interactive)`, or `NULL` if the required columns are
absent.
