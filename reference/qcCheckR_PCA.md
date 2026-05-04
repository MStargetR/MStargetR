# PCA Analysis and Plotting

Performs PCA analysis on the `master_list` data and generates PCA plots
using `ggplot2`.

## Usage

``` r
qcCheckR_PCA(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list`. The `pca` slot is populated with:

- `pca$models`:

  Named list of [`ropls::opls`](https://rdrr.io/pkg/ropls/man/opls.html)
  model objects, one per source/key/preprocessed combination (e.g.
  `"peakArea.imputed.raw"`,
  `"concentration.statTargetProcessed.preprocessed"`).

- `pca$scores`:

  Named list of score data frames (one per model), retaining all
  `sample_*` metadata columns alongside PC coordinates.

- `pca$plot`:

  Named list of two `ggplot` objects, keyed by `"sample_type_factor"`
  and `"sample_plate_id"`.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_PCA(master_list)
} # }
```
