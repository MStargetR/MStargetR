# Apply Per-Metabolite Mean-Adjustment Ratios

Divides each metabolite column in `data` by the corresponding ratio,
skipping metabolites whose ratio is `NA`, zero, or non-finite. `ratios`
is a **named numeric vector indexed by metabolite** (one value per
metabolite column in `data`); there is no batch dimension. Names of
`ratios` that do not appear as columns of `data` are silently skipped.

## Usage

``` r
bc_apply_mean_ratios(data, ratios)
```

## Arguments

- data:

  Data.frame or tibble to adjust. Metabolite columns are identified by
  name-matching against `names(ratios)`.

- ratios:

  Named numeric vector of correction ratios, as returned by
  `bc_compute_mean_ratios`.

## Value

The adjusted data.frame.
