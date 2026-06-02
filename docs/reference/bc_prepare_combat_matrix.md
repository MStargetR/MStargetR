# Prepare Feature Matrix for ComBat

Transposes the metabolite columns into a features-by-samples matrix,
imputes NAs with row medians, and removes zero-variance features.

## Usage

``` r
bc_prepare_combat_matrix(data, metabolite_cols)
```

## Arguments

- data:

  Data.frame with metabolite value columns.

- metabolite_cols:

  Character vector of metabolite column names.

## Value

A list with elements `dat_combat` (the cleaned matrix), `zero_var`
(logical vector indicating which features had zero variance), and
`kept_features` (character vector of retained feature names).
