# Prepare Imputation Matrix

This function prepares the imputation matrix from the sorted peak area
data. It removes sample name columns, converts the data to a matrix, and
replaces problematic values (zeros, infinite, NaN).

## Usage

``` r
prepare_imputation_matrix(data)
```

## Arguments

- data:

  A tibble containing the sorted peak area data.

## Value

A tibble ready for imputation, with problematic values replaced.
