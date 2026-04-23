# LGW Impute

This function performs imputation on a tibble by replacing zero values
with half the minimum non-zero value in each column. It calculates the
minimum non-zero value for each column, divides it by 2, and replaces
zero values with this calculated value.

## Usage

``` r
lgw_impute(x)
```

## Arguments

- x:

  A tibble containing numeric data.

## Value

A tibble with zero values replaced by half the minimum non-zero value in
each column.
