# Impute Missing Data

This function imputes missing and zero values in the `master_list` data
using the minimum intensity of each feature in the batch divided by 2.
It handles infinite and NaN values, applies imputation, and merges
metadata back into the result.

## Usage

``` r
qcCheckR_impute_data(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with imputed peak area data.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_impute_data(master_list)
} # }
```
