# Transpose PeakForgeR Report Data

This function transposes PeakForgeR report data for each plate in the
master list. It reshapes the data, cleans sample names, converts values
to numeric, and stores the result.

## Usage

``` r
qcCheckR_transpose_data(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` object with transposed peak area data.

## Examples

``` r
if (FALSE) { # \dontrun{
qcCheckR_transpose_data(master_list)
} # }
```
