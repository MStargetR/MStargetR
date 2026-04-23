# Sort and QC Check Data

This function sorts the transposed peak area data by run order and
performs QC checks. It assigns sample types, validates QC coverage, and
sets the appropriate QC type for the project.

## Usage

``` r
qcCheckR_sort_data(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with sorted data and QC check results.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_sort_data(master_list)
} # }
```
