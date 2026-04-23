# SIL Internal Standard Filter

Filters SIL internal standards based on missing values in the
`master_list` data.

## Usage

``` r
qcCheckR_sil_IntStd_filter(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with SIL internal standard filter flags.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_sil_IntStd_filter(master_list)
} # }
```
