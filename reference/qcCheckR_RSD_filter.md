# RSD Filter

Filters features per plate with a %RSD \> 30% based on the precision of
measurement in the `master_list` data. Applies filtering to peakArea,
concentration, and statTarget concentration data sources.

## Usage

``` r
qcCheckR_RSD_filter(master_list)
```

## Arguments

- master_list:

  Master list from previous functions.

## Value

The updated `master_list` with RSD filter flags.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_RSD_filter(master_list)
} # }
```
