# reimport_PeakForgeR_file

This function reimports a PeakForgeR file for a given plate, converts
specific columns to numeric, and cleans the column names.

## Usage

``` r
reimport_PeakForgeR_file(master_list, plate_idx)
```

## Arguments

- master_list:

  A list containing project details and data.

- plate_idx:

  The index of the plate to reimport the Skyline file for.

## Value

A data frame containing the reimported PeakForgeR data with cleaned
column names.

## Examples

``` r
if (FALSE) { # \dontrun{
reimport_PeakForgeR_file(master_list, plate_idx)
} # }
```
