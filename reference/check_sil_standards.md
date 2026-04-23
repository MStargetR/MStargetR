# check_sil_standards

This function checks if the SIL standards on a given plate match the SIL
standards for a specified mrm_guide version.

## Usage

``` r
check_sil_standards(master_list, plate_idx, current_version)
```

## Arguments

- master_list:

  A list containing project details and data.

- plate_idx:

  The index of the plate to check the SIL standards for.

- current_version:

  The version of the MRM guide to compare against.

## Value

A logical value indicating whether the SIL standards on the plate match
the SIL standards for the specified version.

## Examples

``` r
if (FALSE) { # \dontrun{
check_sil_standards(master_list, plate_idx, current_version)
} # }
```
