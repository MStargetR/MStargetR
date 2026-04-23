# peak_picking

This function processes mzML files for each plate, optimizes retention
times, updates peak boundaries, and checks for SIL internal standards.

## Usage

``` r
peak_picking(plateID, master_list)
```

## Arguments

- plateID:

  Plate ID for the current plate.

- master_list:

  Master list generated internally.

## Value

The updated `master_list` object with peak picking details.

## Examples

``` r
if (FALSE) { # \dontrun{
peak_picking(plateID, master_list)
} # }
```
