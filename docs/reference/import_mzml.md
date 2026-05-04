# import_mzml

This function imports mzML files for each plate using the mzR package,
extracts relevant information, and updates the script log.

## Usage

``` r
import_mzml(plateID, master_list)
```

## Arguments

- plateID:

  Plate ID for the current plate.

- master_list:

  Master list generated internally.

## Value

The updated `master_list` object with the mzML import details.

## Examples

``` r
if (FALSE) { # \dontrun{
import_mzml("plateID", master_list)
} # }
```
