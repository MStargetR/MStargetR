# PeakForgeR_mzml.R

mzML file import, parsing, and chromatogram handling functions. Includes
mzR MRM finding, peak boundary detection, and lipid matching.
import_mzml

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

## Details

This function imports mzML files for each plate using the mzR package,
extracts relevant information, and updates the script log.

## Examples

``` r
if (FALSE) { # \dontrun{
import_mzml("plateID", master_list)
} # }
```
