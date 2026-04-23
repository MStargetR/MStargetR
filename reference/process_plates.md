# process_plates

This function processes mzML files for each plate in the master list,
extracting relevant information and updating the master list.

## Usage

``` r
process_plates(master_list, mzml_filelist)
```

## Arguments

- master_list:

  A list containing project details and data.

- mzml_filelist:

  A list of mzML files for each plate.

## Value

The updated `master_list` object with processed mzML data.

## Examples

``` r
if (FALSE) { # \dontrun{
process_plates(master_list, mzml_filelist)
} # }
```
