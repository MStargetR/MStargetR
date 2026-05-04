# initialise_mzml_filelist

This function initializes a list of mzML files for each plate in the
master list, excluding files with specific patterns.

## Usage

``` r
initialise_mzml_filelist(master_list)
```

## Arguments

- master_list:

  A list containing project details, including plate IDs and project
  directory.

## Value

A list of mzML files for each plate, excluding files with "COND",
"Blank", or "ISTDs" in their names.

## Examples

``` r
if (FALSE) { # \dontrun{
initialise_mzml_filelist(master_list)
} # }
```
