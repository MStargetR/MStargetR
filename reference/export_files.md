# export_files

This function exports various files related to the project for a given
plate, including updated MRM guides, peak boundaries, and default
templates.

## Usage

``` r
export_files(master_list, plate_idx)
```

## Arguments

- master_list:

  A list containing project details and data.

- plate_idx:

  The index of the plate to export files for.

## Value

Exports CSV, SKY, and TSV files to the specified project directory.

## Examples

``` r
if (FALSE) { # \dontrun{
export_files(master_list, plate_idx)
} # }
```
