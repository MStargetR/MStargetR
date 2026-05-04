# execute_PeakForgeR_command

This function executes a Skyline system command to process mzML files
and generate various reports for a given plate.

## Usage

``` r
execute_PeakForgeR_command(master_list, plate_idx)
```

## Arguments

- master_list:

  A list containing project details and data.

- plate_idx:

  The index of the plate to execute the Skyline command for.

## Value

Executes the Skyline command and generates reports and chromatogram
files saving to project directory.

## Examples

``` r
if (FALSE) { # \dontrun{
execute_PeakForgeR_command(master_list, plate_idx)
} # }
```
