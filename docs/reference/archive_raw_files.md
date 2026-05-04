# Archive Raw Files

This function moves raw files (wiff and mzML) to an archive directory
after processing is complete.

## Usage

``` r
archive_raw_files(project_directory)
```

## Arguments

- project_directory:

  Path to the directory for the project parsed from PeakForgeR.

## Value

None. The function performs the archive operation and a message upon
successful completion.

## Examples

``` r
if (FALSE) { # \dontrun{
archive_raw_files("path/to/project_directory")
} # }
```
