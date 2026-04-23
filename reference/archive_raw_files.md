# PeakForgeR_archive.R

File archiving functions for moving raw files and processed data to
archive directories after processing is complete. Archive Raw Files

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

## Details

This function moves raw files (wiff and mzML) to an archive directory
after processing is complete.

## Examples

``` r
if (FALSE) { # \dontrun{
archive_raw_files("path/to/project_directory")
} # }
```
