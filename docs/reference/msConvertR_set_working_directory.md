# msConvertR_set_working_directory

This function sets the working directory to the project directory. The
caller is responsible for restoring the previous wd via
`on.exit(setwd(old_wd))` (see `msConvertR_mzml_conversion`).
withr::with_dir would be preferable but withr is only in DESCRIPTION
Suggests (see REVIEW_REPORT BC-H10).

## Usage

``` r
msConvertR_set_working_directory(directory)
```

## Arguments

- directory:

  Directory path for the project folder.

## Value

None. The function sets the working directory.

## Examples

``` r
if (FALSE) { # \dontrun{
msConvertR_set_working_directory("path/to/output_directory")
} # }
```
