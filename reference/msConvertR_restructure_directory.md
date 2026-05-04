# msConvertR_restructure_directory

This function restructures the directory by moving raw_data and mzML
files to correct locations.

## Usage

``` r
msConvertR_restructure_directory(
  output_directory,
  plateIDs,
  vendor_extension_patterns
)
```

## Arguments

- output_directory:

  Output directory where the mzML files will be stored.

- plateIDs:

  filenames for plates being converted with no extension.

- vendor_extension_patterns:

  vector of file extensions for vendor files

## Value

`invisible(NULL)`. Called for its side effects.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- msConvertR_restructure_directory(output_directory,
                                                plateIDs,
                                                vendor_extension_patterns)
} # }
```
