# msConvertR_mzml_conversion

This function converts raw vendor files to mzML format using
ProteoWizard's msconvert tool, restructures directories, and updates the
script log.

## Usage

``` r
msConvertR_mzml_conversion(
  input_directory,
  output_directory,
  plateIDs,
  vendor_extension_patterns,
  sanitized_plateIDs = plateIDs,
  enable_HPC = getOption("MStargetR.enable_HPC", FALSE)
)
```

## Arguments

- input_directory:

  Directory path for project folder

- output_directory:

  Directory path for project folder if different from input directory.

- plateIDs:

  vector of vendor files names to be converted

- vendor_extension_patterns:

  character string of vendor file extensions.

## Value

Converted mzml files.

## Examples

``` r
if (FALSE) { # \dontrun{
msConvertR_mzml_conversion(input_directory, output_directory, plateIDs)
} # }
```
