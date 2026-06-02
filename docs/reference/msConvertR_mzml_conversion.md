# msConvertR_mzml_conversion

This function converts raw vendor files to mzML format using
ProteoWizard's msconvert tool, restructures directories, and updates the
script log.

## Usage

``` r
msConvertR_mzml_conversion(
  input_directory,
  output_directory,
  groups,
  vendor_extension_patterns = MSTARGETR_VENDOR_EXT_PATTERN,
  sanitized_plateIDs = NULL,
  enable_HPC = getOption("MStargetR.enable_HPC", FALSE)
)
```

## Arguments

- input_directory:

  Directory path for project folder

- output_directory:

  Directory path for project folder if different from input directory.

- groups:

  Plate membership table from
  [`derive_plate_groups()`](https://mstargetr.github.io/MStargetR/reference/derive_plate_groups.md).

## Value

Converted mzml files.

## Examples

``` r
if (FALSE) { # \dontrun{
msConvertR_mzml_conversion(input_directory, output_directory, groups)
} # }
```
