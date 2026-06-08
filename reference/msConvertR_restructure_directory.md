# msConvertR_restructure_directory

Relocates each plate's raw vendor files (and any `.wiff.scan`
companions) into `<plateID>/data/raw_data`, using the explicit plate
membership recorded in the grouping table rather than substring
matching. mzML files are written directly into `<plateID>/data/mzml` by
msconvert; this function only reports their count.

## Usage

``` r
msConvertR_restructure_directory(
  output_directory,
  groups,
  vendor_extension_patterns = MSTARGETR_VENDOR_EXT_PATTERN
)
```

## Arguments

- output_directory:

  Output directory where the plate folders live.

- groups:

  Membership table from
  [`derive_plate_groups()`](https://mstargetr.github.io/MStargetR/reference/derive_plate_groups.md).

## Value

`invisible(NULL)`. Called for its side effects.

## Examples

``` r
if (FALSE) { # \dontrun{
msConvertR_restructure_directory(output_directory, groups)
} # }
```
