# Regex that identifies ANPC QC / support files to exclude from mzML counts

Matches ANPC conditioning runs, blanks, and ISTDs files so they are not
counted as converted sample mzML files. Used by
[`msConvertR_restructure_directory()`](https://mstargetr.github.io/MStargetR/reference/msConvertR_restructure_directory.md)
and testable in isolation via the
[`is_qc_support_file()`](https://mstargetr.github.io/MStargetR/reference/is_qc_support_file.md)
helper.

## Usage

``` r
MSTARGETR_QC_SUPPORT_PATTERN
```

## Format

An object of class `character` of length 1.
