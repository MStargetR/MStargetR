# Default RSD threshold used in export and summary functions

Features whose QC RSD (%) meets or exceeds this value are excluded from
the pre-processed concentration outputs. Both
[`filter_concentration()`](https://mstargetr.github.io/MStargetR/reference/filter_concentration.md)
and the display columns in
[`generate_plate_summary()`](https://mstargetr.github.io/MStargetR/reference/generate_plate_summary.md)
reference this constant so the two remain in sync.

## Usage

``` r
DEFAULT_RSD_THRESHOLD
```

## Format

An object of class `integer` of length 1.
