# Report peak picking failure with informative error message

Called when no version produced a SIL match. Distinguishes between
Skyline failures and missing SIL standards.

## Usage

``` r
report_peak_picking_failure(plate_idx, version_errors)
```

## Arguments

- plate_idx:

  Plate identifier string.

- version_errors:

  Named list of error messages keyed by version.
