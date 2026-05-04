# Extract Run Order from PeakForgeR Report

This function extracts the run order data from a PeakForgeR report,
filtering by plate ID.

## Usage

``` r
extract_run_order(report, plate_id)
```

## Arguments

- report:

  The PeakForgeR report data frame.

- plate_id:

  The ID of the plate to filter by.

## Value

A data frame containing the sample names, timestamps, and other relevant
information.
