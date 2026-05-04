# Calculate SIL Flags per Version

This function calculates flags for SIL internal standards across
different template versions. It aggregates SIL flags from all plates for
each version, counts missing values, and flags versions with excessive
missing values.

## Usage

``` r
calculate_sil_flags_per_version(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with SIL flags calculated for each version.
