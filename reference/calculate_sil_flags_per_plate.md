# Calculate SIL Flags per Plate

This function calculates flags for SIL internal standards on a per-plate
basis. It computes the number of peak areas below a threshold, counts
missing values, and flags plates with excessive missing values.

## Usage

``` r
calculate_sil_flags_per_plate(master_list, idx_batch)
```

## Arguments

- master_list:

  A list containing project details and data.

- idx_batch:

  The index of the batch (plate) to process.

## Value

A tibble containing SIL flags for each lipid, including counts of peak
areas below a threshold, missing values, and flags for excessive missing
values.
