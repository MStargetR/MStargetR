# Generate Plate Summary

This function generates a summary for a specific plate in the
`master_list`. It includes metrics such as matrix type, sample counts,
lipid targets, SIL versions, missing value filter flags, and RSD
percentages.

## Usage

``` r
generate_plate_summary(master_list, idx_batch, metrics, sample_tags)
```

## Arguments

- master_list:

  A list containing project details and data.

- idx_batch:

  The index of the batch (plate) to process.

- metrics:

  tibble of the metrics to be included in the summary.

- sample_tags:

  A vector of sample type tags to be included in the summary.

## Value

A tibble containing the summary metrics for the specified plate.
