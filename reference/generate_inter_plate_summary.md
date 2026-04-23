# Generate Inter-Plate Summary

This function generates a summary of all plates in the `master_list`. It
aggregates data across all plates, including metrics such as matrix
type, sample counts, lipid targets, SIL versions, missing value filter
flags, and RSD percentages.

## Usage

``` r
generate_inter_plate_summary(master_list, metrics, sample_tags)
```

## Arguments

- master_list:

  A list containing project details and data.

- metrics:

  tibble of the metrics to be included in the summary.

- sample_tags:

  A vector of sample type tags to be included in the summary.

## Value

A tibble containing the inter-plate summary metrics.
