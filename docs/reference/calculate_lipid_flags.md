# Calculate lipid flags for a batch

This function calculates flags for lipids based on their signal
intensity and missing values in a specific batch. It checks for SIL
internal standards, counts peak areas below a threshold, and flags
plates with excessive missing values.

## Usage

``` r
calculate_lipid_flags(master_list, idx_batch, lipid_matrix)
```

## Arguments

- master_list:

  A list containing project details and data.

- idx_batch:

  The index of the batch (plate) to process.

- lipid_matrix:

  A matrix containing lipid data for the batch.

## Value

A tibble containing lipid flags, including counts of peak areas below a
threshold, missing values, and flags for excessive missing values.
