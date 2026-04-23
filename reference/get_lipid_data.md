# Extract lipid matrix for a batch

This function extracts the lipid data matrix for a specific batch from
the `master_list`. It filters out samples with missing values and
selects relevant columns.

## Usage

``` r
get_lipid_data(master_list, idx_batch)
```

## Arguments

- master_list:

  A list containing project details and data.

- idx_batch:

  The index of the batch (plate) to process.
