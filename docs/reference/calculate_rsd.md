# Calculate RSD for a given data source and batch list

This function calculates the relative standard deviation (RSD) for each
feature in the provided data batches. It filters out failed samples and
selects only QC samples, then computes the RSD values.

## Usage

``` r
calculate_rsd(master_list, source_name, data_batches)
```

## Arguments

- master_list:

  Master list containing project details and data.

- source_name:

  Name of the data source (e.g., "peakArea", "concentration").

- data_batches:

  A list of data batches to process.
