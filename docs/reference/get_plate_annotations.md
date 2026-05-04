# Get Plate Annotations

This function retrieves the median run index for each plate in the
`master_list` data. It creates a tibble with sample data source, plate
ID, run index, and placeholder values for PCA components and value.

## Usage

``` r
get_plate_annotations(master_list)
```

## Arguments

- master_list:

  A list containing project details and PCA scores.

## Value

A tibble containing plate annotations with median run indices.
