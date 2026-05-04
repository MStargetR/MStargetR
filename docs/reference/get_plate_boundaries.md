# Get Plate Boundaries

This function retrieves the boundaries for each plate in the
`master_list` data. It calculates the minimum and maximum run indices
for each plate and creates a list of boundaries and labels for plotting.

## Usage

``` r
get_plate_boundaries(master_list)
```

## Arguments

- master_list:

  A list containing project details and PCA scores.

## Value

A vector of unique boundaries for the plates.
