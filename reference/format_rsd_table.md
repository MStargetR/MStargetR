# Format RSD Table

This function formats the RSD table from the `master_list` filters. It
rounds the RSD values, adds a data column, and transposes the table for
better readability.

## Usage

``` r
format_rsd_table(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

A tibble containing the formatted RSD table with rounded values and
transposed structure.
