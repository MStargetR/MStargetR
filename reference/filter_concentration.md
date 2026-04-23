# Filter Concentration Data

This function filters the concentration data from the `master_list`
based on the specified source. It removes failed samples and lipids, and
applies RSD filters based on the specified source.

## Usage

``` r
filter_concentration(master_list, source)
```

## Arguments

- master_list:

  A list containing project details and data.

- source:

  The data source to filter (e.g., "concentration").

## Value

A tibble containing the filtered concentration data.
