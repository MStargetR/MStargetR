# Clean statTarget Output

This function cleans the output data from `statTarget` by filtering out
unwanted rows and renaming columns. It handles different column
structures based on the presence of specific sample columns.

## Usage

``` r
clean_statTarget_output(data)
```

## Arguments

- data:

  A tibble containing the output data from `statTarget`.

## Value

A cleaned tibble with renamed columns and numeric data types.
