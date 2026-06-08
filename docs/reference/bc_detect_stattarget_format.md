# Detect statTarget Output Format and Filter/Rename

The raw output from statTarget can appear in several column layouts.
This helper detects the format, filters out header rows, and ensures the
first column is called `name` with the remaining columns converted to
numeric.

## Usage

``` r
bc_detect_stattarget_format(data)
```

## Arguments

- data:

  A tibble or data.frame of raw statTarget output.

## Value

A cleaned tibble with a `name` column and numeric value columns.
