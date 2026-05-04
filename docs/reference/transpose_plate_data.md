# Transpose and Clean Plate Data

This function transposes the plate data from the PeakForgeR report,
reshaping it into a wide format. It cleans the sample names by removing
the file extension and converts area values to numeric.

## Usage

``` r
transpose_plate_data(data)
```

## Arguments

- data:

  The PeakForgeR report data frame for a specific plate.

## Value

A transposed tibble with sample names as rows and molecule names as
columns.
