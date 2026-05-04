# Integrate Corrected Data into Master List

This function integrates the corrected data from `statTarget` into the
master list. It splits the corrected data by sample plate ID, updates
the sample data source, and processes the peak area and concentration
data.

## Usage

``` r
integrate_corrected_data(master_list, FUNC_list)
```

## Arguments

- master_list:

  A list containing project details and data.

- FUNC_list:

  A list containing the corrected data from `statTarget`.

## Value

The updated `master_list` with integrated corrected data.
