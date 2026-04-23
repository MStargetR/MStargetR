# Harmonise Lipid Columns

This function harmonises the lipid columns across response and
concentration data types in the master list. It ensures that all lipid
subtypes have the same columns by selecting only the common lipids
across all plates.

## Usage

``` r
harmonise_lipid_columns(master_list)
```

## Arguments

- master_list:

  A list containing project details and response/concentration data.

## Value

The updated master list with harmonised lipid columns.
