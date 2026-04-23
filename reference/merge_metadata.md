# Merge_Metadata

This function merges metadata from the original data with the imputed
data. It selects sample name columns from the original data and performs
a left join with the imputed data. It also adds a column indicating the
data source of the imputed data.

## Usage

``` r
merge_metadata(original_data, imputed_data)
```

## Arguments

- original_data:

  A tibble containing the original peak area data with sample names.

- imputed_data:

  A tibble containing the imputed peak area data with sample names.

## Value

A tibble with merged metadata and imputed data, including a sample data
source column.
