# Create Pheno File

This function creates a phenotype file from the master data. It selects
relevant columns, renames them, and formats the sample IDs and classes.

## Usage

``` r
create_pheno_file(FUNC_list)
```

## Arguments

- FUNC_list:

  A list containing the master data and project directory.

## Value

The updated `FUNC_list` with the created phenotype file.
