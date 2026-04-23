# Prepare statTarget Files

This function prepares the phenotype and profile files required for
`statTarget` batch correction. It creates a phenotype file with sample
metadata and a profile file with metabolite data.

## Usage

``` r
prepare_statTarget_files(FUNC_list)
```

## Arguments

- FUNC_list:

  A list containing the project directory, master data, and metabolite
  list.

## Value

The updated `FUNC_list` with created phenotype and profile files.
