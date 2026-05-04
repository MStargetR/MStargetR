# Create Profile File

This function creates a profile file from the master data. It selects
the sample names and metabolite data, renames columns, and formats the
data into a matrix.

## Usage

``` r
create_profile_file(FUNC_list)
```

## Arguments

- FUNC_list:

  A list containing the master data, metabolite list, and project
  directory.

## Value

The updated `FUNC_list` with the created profile file.
