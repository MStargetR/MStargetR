# optimise_retention_times

This function optimises retention times for each plate in the master
list using the mzR_mrm_findR function and updates the MRM guide.

## Usage

``` r
optimise_retention_times(master_list, plate_idx)
```

## Arguments

- master_list:

  A list containing project details and data.

- plate_idx:

  A vector of plate indices to optimise retention times for.

## Value

A list containing the optimised retention times and updated MRM guide
for each plate.

## Examples

``` r
if (FALSE) { # \dontrun{
optimise_retention_times(master_list, plate_idx)
} # }
```
