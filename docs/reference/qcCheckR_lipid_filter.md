# Lipid Filter

Filters lipids based on missing values across plates and template
versions. Flags lipids with more than 50% missing values and compiles a
list of failed lipids.

## Usage

``` r
qcCheckR_lipid_filter(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with lipid filter flags and failed lipid list.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_lipid_filter(master_list)
} # }
```
