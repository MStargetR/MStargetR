# read_mrm_guides

This function reads MRM guides from user-supplied paths in the
mrm_template_list. Capable of reading .tsv or .csv files

## Usage

``` r
read_mrm_guides(master_list, mrm_template_list)
```

## Arguments

- master_list:

  The master list object.

- mrm_template_list:

  List of MRM guide file paths.

## Value

The updated `master_list` with each validated MRM guide stored under
`master_list$templates$mrm_guides[[version]]$mrm_guide`, where `version`
is the basename of the corresponding template file.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- read_mrm_guides(master_list, mrm_template_list)
} # }
```
