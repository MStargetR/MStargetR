# Validate qcCheckR mrm template list

This function validates the mrm_template_list list by checking the
column headers and ensuring there are no NA or NULL values in the
SIL_guide and conc_guide files.

## Usage

``` r
validate_qcCheckR_mrm_template_list(master_list)
```

## Arguments

- master_list:

  A list containing all project details and data

## Value

TRUE if validation passes. Stops execution if validation fails.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_qcCheckR_mrm_template_list(master_list)
} # }
```
