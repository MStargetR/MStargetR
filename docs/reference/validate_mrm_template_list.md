# Validate MRM Template List

This function checks if the `mrm_template_list` parameter is valid and
contains required columns.

## Usage

``` r
validate_mrm_template_list(mrm_template_list, user_name)
```

## Arguments

- mrm_template_list:

  A list of character strings or a named list of data frames
  representing MRM templates.

- user_name:

  A character string identifying the user.

## Value

NULL or an ANPC mrm_template_list if mrm_template_list is NULL and
user_name is ANPC

## Examples

``` r
if (FALSE) { # \dontrun{
validate_mrm_template_list(list("path/to/template1.csv", "path/to/template2.csv"), "user")
} # }
```
