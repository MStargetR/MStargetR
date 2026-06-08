# Validate MRM Template List

This function checks if the `mrm_template_list` parameter is valid and
contains required columns. For ANPC users who pass `NULL`, call
[`default_mrm_templates()`](https://mstargetr.github.io/MStargetR/reference/default_mrm_templates.md)
first to obtain the default list and then pass it here for validation.

## Usage

``` r
validate_mrm_template_list(mrm_template_list, user_name)
```

## Arguments

- mrm_template_list:

  A named list of file paths (character) or data frames representing MRM
  templates.

- user_name:

  A character string identifying the user.

## Value

`invisible(TRUE)` on success; stops with an error on failure. For ANPC
users with `NULL` templates the function returns the default template
list (for backward compatibility with existing callers).

## Examples

``` r
if (FALSE) { # \dontrun{
validate_mrm_template_list(list("path/to/template1.csv", "path/to/template2.csv"), "user")
} # }
```
