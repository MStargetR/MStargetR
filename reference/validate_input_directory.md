# Validate input Directory

This function checks if the `input_directory` parameter is a single
string and if the specified directory exists.

## Usage

``` r
validate_input_directory(input_directory)
```

## Arguments

- input_directory:

  A character string representing the path to the project directory.

## Value

TRUE if the validation is successful, otherwise an error is thrown.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_project_directory("input_directory")
} # }
```
