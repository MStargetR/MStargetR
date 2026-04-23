# Validate Project Directory

This function checks if the `project_directory` parameter is a single
string and if the specified directory exists.

## Usage

``` r
validate_project_directory(project_directory)
```

## Arguments

- project_directory:

  A character string representing the path to the project directory.

## Value

The normalized project directory path (invisibly), or throws an error if
validation fails.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_project_directory("path/to/project_directory")
} # }
```
