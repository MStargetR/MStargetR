# Log Error to File

This function logs error messages to a file named `error_log.txt`.

## Usage

``` r
log_error(error_message, plateID, project_directory = getwd())
```

## Arguments

- error_message:

  A character string representing the error message to be logged.

- plateID:

  A character string identifying the plate.

- project_directory:

  A character string for the project directory path. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

## Value

None. The function writes the error message to the log file.

## Examples

``` r
if (FALSE) { # \dontrun{
log_error("An error occurred while processing the data.")
} # }
```
