# wait_until_files_free This function waits until files are not in use by attempting to rename them.

wait_until_files_free This function waits until files are not in use by
attempting to rename them.

## Usage

``` r
wait_until_files_free(files_to_copy, max_wait = 60, max_retries = 30)
```

## Arguments

- files_to_copy:

  A vector of file paths to check.

- max_wait:

  Maximum wait time (in seconds) for the system prior to deleting the
  moved folder. Default is 60

- max_retries:

  Maximum number of attempts to try move/delete prior to error. Default
  is 30.

## Value

None. The function waits until files are free.

## Examples

``` r
if (FALSE) { # \dontrun{
wait_until_files_free(files_to_copy, max_wait = 60, max_retries = 30)
} # }
```
