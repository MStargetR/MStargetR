# copy_files This function copies files from the source directory to the destination directory.

Uses `overwrite = TRUE` so re-archiving into an existing destination
(e.g. a re-run of the same project) succeeds. Transient failures (common
on OneDrive / network drives where the destination holds a brief sync
lock) are retried with a short backoff before stopping.

## Usage

``` r
copy_files(source_dir, dest_dir, max_retries = 3, retry_delay = 0.5)
```

## Arguments

- source_dir:

  Directory path for the folder to copy.

- dest_dir:

  Directory path for the folder to be copied to.

- max_retries:

  Integer number of attempts per file. Default 3.

- retry_delay:

  Seconds to sleep between attempts. Default 0.5.

## Value

A vector of file paths that were copied.

## Examples

``` r
if (FALSE) { # \dontrun{
copy_files(source_dir = "path/to/source", dest_dir = "path/to/destination")
} # }
```
