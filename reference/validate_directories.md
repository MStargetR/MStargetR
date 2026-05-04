# validate_directories

This function validates the existence of the source directory and
creates the destination directory if it does not exist.

## Usage

``` r
validate_directories(source_dir, dest_dir)
```

## Arguments

- source_dir:

  Directory path for the folder to copy.

- dest_dir:

  Directory path for the folder to be copied to.

## Value

None. The function performs directory validation.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_directories(source_dir = "path/to/source", dest_dir = "path/to/destination")
} # }
```
