# move_folder This function moves a folder from the source directory to the destination directory, waits until files are not in use, and then deletes the source directory.

move_folder This function moves a folder from the source directory to
the destination directory, waits until files are not in use, and then
deletes the source directory.

## Usage

``` r
move_folder(source_dir, dest_dir, max_wait = 60, max_retries = 30)
```

## Arguments

- source_dir:

  Directory path for the folder to copy.

- dest_dir:

  Directory path for the folder to be copied to.

- max_wait:

  Maximum wait time (in seconds) for the system prior to deleting the
  moved folder.

- max_retries:

  Maximum number of attempts to try move/delete prior to error. Default
  30

## Value

None. The function performs the move operation and a message upon
successful completion.

## Examples

``` r
if (FALSE) { # \dontrun{
move_folder(source_dir = "path/to/source",
            dest_dir = "path/to/destination",
            max_wait = 60,
            max_retries = 30)
} # }
```
