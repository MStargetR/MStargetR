# Remove junctions created by `mst_make_safe_mount_path()`.

`recursive = FALSE` is critical: `unlink(j, recursive = TRUE)` on a
Windows NTFS junction will recurse INTO the target and delete the user's
real files, which is exactly the disaster the safe-mount path is meant
to avoid. `RemoveDirectory()` (what
[`unlink()`](https://rdrr.io/r/base/unlink.html) ends up calling on a
junction with `recursive = FALSE`) only removes the reparse point.

## Usage

``` r
mst_cleanup_mount_junctions(junctions)
```

## Arguments

- junctions:

  Character vector of junction paths (any length, may include
  `NULL`-equivalents).

## Value

Invisibly, the input.
