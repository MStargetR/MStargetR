# Make a path safe for use as a Docker bind-mount source on Windows.

Docker on Windows fails with `"invalid reference format"` when the host
side of a `-v` mount contains spaces, because the
`cmd.exe -> docker.exe` command-line handoff loses the quotes that
`system2() / shQuote()` adds. This affects common locations like
OneDrive ("OneDrive - Org Name") and "Program Files".

## Usage

``` r
mst_make_safe_mount_path(host_path, prefix = "mst_mnt_")
```

## Arguments

- host_path:

  Real host filesystem path to be bind-mounted.

- prefix:

  Prefix used for the junction name (passed to
  [`tempfile()`](https://rdrr.io/r/base/tempfile.html)).

## Value

A list with two elements:

- `safe_path`: the path to use in the docker `-v` arg
  (forward-slash-normalised).

- `junction`: the junction path that needs to be unlinked after use, or
  `NULL` if no junction was created.

## Details

On Windows, when `host_path` contains a space, this helper creates an
NTFS junction under a no-spaces temp directory pointing at the original
location and returns the junction path. The caller is responsible for
unlinking the junction (with `recursive = FALSE` so only the reparse
point is removed, not the target's contents).

On non-Windows platforms, or when `host_path` has no spaces, the
original path is returned unchanged and `junction` is `NULL`, so callers
can pass the result through unconditionally.
