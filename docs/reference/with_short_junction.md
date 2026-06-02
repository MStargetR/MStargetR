# Evaluate a function with a Windows junction to shorten a long path

On Windows, when `long_path` exceeds 260 characters, a directory
junction is created at a temporary location and `fn(junction_path)` is
called instead. The junction is removed on exit (success or error). On
non-Windows platforms, or when the path is short enough, `fn(long_path)`
is called directly.

## Usage

``` r
with_short_junction(long_path, fn, pattern = "PeakForgeR_short_")
```

## Arguments

- long_path:

  The full, potentially long directory path.

- fn:

  A function that accepts a single path argument and returns a value.

- pattern:

  Prefix passed to [`tempfile()`](https://rdrr.io/r/base/tempfile.html)
  for the junction name.

## Value

The value returned by `fn`.

## Details

Sys.junction() is preferred over `shell(mklink /J)` to avoid
shell-quoting pitfalls. The shell fallback is retained for environments
where Sys.junction is unavailable.
