# Export All Project Outputs

Exports the `master_list` to XLSX, HTML, and qs2 formats, including
summary tables, QC metrics, and processed data.

## Usage

``` r
qcCheckR_export_all(
  master_list,
  write_rda = TRUE,
  qs_nthreads = max(1L, parallel::detectCores() - 1L),
  qs_compress_level = 3L
)
```

## Arguments

- master_list:

  A list containing project details and data.

- write_rda:

  Logical. When `TRUE` (default) the master_list qs2 file is written
  synchronously as part of the export step. Set to `FALSE` when the
  caller intends to write it out-of-band (e.g. the Shiny GUI fires a
  detached background save so results can render before the qs2 write
  completes). When `FALSE`, callers are responsible for invoking
  [`export_master_list_qs()`](https://mstargetr.github.io/MStargetR/reference/export_master_list_qs.md)
  themselves. The name is retained from the prior `.rda` API to avoid
  churning every caller; the underlying output is now `.qs2`.

- qs_nthreads:

  Forwarded to
  [`export_master_list_qs()`](https://mstargetr.github.io/MStargetR/reference/export_master_list_qs.md);
  see its documentation for the default.

- qs_compress_level:

  Forwarded to
  [`export_master_list_qs()`](https://mstargetr.github.io/MStargetR/reference/export_master_list_qs.md);
  see its documentation for the default.

## Value

The updated `master_list` with exported files.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_export_all(master_list)
} # }
```
