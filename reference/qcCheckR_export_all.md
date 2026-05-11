# Export All Project Outputs

Exports the `master_list` to XLSX, HTML, and RDA formats, including
summary tables, QC metrics, and processed data.

## Usage

``` r
qcCheckR_export_all(master_list, write_rda = TRUE, rda_compress = FALSE)
```

## Arguments

- master_list:

  A list containing project details and data.

- write_rda:

  Logical. When `TRUE` (default) the master_list RDA file is written
  synchronously as part of the export step. Set to `FALSE` when the
  caller intends to write the RDA out-of-band (e.g. the Shiny GUI fires
  a detached background save so results can render before the slow RDA
  write completes). When `FALSE`, callers are responsible for invoking
  [`export_master_list_rda()`](https://mstargetr.github.io/MStargetR/reference/export_master_list_rda.md)
  themselves.

- rda_compress:

  Forwarded to
  [`export_master_list_rda()`](https://mstargetr.github.io/MStargetR/reference/export_master_list_rda.md);
  see its documentation. Default is `FALSE` (uncompressed save).

## Value

The updated `master_list` with exported files.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_export_all(master_list)
} # }
```
