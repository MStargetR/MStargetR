# Export All Project Outputs

Exports the `master_list` to XLSX, HTML, and RDA formats, including
summary tables, QC metrics, and processed data.

## Usage

``` r
qcCheckR_export_all(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with exported files.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_export_all(master_list)
} # }
```
