# Set QC Type for Filtering

Determines and sets the QC type for filtering based on the global QC
pass status in the `master_list`. If no viable QC type is found, the
function stops execution and prints a detailed error message.

## Usage

``` r
qcCheckR_set_qc(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with the QC type set and filters initialized.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_set_qc(master_list)
} # }
```
