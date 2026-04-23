# Target Control Charts

Generates control charts for the `master_list` data, showing the values
of target metabolites and SIL internal standards across different sample
types and data sources.

## Usage

``` r
qcCheckR_target_control_charts(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with control charts stored in
`master_list$control_charts`.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_target_control_charts(master_list)
} # }
```
