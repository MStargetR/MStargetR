# Sample Filter

Flags samples based on missing values and summed signal intensity in the
`master_list` data. Applies thresholds to identify low-quality samples
and aggregates results across plates.

## Usage

``` r
qcCheckR_sample_filter(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with sample filter flags and failed sample
lists.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_sample_filter(master_list)
} # }
```
