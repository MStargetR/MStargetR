# Determine QC Type

This function determines the QC type based on the global QC pass status.
It checks if there are multiple QC types that have passed. Assesses if a
secondary QC sample is available such as a pooled Quality controls.

## Usage

``` r
determine_qc_type(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

A string indicating the QC type ("pqc", "ltr", or "unknown").
