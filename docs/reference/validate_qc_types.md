# Validate QC Types

This function validates the sample types in the run order against the
provided sample tags.

## Usage

``` r
validate_qc_types(run_order, sample_tags)
```

## Arguments

- run_order:

  A data frame containing the run order information.

- sample_tags:

  A character vector of QC types to validate against.

## Value

NULL if validation passes, otherwise throws an error.
