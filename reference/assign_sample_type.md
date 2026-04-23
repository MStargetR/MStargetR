# Assign Sample Type

This function assigns sample types based on the sample names and
user-defined QC types.

## Usage

``` r
assign_sample_type(sample_tags, run_order)
```

## Arguments

- sample_tags:

  A character vector passed from
  master_list\$project_details\$sample_tags, containing QC types.

- run_order:

  A data frame containing the run order information.
