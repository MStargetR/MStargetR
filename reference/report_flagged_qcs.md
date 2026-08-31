# Report QC injections lost to the sample filter

Counts how many of the samples flagged by
[`qcCheckR_sample_filter()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR_sample_filter.md)
are QC injections, and warns per plate when every QC injection on that
plate was flagged. Without QCs,
[`calculate_rsd()`](https://mstargetr.github.io/MStargetR/reference/calculate_rsd.md)
cannot compute %RSD for that plate.

## Usage

``` r
report_flagged_qcs(master_list)
```

## Arguments

- master_list:

  A list containing project details and filter results.

## Value

`invisible(NULL)`, called for its messages / warnings.
