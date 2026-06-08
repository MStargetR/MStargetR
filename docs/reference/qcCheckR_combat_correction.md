# ComBat Batch Correction for qcCheckR Pipeline

Applies empirical Bayes batch correction using
[`sva::ComBat`](https://rdrr.io/pkg/sva/man/ComBat.html) within the
qcCheckR pipeline. Unlike statTarget methods, ComBat does not require QC
samples.

## Usage

``` r
qcCheckR_combat_correction(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with corrected data.
