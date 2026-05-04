# Batch Correction and Signal Drift Adjustment

This function performs batch correction and signal drift adjustment on
the concentration data in `master_list` using the `statTarget` package.
It prepares phenotype and profile files, runs
[`statTarget::shiftCor`](https://rdrr.io/pkg/statTarget/man/shiftCor.html),
and integrates corrected data back into the master list.

## Usage

``` r
qcCheckR_statTarget_batch_correction(master_list)
```

## Arguments

- master_list:

  A list containing project details, concentration data, and metadata.

## Value

The updated `master_list` with corrected concentration and peak area
data.
