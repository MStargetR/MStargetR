# Prepare Feature Matrix for QC-RLSC

Builds the samples-by-variables data.frame that
[`qcrlscR::qc.rlsc.wrap`](https://rdrr.io/pkg/qcrlscR/man/qc.rlsc.wrap.html)
expects. Unlike ComBat, QC-RLSC consumes data in the native MStargetR
orientation (rows = samples, columns = features), so **no transpose** is
performed. All-NA and zero-variance features are dropped (a LOESS trend
cannot be fit to them) and reverted to their original values during
reconstruction.

## Usage

``` r
bc_prepare_qcrlsc_matrix(data, metabolite_cols)
```

## Arguments

- data:

  Data.frame with metabolite value columns (rows = samples).

- metabolite_cols:

  Character vector of metabolite column names.

## Value

A list with `dat_qcrlsc` (samples-by-features data.frame of the retained
features), `dropped` (character vector of dropped features), and
`kept_features` (character vector of retained features).
