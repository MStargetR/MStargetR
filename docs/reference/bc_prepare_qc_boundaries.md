# Prepare Pheno File With Synthetic Boundary QCs

Replaces `bc_reorder_qc_within_batches`. Does NOT move any existing
samples. Instead, assesses QC distribution and inserts synthetic QC rows
at batch boundaries where real QCs are missing.

## Usage

``` r
bc_prepare_qc_boundaries(pheno)
```

## Arguments

- pheno:

  Data.frame with `batch`, `class`, `order` columns.

## Value

A list with `pheno` (data with synthetic rows, flagged with
`synthetic_qc = TRUE`) and `qc_assessment`.
