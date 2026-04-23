# Assess QC Distribution Within Batches

Evaluates QC coverage within each batch without moving any samples.
Reports distribution quality and identifies batches needing synthetic
boundary QCs.

## Usage

``` r
bc_assess_qc_distribution(pheno)
```

## Arguments

- pheno:

  Data.frame with at least columns `batch` and `class`.

## Value

A list with `assessment` (per-batch summary), `needs_leading` and
`needs_trailing` (character vectors of batch IDs).
