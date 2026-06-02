# Populate Synthetic QC Rows With Extrapolated Metabolite Values

Given a tibble that joins a pheno (containing synthetic boundary QC rows
flagged with `synthetic_qc = TRUE`) to a metabolite profile, fills each
synthetic row's metabolite values by linear extrapolation from the real
QCs in the same batch via `bc_estimate_boundary_qc`.

## Usage

``` r
bc_populate_synthetic_qc_values(ordered, metabolite_cols)
```

## Arguments

- ordered:

  Data.frame with columns `sample`, `batch`, `class`, `synthetic_qc`,
  `order`, and one column per metabolite in `metabolite_cols`.

- metabolite_cols:

  Character vector of metabolite column names.

## Value

The same tibble with synthetic rows filled in where possible.

## Details

Without this, synthetic QC rows keep the NA values produced by the
`left_join` from the original data – which biases QCRFSC and, at high
`Frule`, triggers statTarget to drop every feature (producing "subscript
out of bounds" from downstream indexing into an empty matrix).
