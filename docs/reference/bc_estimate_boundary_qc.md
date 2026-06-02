# Estimate Synthetic Boundary QC Values

For batches missing a QC at the leading or trailing boundary, estimates
QC signal by linear back-extrapolation from the nearest 2-3 real QCs,
shrunk toward the batch QC median to prevent wild extrapolations.

## Usage

``` r
bc_estimate_boundary_qc(qc_values, positions, target_pos, shrinkage = 0.5)
```

## Arguments

- qc_values:

  Numeric vector of QC values for one metabolite.

- positions:

  Integer vector of QC positions in run order.

- target_pos:

  The position to extrapolate to.

- shrinkage:

  Shrinkage factor toward median (default 0.5).

## Value

A single numeric estimated value.
