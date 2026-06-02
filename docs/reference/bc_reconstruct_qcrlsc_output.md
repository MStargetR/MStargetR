# Reconstruct Data.frame After QC-RLSC Correction

Writes the corrected feature columns back into the original data.frame.
Because QC-RLSC preserves the input orientation (rows = samples) there
is **no transpose** (contrast `bc_reconstruct_combat_output`). Dropped
(all-NA / zero-variance) features keep their original values.

## Usage

``` r
bc_reconstruct_qcrlsc_output(data, corrected_matrix, kept_features)
```

## Arguments

- data:

  The original data.frame (used as a template; defines row order).

- corrected_matrix:

  The samples-by-features corrected data.frame from
  [`qcrlscR::qc.rlsc.wrap`](https://rdrr.io/pkg/qcrlscR/man/qc.rlsc.wrap.html),
  in the SAME row order as `data`.

- kept_features:

  Character vector of feature names that were corrected.

## Value

The data.frame with corrected metabolite values.
