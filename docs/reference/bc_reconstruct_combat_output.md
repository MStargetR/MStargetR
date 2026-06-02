# Reconstruct Data.frame After ComBat Correction

Takes the corrected matrix from
[`sva::ComBat`](https://rdrr.io/pkg/sva/man/ComBat.html) and writes the
corrected values back into the original data.frame.

## Usage

``` r
bc_reconstruct_combat_output(data, corrected_matrix, kept_features)
```

## Arguments

- data:

  The original data.frame (used as a template).

- corrected_matrix:

  The features-by-samples corrected matrix from ComBat.

- kept_features:

  Character vector of feature names that were corrected.

## Value

The data.frame with corrected metabolite values.
