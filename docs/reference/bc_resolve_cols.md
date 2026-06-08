# Resolve run-order / sample-type column names

qcCheckR-style frames use `sample_run_index` and `sample_type_factor`;
the standalone
[`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
accepts the canonical `run_order` and `sample_type`. This helper hides
the difference so plot constructors work on both shapes.

## Usage

``` r
bc_resolve_cols(df)
```

## Arguments

- df:

  A data frame.

## Value

List with elements `run` and `type` (NULL when no candidate column is
present).
