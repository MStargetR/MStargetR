# Strip .mzML suffix from sample_name

qcCheckR strips the `.mzML` extension from `FileName` when it builds
`sample_name` (see `extract_run_order` in `R/qcCheckR_dataprep.R`). The
standalone batchCorrectR pipeline must apply the same normalisation so
that output `sample_name` values align regardless of which pipeline was
used.

## Usage

``` r
strip_mzml_suffix(x)
```

## Arguments

- x:

  Character vector of sample names.

## Value

Character vector with a trailing `.mzML` removed.
