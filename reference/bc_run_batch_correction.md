# Run statTarget Batch Correction

Run statTarget Batch Correction

## Usage

``` r
bc_run_batch_correction(st_dir, method, ntree, coCV, Frule, imputeM)
```

## Arguments

- st_dir:

  Path to statTarget working directory.

- method, ntree, coCV, Frule, imputeM:

  See `batchCorrectR`.

## Value

Tibble of raw corrected data from statTarget.
