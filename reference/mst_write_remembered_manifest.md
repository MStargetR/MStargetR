# Persist an inferred plate grouping as an editable manifest (best-effort)

Writes `raw_file,plateID` to `path` so the decision is stable across
re-runs and the user can correct it once. Never overwrites an existing
file and never errors: a write failure is reported and ignored.

## Usage

``` r
mst_write_remembered_manifest(path, raw_file, plateID)
```

## Arguments

- path:

  Destination CSV path (see `mst_remembered_manifest_path`).

- raw_file, plateID:

  Parallel character vectors of the inferred mapping.

## Value

Invisibly `TRUE` if written, `FALSE` otherwise.
