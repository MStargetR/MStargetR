# Resolve the configured instrumental LOD threshold

Returns `master_list$project_details$lod_threshold` when it is a single
valid numeric, otherwise falls back to `DEFAULT_LOD_THRESHOLD`. Mirrors
the `sil_mv_threshold` / `rsd_threshold` fallback idiom used elsewhere
so every below-LOD count uses the same threshold.

## Usage

``` r
resolve_lod_threshold(master_list)
```

## Arguments

- master_list:

  The master list object.

## Value

A single numeric LOD threshold (peak area).
