# mst_groups_from_plateIDs

Backward-compatibility shim: builds a grouping table from a character
vector of plate IDs by exact-matching one vendor file per plate in
`<input_directory>/raw_data` (the pre-grouping contract). Internal
callers now pass a groups data.frame directly; this exists so helpers
that historically accepted a plateID vector keep working.

## Usage

``` r
mst_groups_from_plateIDs(input_directory, plateIDs)
```

## Arguments

- input_directory:

  Project directory containing `raw_data/`.

- plateIDs:

  Character vector of (already sanitized) plate IDs.

## Value

A grouping data.frame with the same columns as
[`derive_plate_groups()`](https://mstargetr.github.io/MStargetR/reference/derive_plate_groups.md).
