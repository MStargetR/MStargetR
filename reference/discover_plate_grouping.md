# discover_plate_grouping

Infers plate membership for a set of one-file-per-sample vendor files by
detecting which positional filename token partitions the files into
plate-like groups. Filenames are tokenised on `[-_.]` (the same
separators used by
[`extract_sample_id()`](https://mstargetr.github.io/MStargetR/reference/extract_sample_id.md));
the token position whose distinct values best cluster the files - more
than one value, fewer than one per file, and grouping into plural,
balanced sets - is taken as the plate identifier.

## Usage

``` r
discover_plate_grouping(file_name)
```

## Arguments

- file_name:

  Character vector of vendor file basenames. Pass only sample-level
  files; plate-level formats (`.wiff`/`.wiff2`) are one-plate-per-file
  and must not be routed through here.

## Value

A list with `plateID` (per-file plate token, same length/order as
`file_name`, or `NULL` when nothing could be inferred), `position`
(chosen token index or `NA`), `n_plates`, `ambiguous` (`TRUE` when the
inference is not confident), and a human-readable `reason`.

## Details

This is the "auto-discover" rung of
[`derive_plate_groups()`](https://mstargetr.github.io/MStargetR/reference/derive_plate_groups.md):
it removes the need for a manifest or per-plate subfolders when the
plate is already encoded in the filename, *without* a lab-specific
pattern. When no token groups the files but they share a constant
prefix, the files are assumed to form a single plate named by that
prefix (the common "one plate per run" case). Both fallbacks set
`ambiguous = TRUE` so callers report the inference for confirmation
rather than trusting it silently.
