# derive_plate_groups

Resolves which samples belong to which plate, producing a single
membership table consumed by every downstream msConvertR helper.
Resolution priority:

1.  **Manifest** (`manifest=`, or a remembered `plate_grouping.csv` at
    the project root) — explicit `raw_file -> plateID` mapping;
    overrides everything else.

2.  **Subfolder** — a vendor file in `raw_data/<plateID>/` belongs to
    plate `<plateID>` (the subfolder name).

3.  **Filename** — for sample-level vendor files left flat,
    [`discover_plate_grouping()`](https://mstargetr.github.io/MStargetR/reference/discover_plate_grouping.md)
    infers the plate from the filename token structure (no lab-specific
    pattern needed). The inference is reported and, when
    `remember = TRUE`, persisted as an editable `plate_grouping.csv` so
    corrections stick and re-runs are stable.

4.  **Flat** — a single sample-level file, or files filename discovery
    could not group, form their own plate from the filename (keeps
    `.wiff` working unchanged: one `.wiff` is one plate).

The returned `source`/`plate_level` columns let the caller report (and,
for genuinely ungroupable inputs, warn about) the chosen grouping.

## Usage

``` r
derive_plate_groups(input_directory, manifest = NULL, remember = TRUE)
```

## Arguments

- input_directory:

  Project directory containing a `raw_data/` folder.

- manifest:

  Optional CSV path or `data.frame` (see `read_plate_manifest`). When
  `NULL`, a remembered `plate_grouping.csv` at the project root is
  loaded if present.

- remember:

  When `TRUE` (default), an inferred filename grouping is persisted to
  `plate_grouping.csv` for reuse and manual correction.

## Value

A `data.frame`, one row per vendor file, with columns `raw_path`,
`file_name`, `rel_dir`, `raw_plateID`, `sanitized_plateID`, `is_dir`,
`plate_level`, `source`.
