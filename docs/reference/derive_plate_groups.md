# derive_plate_groups

Resolves which samples belong to which plate, producing a single
membership table consumed by every downstream msConvertR helper.
Resolution priority:

1.  **Manifest** (`manifest=`) — explicit `raw_file -> plateID` mapping;
    overrides everything else.

2.  **Subfolder** — a vendor file in `raw_data/<plateID>/` belongs to
    plate `<plateID>` (the subfolder name).

3.  **Flat** — a vendor file directly in `raw_data/` forms its own plate
    from the filename (legacy behaviour; keeps `.wiff` working
    unchanged, as one `.wiff` is one multi-sample plate).

The returned `source`/`plate_level` columns let the caller apply the
refuse-and-prompt policy for ambiguous flat single-sample inputs.

## Usage

``` r
derive_plate_groups(input_directory, manifest = NULL)
```

## Arguments

- input_directory:

  Project directory containing a `raw_data/` folder.

- manifest:

  Optional CSV path or `data.frame` (see `read_plate_manifest`).

## Value

A `data.frame`, one row per vendor file, with columns `raw_path`,
`file_name`, `rel_dir`, `raw_plateID`, `sanitized_plateID`, `is_dir`,
`plate_level`, `source`.
