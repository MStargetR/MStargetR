# Read and validate a plate-grouping manifest

Parses an optional user-supplied manifest that maps each raw vendor file
to a plate. This lets labs whose plate membership lives in an instrument
worklist, LIMS export, or filename convention express that mapping
explicitly rather than relying on subfolder layout. Accepts a CSV path
or a pre-read `data.frame` with (case-insensitive) columns `raw_file`
and `plateID` (a `sample_name` column, if present, is currently ignored
and reserved for future renaming support).

## Usage

``` r
read_plate_manifest(manifest, known_files)
```

## Arguments

- manifest:

  Path to a CSV file, or a `data.frame`.

- known_files:

  Character vector of validated vendor file basenames; every `raw_file`
  in the manifest must be present here.

## Value

`data.frame` with columns `raw_file` (basename, with whitespace
normalised to underscores to match the auto-sanitised on-disk filenames)
and `plateID`.
