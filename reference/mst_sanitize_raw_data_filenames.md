# Auto-rename vendor files/folders whose names contain whitespace

ProteoWizard's `msconvert` is run inside a container against
bind-mounted vendor data; the file basename is passed as a command
argument (`/data/<name>`), so a blank space in the name is mis-parsed
and the file cannot be located. This pre-flight pass renames any
offending entry in `raw_data/` on disk, replacing whitespace with
underscores, so the physical name matches the sanitised command
argument.

## Usage

``` r
mst_sanitize_raw_data_filenames(input_directory)
```

## Arguments

- input_directory:

  Project directory containing a `raw_data/` folder.

## Value

Invisibly, a data.frame of performed renames (columns `from`, `to`, full
paths); empty when nothing needed renaming. A no-op (empty result, no
message) when `raw_data/` is absent.

## Details

Traversal mirrors
[`validate_file_types()`](https://mstargetr.github.io/MStargetR/reference/validate_file_types.md):
the top level of `raw_data/` plus one level into plate-container
subfolders (a directory whose name does *not* match a vendor extension).
Vendor directories (e.g. `.d`) are renamed as whole units but never
descended into - their internal filenames are meaningful to the vendor
format and never reach the command. `.wiff`/`.wiff.scan` companion pairs
are renamed consistently because the whole basename is transformed.

Renames are computed per directory and validated before any are applied:
if a sanitised target already exists, or two distinct entries would
collapse to the same name, the function stops without touching the
(irreplaceable) raw files.
