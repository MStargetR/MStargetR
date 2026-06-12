# Normalise whitespace in a file or directory name

Replaces every run of whitespace in a single name with one underscore.
Used to make vendor filenames safe to pass as msconvert arguments inside
the container (a space in the basename is mis-parsed as an argument
separator) and to keep manifest `raw_file` entries matching the on-disk
names.

## Usage

``` r
mst_sanitize_filename(name)
```

## Arguments

- name:

  Character vector of file/directory *base*names.

## Value

`name` with whitespace runs collapsed to single underscores.
