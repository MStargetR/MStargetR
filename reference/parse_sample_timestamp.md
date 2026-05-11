# Parse sample_timestamp to POSIXct

Canonical parser for the `sample_timestamp` column. Used by both the
standalone `batchCorrectR` pipeline and `qcCheckR::extract_run_order` so
both paths produce aligned POSIXct output.

## Usage

``` r
parse_sample_timestamp(x, date_order = c("auto", "dmy", "mdy", "ymd", "iso"))
```

## Arguments

- x:

  Character, factor, or POSIXct vector of timestamps.

- date_order:

  One of `"auto"` (default; current heuristic - tries DMY before MDY),
  `"dmy"` (only day-first slash/dash formats), `"mdy"` (only month-first
  slash/dash formats), or `"ymd"` / `"iso"` (only ISO 8601 / year-first
  formats). The qcCheckR pipeline sets this from a cohort-level
  decision; standalone batchCorrectR keeps the default.

## Value

A POSIXct vector the same length as `x` (or `x` itself when every value
fails to parse).

## Details

POSIXct input is returned unchanged. Character/factor input is parsed
against a tryFormats list covering ISO 8601, slash/dash day-first and
month-first formats, AM/PM variants, long month names, and date-only
forms. If every value fails to parse, the original vector is returned
with a warning so callers can decide how to handle it.

Slash-format inputs like `"10/04/2021 12:08:03"` are inherently
locale-dependent: the same mzML files exported by Skyline on different
machines can yield either DMY or MDY strings. When the caller already
knows the format (e.g. it came from a cohort-level detection in
`qcCheckR_sort_data`), it can pass `date_order` to suppress the wrong
family of slash/dash formats so the parser cannot silently pick the
wrong interpretation. ISO 8601 formats are always tried first and are
format-unambiguous regardless of `date_order`.
