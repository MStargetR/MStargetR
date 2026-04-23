# Parse sample_timestamp to POSIXct

Canonical parser for the `sample_timestamp` column. Used by both the
standalone `batchCorrectR` pipeline and `qcCheckR::extract_run_order` so
both paths produce aligned POSIXct output.

## Usage

``` r
parse_sample_timestamp(x)
```

## Arguments

- x:

  Character, factor, or POSIXct vector of timestamps.

## Value

A POSIXct vector the same length as `x` (or `x` itself when every value
fails to parse).

## Details

POSIXct input is returned unchanged. Character/factor input is parsed
against a tryFormats list covering ISO 8601, slash/dash day-first and
month-first formats, AM/PM variants, long month names, and date-only
forms. If every value fails to parse, the original vector is returned
with a warning so callers can decide how to handle it.
