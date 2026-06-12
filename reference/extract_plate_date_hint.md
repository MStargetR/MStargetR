# Extract a YYYYMMDD Date Hint From a Plate ID

Many ANPC plate IDs carry the acquisition date in an unambiguous,
locale-independent form – either as a trailing `_YYYYMMDD` suffix (e.g.
`..._BHASp06_20211004`) or, in newer naming, as a leading ISO date
prefix such as `2026-06-12_ABA1HA_...` or `20260612_...`. When the
dmy/mdy heuristics tie across an otherwise-ambiguous cohort, this hint
is used to tip the decision toward whichever calendar convention places
the parsed dates closest to the embedded date. The looser 6-digit
`_DDMMYY` or `_YYMMDD` forms are intentionally not matched, because in
practice they have been observed to be project codes rather than
acquisition dates.

## Usage

``` r
extract_plate_date_hint(plate_id)
```

## Arguments

- plate_id:

  A character scalar plate identifier.

## Value

A length-1 POSIXct (UTC) or `NA` if no usable hint is present.
