# Parse Slash/Dash Timestamps Under an Explicit DMY/MDY Order

Element-wise parser used by `detect_cohort_date_order`. For each value
it tries the AM/PM (12-hour) formats *before* the 24-hour formats,
because R's `strptime` ignores trailing characters and would otherwise
let `"%H:%M"` swallow `"6:12:31 PM"` as 06:12 – dropping both the
seconds and the meridiem. Getting the time-of-day right matters for the
per-plate median-vs-hint voting; getting the date part right (the only
thing that distinguishes DMY from MDY) is unchanged.

## Usage

``` r
parse_detection_timestamp(x, order = c("dmy", "mdy"))
```

## Arguments

- x:

  Character vector of timestamps.

- order:

  Either `"dmy"` or `"mdy"`.

## Value

A POSIXct (UTC) vector the same length as `x`; values that match no
format are `NA`.
