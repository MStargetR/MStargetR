# Detect Cohort-Wide Date Format for AcquiredTime Strings

Skyline's `AcquiredTime` export uses the system locale of whoever ran
the export, so the same mzML files can produce DMY (UK), MDY (US), ISO,
or other formats on different machines. This helper resolves the format
once per cohort so every plate gets a consistent interpretation.

## Usage

``` r
detect_cohort_date_order(master_list)
```

## Arguments

- master_list:

  The qcCheckR master_list. Must have `data$PeakForgeRReport` populated.

## Value

One of `"dmy"`, `"mdy"`, or `"ymd"`. Never returns `"auto"` (the caller
passed that intent in).

## Details

Decision order:

1.  If every non-empty `AcquiredTime` value parses as ISO 8601, return
    `"ymd"` (no further work needed).

2.  Otherwise count cohort-wide DMY vs MDY parse successes; any single
    value with day part \\\>12\\ locks DMY, any with month part \\\>12\\
    locks MDY. The format with the higher parse count wins.

3.  If the counts are tied (every value is digit-ambiguous), use
    per-plate `_YYYYMMDD$` hints to vote between formats by computing
    which interpretation places the median parsed timestamp closer to
    the plate-name date.

4.  If still ambiguous and no hints exist,
    [`stop()`](https://rdrr.io/r/base/stop.html) so the caller is forced
    to supply `date_order` explicitly rather than silently receive the
    wrong dates.
