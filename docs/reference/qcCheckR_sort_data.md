# Sort and QC Check Data

This function sorts the transposed peak area data by run order and
performs QC checks. It assigns sample types, validates QC coverage, and
sets the appropriate QC type for the project.

## Usage

``` r
qcCheckR_sort_data(
  master_list,
  date_order = c("auto", "dmy", "mdy", "ymd", "iso")
)
```

## Arguments

- master_list:

  A list containing project details and data.

- date_order:

  One of `"auto"` (default), `"dmy"`, `"mdy"`, or `"ymd"` / `"iso"`.
  Forwarded to `parse_sample_timestamp` for any sample where mzR
  `startTimeStamp` is unavailable. When `"auto"`, this function inspects
  the cohort and selects an unambiguous order; if every value is
  ambiguous and no plate-name `_YYYYMMDD$` hint can break the tie, it
  errors so the user must pick explicitly rather than silently get the
  wrong dates.

## Value

The updated `master_list` with sorted data and QC check results.

## Details

Before iterating plates, the cohort-wide date format is resolved once so
every plate uses the same calendar convention. mzML `startTimeStamp`
headers (ISO 8601, locale-invariant) are preferred per sample where
available; the `date_order` argument controls how to parse any
Skyline-exported `AcquiredTime` strings that fall back to the string
path.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_sort_data(master_list)
} # }
```
