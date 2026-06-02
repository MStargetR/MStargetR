# Extract Run Order from PeakForgeR Report

Builds the per-plate run order frame used downstream by qcCheckR. mzML
`startTimeStamp` headers (ISO 8601, locale-invariant) are preferred on a
per-sample basis when `mzR_entries` is supplied; Skyline's
`AcquiredTime` string is used as a fallback for any sample whose mzML
did not carry a usable header. The string-parse path honours
`date_order` so the locale-dependent dmy/mdy ambiguity is resolved by
the caller's cohort-level decision rather than by silent first-match
inside the parser.

## Usage

``` r
extract_run_order(report, plate_id, mzR_entries = NULL, date_order = "auto")
```

## Arguments

- report:

  The PeakForgeR report data frame.

- plate_id:

  The ID of the plate to filter by.

- mzR_entries:

  Optional named list of mzR entries (typically
  `master_list$data[[plate_id]]$mzR`). Each element should carry an
  `mzR_timestamp` character scalar (an ISO 8601 timestamp from the mzML
  `startTimeStamp` header). Where present and non-empty, this value
  replaces the report's `AcquiredTime` for that sample.

- date_order:

  Forwarded to `parse_sample_timestamp` for the fallback path; see that
  function for accepted values.

## Value

A data frame containing the sample names, timestamps, and other relevant
information.
