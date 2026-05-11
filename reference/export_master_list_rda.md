# Export Master List as RDA File

Exports the `master_list` to an RDA file under
`<project_dir>/all/data/rda/`, suitable for re-loading or sharing.

## Usage

``` r
export_master_list_rda(master_list, rda_compress = FALSE)
```

## Arguments

- master_list:

  A list containing project details and data.

- rda_compress:

  Passed straight through to
  [`save()`](https://rdrr.io/r/base/save.html)'s `compress` argument.
  Default is `FALSE` (no compression). On large cohorts (~50+ plates)
  R's single-threaded gzip pass over the serialized master_list can take
  hours and was producing an apparent hang in the R workflow on 54-plate
  cohorts; an uncompressed save completes in seconds-to-minutes at the
  cost of a 5-10x larger file on disk. Set to `"gzip"`, `"bzip2"`, or
  `"xz"` to opt into compression for archival runs.

## Value

The updated `master_list` with the RDA file exported.

## Details

Exported (rather than internal) so that the Shiny GUI can fire this as a
detached background job after
[`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
returns (`qcCheckR(..., write_rda = FALSE)` followed by a separate
[`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html) running
this function), letting users view results immediately while the slow
save continues in the background.
