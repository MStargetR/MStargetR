# Export Master List as RDA File

Exports the `master_list` to an RDA file under
`<project_dir>/all/data/rda/`, suitable for re-loading or sharing.

## Usage

``` r
export_master_list_rda(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with the RDA file exported.

## Details

Exported (rather than internal) so that the Shiny GUI can fire this as a
detached background job after
[`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
returns (`qcCheckR(..., write_rda = FALSE)` followed by a separate
[`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html) running
this function), letting users view results immediately while the slow
compressed save continues in the background.
