# Export Master List as QS2 File

Exports the `master_list` to a `.qs2` file (qs2 package's multi-threaded
zstd serialization format) under `<project_dir>/all/data/qs2/`, suitable
for re-loading or sharing. Load with
[`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html).

## Usage

``` r
export_master_list_qs(
  master_list,
  qs_nthreads = max(1L, parallel::detectCores() - 1L),
  qs_compress_level = 3L
)
```

## Arguments

- master_list:

  A list containing project details and data.

- qs_nthreads:

  Integer. Worker threads passed to
  [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html). Defaults
  to `max(1L, parallel::detectCores() - 1L)`. Multi-threaded zstd is
  what makes large-cohort saves complete in reasonable time; on a
  54-plate cohort the previous single-threaded gzip pass via
  [`base::save()`](https://rdrr.io/r/base/save.html) stalled for hours.

- qs_compress_level:

  Integer. zstd compression level forwarded to
  [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html). Default
  `3L` (qs2 default; fast with good ratio). Higher values (up to 22)
  shrink the file further at the cost of CPU time; negative values trade
  ratio for more speed.

## Value

The updated `master_list` with the qs2 file exported.

## Details

Exported (rather than internal) so that the Shiny GUI can fire this as a
detached background job after
[`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
returns (`qcCheckR(..., write_rda = FALSE)` followed by a separate
[`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html) running
this function), letting users view results immediately while the save
continues in the background.
