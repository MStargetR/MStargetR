# Default instrumental limit-of-detection (LOD) threshold

Peak-area values below this threshold are counted as "below LOD"
(missing) when flagging samples and features in
[`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md).
The LOD is instrument- and lab-specific; override it via the
`lod_threshold` argument of
[`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
(stored in `master_list$project_details$lod_threshold`). The filter
functions in `qcCheckR_filter.R` fall back to this constant when no
value is set.

## Usage

``` r
DEFAULT_LOD_THRESHOLD
```
