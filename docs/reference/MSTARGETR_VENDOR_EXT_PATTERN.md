# Regex pattern matching all supported vendor-file extensions

Used by
[`msConvertR()`](https://mstargetr.github.io/MStargetR/reference/msConvertR.md)
to strip extensions when deriving plate IDs from file names, and by
[`msConvertR_restructure_directory()`](https://mstargetr.github.io/MStargetR/reference/msConvertR_restructure_directory.md)
when scanning the raw-data folder. Having a single definition prevents
the two call-sites from drifting out of sync.

## Usage

``` r
MSTARGETR_VENDOR_EXT_PATTERN
```
