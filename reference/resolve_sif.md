# Resolve the SIF file for the current image tag.

Lookup order:

1.  `getOption("MStargetR.sif_path")`, if set and the file exists.

2.  `tools::R_user_dir("MStargetR", "cache")/mstargetr-pwiz-<tag>.sif`,
    if it exists.

3.  `apptainer pull docker://<image>:<tag>` into the cache, writing
    `mstargetr-pwiz-<tag>.sif`.

## Usage

``` r
resolve_sif()
```

## Value

Absolute path to a SIF file.

## Details

The filename encodes the image tag so a tag bump triggers a one-time
re-pull and earlier SIFs remain on disk for reproducing prior analyses.

If the auto-pull fails (the most likely cause on an HPC compute node is
no outbound network), the error directs the user to pull the SIF on a
login node and set `options(MStargetR.sif_path = "...")`.
