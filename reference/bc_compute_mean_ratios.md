# Compute Per-Metabolite Mean-Adjustment Ratios

Computes the per-metabolite ratio of corrected QC mean to original QC
mean. Both inputs are **named numeric vectors indexed by metabolite**
(one value per metabolite, obtained via
[`colMeans()`](https://rdrr.io/r/base/colSums.html) over QC rows); there
is no batch dimension here. The returned ratios are used by
`bc_apply_mean_ratios` to rescale the corrected data column-wise so that
post-correction QC means match the pre-correction scale.

## Usage

``` r
bc_compute_mean_ratios(orig_means, corr_means)
```

## Arguments

- orig_means:

  Named numeric vector of original QC means. Names must be metabolite
  identifiers.

- corr_means:

  Named numeric vector of corrected QC means. Names must match
  `orig_means` (names absent from either are dropped).

## Value

Named numeric vector of correction ratios (`corr_means / orig_means`),
with names limited to metabolites present in both inputs and values
clipped to \\\[10^{-2},\\10^{2}\]\\.

## Details

Ratios are capped at \\10^{\pm 2}\\ (i.e. a 100x inflation or
deflation). Ratios outside that window almost always indicate a
near-zero mean in one of the two inputs, and would otherwise produce
astronomical corrected values. A
[`warning()`](https://rdrr.io/r/base/warning.html) is emitted listing up
to the first five offending metabolites; the ratios for those
metabolites are clamped to the boundary.
