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
with names limited to metabolites present in both inputs. NA /
non-finite / non-positive ratios are reported (warning) and left as-is;
`bc_apply_mean_ratios` then skips those metabolites.

## Details

The previous implementation clamped ratios outside
\\\[10^{-2},\\10^{2}\]\\ on the assumption that such magnitudes always
indicate a near-zero mean. That assumption is wrong for the QCRFSC
pipeline: `statTarget::REGfit` normalises every value by dividing by the
random-forest fit (`x[i,] / rfP`), so post-correction QC values are
anchored at ~1 by construction regardless of the metabolite's
concentration scale. The corrected/original ratio is therefore
approximately \\1 / \mathrm{concentration}\\, which for a typical lipid
panel routinely spans 0.001 to 1000. The earlier clamp silently
substituted a wrong rescaling factor (\\10^{-2}\\ or \\10^{2}\\) and
mis-scaled the corrected concentrations for any feature whose mean
concentration was further than 100x from 1 – which on real ANPC plates
was 90%+ of features. Only NA / non-finite / non-positive ratios are now
sanitised; legitimate large-magnitude rescaling is allowed to pass
through.
