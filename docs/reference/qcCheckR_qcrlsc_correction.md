# QC-RLSC Batch Correction for qcCheckR Pipeline

Applies Quality Control-based Robust LOESS Signal Correction
([`qcrlscR::qc.rlsc.wrap`](https://rdrr.io/pkg/qcrlscR/man/qc.rlsc.wrap.html);
Dunn et al. 2011) within the qcCheckR pipeline. Like the statTarget
(QCRFSC) path – and unlike ComBat – QC-RLSC requires QC samples and only
the chosen QC type plus biological samples are corrected; blanks,
SIL-only injections, conditioning runs and non-chosen QCs ("other") are
left uncorrected so they do not get a LOESS trend they never informed,
and are re-bound afterwards to keep downstream tables complete.

## Usage

``` r
qcCheckR_qcrlsc_correction(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` with corrected data.
