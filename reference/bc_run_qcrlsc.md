# Run QC-RLSC Batch Correction

Applies
[`qcrlscR::qc.rlsc.wrap`](https://rdrr.io/pkg/qcrlscR/man/qc.rlsc.wrap.html)
(Quality Control-based Robust LOESS Signal Correction; Dunn et al. 2011,
[doi:10.1038/nprot.2011.335](https://doi.org/10.1038/nprot.2011.335) )
to a data.frame that has a `batch` column, a `sample_type` column and
numeric metabolite columns.

## Usage

``` r
bc_run_qcrlsc(
  data,
  metabolite_cols,
  qc_label,
  qcrlsc_method = "subtract",
  intra = FALSE,
  opti = TRUE,
  log10 = TRUE,
  outl = TRUE,
  shift = TRUE
)
```

## Arguments

- data:

  Data.frame with `batch`, `sample_type` and metabolite columns (rows =
  samples, in acquisition order).

- metabolite_cols:

  Character vector of metabolite column names.

- qc_label:

  Character identifying QC samples in `sample_type`.

- qcrlsc_method:

  Scaling method, "subtract" (default) or "divide".

- intra:

  Logical. Intra-batch (TRUE) vs inter-batch (FALSE) correction.

- opti:

  Logical. Optimise the LOESS span by generalised cross-validation.

- log10:

  Logical. Log10-transform before fitting.

- outl:

  Logical. QC outlier detection before fitting.

- shift:

  Logical. Apply `batch.shift` after signal correction.

## Value

Data.frame in the same format and row order as input with corrected
metabolite values.

## Details

Key differences from ComBat: (1) the input orientation already matches
qcrlscR (rows = samples) so no transpose is needed; (2) `qc.rlsc`
re-centres each feature on its QC mean internally, so **no** additional
QC-mean rescaling is layered on top (unlike the QCRFSC path); (3) QC
samples are required because the LOESS trend is fit through them.

Robustness handling: residual missing values are imputed (per-feature QC
median, with an overall feature-median fallback) before the call because
`loess` fails on NA QC responses; any non-finite corrected value (e.g.
from `method = "divide"` when the fitted trend approaches zero) is
reverted to the original input value; and rows are restored to the input
order afterwards (the `intra = TRUE` path of `qc.rlsc.wrap` rbinds
batch-wise and would otherwise reorder them).
