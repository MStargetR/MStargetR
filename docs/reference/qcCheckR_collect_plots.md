# Collect all QC plots for advanced_plots = TRUE

Builds the named list of plots written to
`<project_dir>/all/figures/qcCheckR/` when
`qcCheckR(advanced_plots = TRUE)`. Pairs the static ggplot (PDF) with
the interactive plotly (HTML) for every figure the GUI renders,
including per-metabolite control charts.

## Usage

``` r
qcCheckR_collect_plots(master_list, fail_thr = 30)
```

## Arguments

- master_list:

  A qcCheckR master_list (post-pipeline).

- fail_thr:

  Numeric. RSD% threshold annotated on the histogram.

## Value

Named list; each entry is `list(static, interactive)` and the names
become the saved-file basenames.
