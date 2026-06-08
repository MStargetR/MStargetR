# Generate Results Explorer plots and summary from R

Recreates every plot the Shiny app's Results Explorer tab renders – RSD
distribution, pass/fail donut, quality-by-class, RSD scatter,
concentration-vs-RSD scatter, per-metabolite box plot, run-order scatter
(before/after when batch correction is supplied), and concentration
heatmap – so they can be inspected from an R script and (optionally)
written to disk.

## Usage

``` r
resultsExplorerR(
  data,
  project_dir = NULL,
  advanced_plots = FALSE,
  warn_thr = 20,
  fail_thr = 30,
  qc_label = "qc",
  class_map = NULL
)
```

## Arguments

- data:

  One of:

  - A data frame of samples (rows) x metabolites (columns) plus metadata
    columns (e.g. `sample_type`, `sample_run_index`).

  - A
    [`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
    master_list (auto-extracts `data$concentration$corrected` and
    `filters$rsd`).

  - A
    [`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
    result (auto-extracts `corrected_data`, `qc_rsd_before`,
    `qc_rsd_after`).

- project_dir:

  Character or NULL. When `advanced_plots = TRUE`, plots are written to
  `file.path(project_dir, "all", "figures", "results_explorer")`.
  Required for disk writes; otherwise ignored.

- advanced_plots:

  Logical. When `TRUE` and `project_dir` is non-null, every plot is
  saved as PDF + HTML. Plots are returned in-memory either way. Default
  `FALSE`.

- warn_thr, fail_thr:

  Numeric thresholds for the RSD-based pass/warning/fail classification.
  Defaults `20` and `30` (mirrors the Shiny app defaults).

- qc_label:

  Character. Sample-type value identifying QC injections, used to
  compute RSDs from a bare data frame. Default `"qc"`
  (case-insensitive).

- class_map:

  Optional named character vector mapping metabolite name -\> class. If
  `NULL`, classes are derived from metabolite names via the same regex
  the GUI uses.

## Value

A list with two elements:

- `plots`:

  Named list of plots; each entry is
  `list(static = <ggplot>, interactive = <plotly>)`.

- `summary`:

  Tibble with one row per metabolite: `metabolite`, `class`, `rsd`,
  `status`.

## Details

Accepts a plain data frame, a
[`qcCheckR()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR.md)
master_list, or a
[`batchCorrectR()`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md)
result; dispatch picks the relevant tables. When `advanced_plots = TRUE`
and `project_dir` is supplied, every plot is written to
`<project_dir>/all/figures/results_explorer/` as both a static `.pdf`
(via
[`ggplot2::ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html))
and an interactive `.html` (via
[`htmlwidgets::saveWidget`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html)).

## Examples

``` r
if (FALSE) { # \dontrun{
  # From a batchCorrectR result, save figures to disk:
  bc <- batchCorrectR(my_data, project_dir = my_project)
  re <- resultsExplorerR(bc, project_dir = my_project,
                          advanced_plots = TRUE)

  # From a plain data frame, return plots in memory only:
  re <- resultsExplorerR(my_data, qc_label = "PQC")
  re$plots$rsd_histogram$static  # ggplot
  re$plots$rsd_histogram$interactive  # plotly
} # }
```
