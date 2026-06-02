# Export batchCorrectR HTML Report

Renders an interactive HTML report replicating the GUI Results Explorer,
including RSD comparison, PCA plots, run-order signal drift, heatmap,
and correction summary tables.

## Usage

``` r
bc_export_html_report(
  result,
  original_data = NULL,
  qc_label = "qc",
  output_file = "batchCorrectR_report.html",
  open = interactive()
)
```

## Arguments

- result:

  The result list returned by
  [`batchCorrectR`](https://mstargetr.github.io/MStargetR/reference/batchCorrectR.md).

- original_data:

  The original (uncorrected) data frame that was passed to
  `batchCorrectR`. Required for before-correction PCA and run-order
  plots. If a list of plates was used, pass the combined data frame.

- qc_label:

  Character. The label identifying QC samples (default `"qc"`).

- output_file:

  Character. Path for the output HTML file. Defaults to
  `"batchCorrectR_report.html"` in the current working directory.

- open:

  Logical. Whether to open the report in the browser after rendering.
  Default `TRUE` in interactive sessions.

## Value

Invisibly returns the path to the rendered HTML file.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- batchCorrectR(my_data, qc_label = "PQC")
bc_export_html_report(result, original_data = my_data, qc_label = "PQC")
} # }
```
