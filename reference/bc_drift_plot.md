# Signal-drift plot for a single metabolite (advanced_plots)

Recreates the GUI's Signal Drift panel
(`output$batch_plot_before/after`) as both a static ggplot and an
interactive plotly. Each point is a sample coloured by type, with a
dashed LOESS line fitted to QC samples to visualise drift.

## Usage

``` r
bc_drift_plot(df, met, title_prefix, qc_label = "qc")
```

## Arguments

- df:

  Data frame containing run-order, sample-type and `met` cols.

- met:

  Character. Metabolite column to plot.

- title_prefix:

  Character. Prepended to the title (e.g. "Before").

- qc_label:

  Character. Label identifying QC samples (default "qc").

## Value

`list(static, interactive)`, or `NULL` on missing columns.
