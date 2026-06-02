# Save one figure to disk as both PDF and HTML

Accepts a plot in one of three shapes:

- a `list(static = <ggplot>, interactive = <plotly>)` (preferred — both
  formats use their best source);

- a bare `ggplot` object (PDF from ggsave; HTML from `ggplotly()` wrap);

- a bare `plotly` object (HTML only; PDF skipped with a single message).

## Usage

``` r
save_figure(plot, name, project_dir, module, width = 10, height = 7, dpi = 300)
```

## Arguments

- plot:

  A ggplot, plotly, or `list(static, interactive)`.

- name:

  Character. File basename (no extension).

- project_dir, module:

  Forwarded to
  [`figures_dir()`](https://mstargetr.github.io/MStargetR/reference/figures_dir.md).

- width, height:

  Numeric. PDF dimensions (inches). Defaults 10 x 7.

- dpi:

  Numeric. PDF resolution. Default 300.

## Value

Character. The path stem (`<dir>/<name>`) invisibly.

## Details

Returns the paths written (without extension) invisibly.
