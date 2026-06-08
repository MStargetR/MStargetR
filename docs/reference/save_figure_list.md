# Save a named list of figures

Iterates a named list, calling
[`save_figure()`](https://mstargetr.github.io/MStargetR/reference/save_figure.md)
for each non-null entry. Names are used as the file basename. NULL
entries are skipped silently (allows callers to build the list
conditionally without [`Filter()`](https://rdrr.io/r/base/funprog.html)
noise).

## Usage

``` r
save_figure_list(plots, project_dir, module, ...)
```

## Arguments

- plots:

  Named list of plots; each entry suitable for
  [`save_figure()`](https://mstargetr.github.io/MStargetR/reference/save_figure.md).

- project_dir, module, ...:

  Forwarded to
  [`save_figure()`](https://mstargetr.github.io/MStargetR/reference/save_figure.md).

## Value

Character vector of path stems (invisible).
