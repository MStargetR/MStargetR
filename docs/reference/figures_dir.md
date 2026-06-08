# Build (and create) the figures output directory for a module

Returns `<project_dir>/all/figures/<module>`, creating it if it does not
already exist. Mirrors the existing `all/xlsx_report/`,
`all/html_report/`, `all/data/qs2/` convention used by
[`qcCheckR_export_all()`](https://mstargetr.github.io/MStargetR/reference/qcCheckR_export_all.md).

## Usage

``` r
figures_dir(project_dir, module)
```

## Arguments

- project_dir:

  Character. Path to the project directory.

- module:

  Character. One of `"qcCheckR"`, `"batch_corrector"`,
  `"results_explorer"`.

## Value

The figures directory path (invisible).
