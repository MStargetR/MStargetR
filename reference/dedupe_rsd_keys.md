# Enforce unique (dataSource, dataBatch) keys on the RSD table

[`format_rsd_table()`](https://mstargetr.github.io/MStargetR/reference/format_rsd_table.md),
[`generate_plate_summary()`](https://mstargetr.github.io/MStargetR/reference/generate_plate_summary.md)
and the Shiny QC tab all key on `paste0(dataSource, ".", dataBatch)`.
Duplicated keys turn into duplicated column names in the exported XLSX
sheet, which dplyr rejects outright. When a key repeats, keep the most
informative row (the one with the most non-NA %RSD values) and warn,
since a duplicate signals an upstream labelling bug.

## Usage

``` r
dedupe_rsd_keys(rsd)
```

## Arguments

- rsd:

  The `master_list$filters$rsd` tibble (already renamed to `dataSource`
  / `dataBatch`).

## Value

`rsd` with one row per (dataSource, dataBatch) pair, in the original row
order.
