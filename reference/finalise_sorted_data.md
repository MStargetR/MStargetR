# Finalise Sorted Data

This function finalises the sorted data by adding sample type factors,
reversing the order of sample types, and setting the sample data source.

## Usage

``` r
finalise_sorted_data(master_list)
```

## Arguments

- master_list:

  A list containing project details and sorted peak area data.

## Value

The updated master list with finalised sorted data, including the
additive `sample_class` column described above.

## Details

In addition to the legacy two-value `sample_type` column (`"qc"` /
`"sample"`), a new `sample_class` column is populated with three
mutually-exclusive values:

- `"qc"` — the chosen pooled QC (project-level `qc_type`).

- `"sample"` — biological study samples.

- `"other"` — blanks, SIL/IS injections, conditioning samples, and
  non-chosen QCs (any `sample_tags` value that is not the chosen
  `qc_type`). These must never be treated as biological samples for
  QC-vs-sample statistics (medians, drift models, RSD, etc.).

Downstream code should prefer `sample_class` for QC-vs-sample splits.
`sample_type` is kept populated the old way for backward compatibility
with existing exports and downstream consumers.
