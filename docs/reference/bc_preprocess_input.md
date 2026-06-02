# Preprocess batchCorrectR Input

Accepts a single data.frame or a list of data.frames, combines them, and
maps column names to the canonical internal format expected by the
pipeline.

## Usage

``` r
bc_preprocess_input(data, batch_column = NULL)
```

## Arguments

- data:

  A data.frame, tibble, or list of data.frames/tibbles.

- batch_column:

  Optional character. Name of the column in `data` that holds the batch
  identifier. When `NULL` (default) the function auto-detects `batch` or
  `sample_plate_id` (in that order). When supplied, that column's values
  are copied into the canonical `batch` column for the rest of the
  pipeline. Use this to drive the correction off any user-named column
  (e.g. `plate`, `run_batch`).

## Value

A single data.frame in canonical format with columns: `sample_name`,
`batch`, `sample_type`, `run_order`, plus metabolite columns.
