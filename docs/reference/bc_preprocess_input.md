# Preprocess batchCorrectR Input

Accepts a single data.frame or a list of data.frames, combines them, and
maps column names to the canonical internal format expected by the
pipeline.

## Usage

``` r
bc_preprocess_input(data)
```

## Arguments

- data:

  A data.frame, tibble, or list of data.frames/tibbles.

## Value

A single data.frame in canonical format with columns: `sample_name`,
`batch`, `sample_type`, `run_order`, plus metabolite columns.
