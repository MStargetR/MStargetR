# Save Batch Correction Results to a Project Directory

Writes corrected data and correction summary CSVs into a
`batch_correction` subfolder of the specified project directory.

## Usage

``` r
bc_save_to_project(result, project_dir)
```

## Arguments

- result:

  The result list from `batchCorrectR`.

- project_dir:

  Character. Path to the project directory.

## Value

Invisibly returns the output directory path.
