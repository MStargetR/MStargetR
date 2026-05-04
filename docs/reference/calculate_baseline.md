# calculate_baseline

This function calculates baseline for a single transition.

## Usage

``` r
calculate_baseline(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
```

## Arguments

- FUNC_mzR:

  List from master_list containing mzR object for each sample,
  mzR_header, mzR_chromatogram parsed internally.

- idx_plate:

  A string naming the current plate ID being processed. Passed from
  process_files function.

- idx_mzML:

  A string naming the current mzML file being processed. Passed from
  process_files function.

- idx_mrm:

  A string identifying the current mrm. Passed from process_files
  function.

## Value

A numeric value for baseline.

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_baseline(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
} # }
```
