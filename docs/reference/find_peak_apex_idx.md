# find_peak_apex_idx

This function finds peak apex for a single transition.

## Usage

``` r
find_peak_apex_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
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

A numeric value for peak apex index.

## Examples

``` r
if (FALSE) { # \dontrun{
find_peak_apex_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
} # }
```
