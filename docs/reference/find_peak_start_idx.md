# find_peak_start_idx

This function finds peak start for a single transition.

## Usage

``` r
find_peak_start_idx(
  FUNC_mzR,
  idx_plate,
  idx_mzML,
  idx_mrm,
  peak_apex_idx,
  baseline_value
)
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

- peak_apex_idx:

  Index of peak apex passed from process_files function.

- baseline_value:

  Numeric value indicating transitions baseline. Passed from
  process_files function.

## Value

A numeric value for peak start index.

## Examples

``` r
if (FALSE) { # \dontrun{
find_peak_start_idx(FUNC_mzR, idx_plate, idx_mzML, idx_mrm, peak_apex_idx, baseline_value)
} # }
```
