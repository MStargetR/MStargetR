# process_mrm_transitions

This function processes mrm_transition for each sample on a plate.

## Usage

``` r
process_mrm_transitions(FUNC_mzR, FUNC_mrm_guide, idx_plate, idx_mzML)
```

## Arguments

- FUNC_mzR:

  List from master_list containing mzR object for each sample,
  mzR_header, mzR_chromatogram parsed internally.

- FUNC_mrm_guide:

  Tibble of mrm details parsed internally. See run mrm_template_guide
  for example.

- idx_plate:

  A string naming the current plate ID being processed. Passed from
  process_files function.

- idx_mzML:

  A string naming the current mzML file being processed. Passed from
  process_files function.

## Value

A tibble of mrm data for the specific idx_plate/idx_mzML.

## Examples

``` r
if (FALSE) { # \dontrun{
process_mrm_transitions(FUNC_mzR, FUNC_mrm_guide, idx_plate, idx_mzML)
} # }
```
