# mzR_mrm_findR

This function processes mzML files to find peak apex and boundaries
using QC mzR data, updates the mrm guide, and provides peak boundary
information.

## Usage

``` r
mzR_mrm_findR(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
```

## Arguments

- FUNC_mzR:

  List from master_list containing mzR object for each sample,
  mzR_header, mzR_chromatogram parsed internally.

- FUNC_mrm_guide:

  Tibble of mrm details parsed internally. See run mrm_template_guide
  for example.

- FUNC_OPTION_qc_type:

  QC type used in the experiment passed internally.

## Value

A list containing updated mrm guide and peak boundary information.

## Examples

``` r
if (FALSE) { # \dontrun{
mzR_mrm_findR(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
} # }
```
