# validate_mzR_parameters

This function validates parameters mzR_mrmfindR. If parameters fail
validation script stops

## Usage

``` r
validate_mzR_parameters(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
```

## Arguments

- FUNC_mzR:

  List from master_list containing mzR object for each sample,
  mzR_header, mzR_chromatogram parsed internally.

- FUNC_mrm_guide:

  Tibble of mrm details parsed internally. See run mrm_template_guide
  for example.

- FUNC_OPTION_qc_type:

  QC type used in the experiment (LTR, PQC, none) parsed internally.

## Examples

``` r
if (FALSE) { # \dontrun{
validate_mzR_parameters(FUNC_mzR, FUNC_mrm_guide, FUNC_OPTION_qc_type)
} # }
```
