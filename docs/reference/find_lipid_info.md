# find_lipid_info

This function finds and matches lipids to mrm data.

## Usage

``` r
find_lipid_info(
  FUNC_mrm_guide,
  precursor_mz,
  product_mz,
  mzml_rt_apex,
  FUNC_mzR,
  idx_plate,
  idx_mzML,
  idx_mrm
)
```

## Arguments

- FUNC_mrm_guide:

  Tibble of mrm details parsed internally. See run mrm_template_guide
  for example.

- precursor_mz:

  A numeric value for precursor mass to charge ratio.

- product_mz:

  A numeric value for product mass to charge ratio.

- mzml_rt_apex:

  A numeric value for retention time apex.

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

A list contain lipid class and lipid species information.

## Examples

``` r
if (FALSE) { # \dontrun{
find_lipid_info(FUNC_mrm_guide, precursor_mz, product_mz,
                mzml_rt_apex, FUNC_mzR, idx_plate, idx_mzML, idx_mrm)
} # }
```
