# create_output

This function updated mrm_guide and peak_boundary_update for later use
in PeakForgeR

## Usage

``` r
create_output(FUNC_tibble, FUNC_mrm_guide, mzML_filelist)
```

## Arguments

- FUNC_tibble:

  A tibble of mrm details per sample per lipid species.

- FUNC_mrm_guide:

  Tibble of mrm details parsed internally. See run mrm_template_guide
  for example.

- mzML_filelist:

  A list containing mzML file information from plate.

## Value

A list containing two tibbles for mrm_guide_updated and
peak_boundary_update.

## Examples

``` r
if (FALSE) { # \dontrun{
create_output(FUNC_tibble, FUNC_mrm_guide, mzML_filelist)
} # }
```
