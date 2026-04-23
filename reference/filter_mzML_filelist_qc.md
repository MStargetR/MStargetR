# filter_mzML_filelist_qc

This function filters the mzML_filelist for user selected quality
control samples

## Usage

``` r
filter_mzML_filelist_qc(mzML_filelist, FUNC_OPTION_qc_type)
```

## Arguments

- mzML_filelist:

  A list containing mzML file information from plate.

- FUNC_OPTION_qc_type:

  QC type used in the experiment parsed internally.

## Value

A list containing quality control sample data.

## Examples

``` r
if (FALSE) { # \dontrun{
filter_mzML_filelist_qc(mzML_filelist,FUNC_OPTION_qc_type)
} # }
```
