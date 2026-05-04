# Assess QC Coverage

This function assesses the QC coverage for each plate in the master
list. It checks the ratio of QC samples to total samples and determines
if the QC passed or failed.

## Usage

``` r
assess_qc_coverage(master_list)
```

## Arguments

- master_list:

  A list containing project details and sorted peak area data.

## Value

The updated master list with QC coverage results.
