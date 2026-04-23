# Flag Failed QC Injections

This function flags failed QC injections by checking the signal
intensity of QC samples. It identifies QC samples with low signal
intensity and marks them as "sample" in the master data.

## Usage

``` r
flag_failed_qc_injections(FUNC_list)
```

## Arguments

- FUNC_list:

  A list containing the master data and metabolite list.

## Value

The updated `FUNC_list` with flagged failed QC injections.
