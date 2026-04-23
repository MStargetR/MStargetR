# Run statTarget shiftCor

This function runs the
[`statTarget::shiftCor`](https://rdrr.io/pkg/statTarget/man/shiftCor.html)
function to perform signal drift correction on the prepared phenotype
and profile files. It specifies the correction parameters and reads the
corrected data from the output file.

## Usage

``` r
run_statTarget_shiftCor(FUNC_list, master_list)
```

## Arguments

- FUNC_list:

  A list containing the project directory, phenotype file, and profile
  file.

- master_list:

  A list containing all project details and data.

## Value

The updated `FUNC_list` with corrected data and adjusted QC means.
