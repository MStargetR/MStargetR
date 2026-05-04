# Adjust QC Means

This function adjusts the means of QC samples in the corrected data by
calculating the correction ratio based on original and corrected means.
It applies the correction ratio to the corrected data and updates the
sample type for QC samples.

## Usage

``` r
adjust_qc_means(FUNC_list, master_list)
```

## Arguments

- FUNC_list:

  A list containing the master data and corrected data.

- master_list:

  A list containing the project details and data.

## Value

The updated `FUNC_list` with adjusted QC means in the corrected data.
