# Transpose PeakForgeR Report Data

This function transposes PeakForgeR report data for each plate in the
master list. It reshapes the data, cleans sample names, converts values
to numeric, and stores the result.

## Usage

``` r
qcCheckR_transpose_data(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

The updated `master_list` object with transposed peak area data.

## Examples

``` r
if (FALSE) { # \dontrun{
# master_list must first be built by qcCheckR_setup_project():
# master_list <- qcCheckR_setup_project(user_name, project_directory,
#                                        mrm_template_list, QC_sample_label,
#                                        sample_tags, mv_threshold)
master_list <- qcCheckR_transpose_data(master_list)
} # }
```
