# qcCheckR_set_project_details

This function sets project details in the master list.

## Usage

``` r
qcCheckR_set_project_details(
  master_list,
  user_name,
  project_directory,
  QC_sample_label,
  sample_tags,
  mv_threshold,
  lod_threshold = 5000
)
```

## Arguments

- master_list:

  The master list object.

- user_name:

  Character string representing the user name for the project.

- project_directory:

  Directory path for the project folder.

- QC_sample_label:

  Key for filtering QC samples from sample list.

- sample_tags:

  Character vector of sample tags to filter sample types from
  file_names.

- mv_threshold:

  Numeric value for the missing value sample threshold.

- lod_threshold:

  Numeric instrumental limit of detection (peak area) below which values
  are counted as missing. Default is 5000.

## Value

The updated master list object with project details.

## Examples

``` r
if (FALSE) { # \dontrun{
master_list <- qcCheckR_set_project_details(master_list,
                                            user_name,
                                            project_directory,
                                            QC_sample_label,
                                            sample_tags,
                                            mv_threshold,
                                            lod_threshold)
} # }
```
