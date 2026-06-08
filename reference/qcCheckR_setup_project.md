# qcCheckR_setup_project

This function sets up the project by initialising the master list,
setting up project directories, and updating the script log.

## Usage

``` r
qcCheckR_setup_project(
  user_name,
  project_directory,
  mrm_template_list,
  QC_sample_label,
  sample_tags,
  mv_threshold,
  lod_threshold = 5000
)
```

## Arguments

- user_name:

  Character string representing the user name for the project.

- project_directory:

  Directory path for the project folder containing the wiff folder and
  .wiff and .wiff.scan files for each plate.

- mrm_template_list:

  List of lists for mrm_guides.

- QC_sample_label:

  Key for filtering QC samples from sample list.

- sample_tags:

  Vector of character strings to pull sample types for names

- mv_threshold:

  threshold for missing value filter. default is 50%.

- lod_threshold:

  Numeric instrumental limit of detection (peak area) below which values
  are counted as missing. default is 5000.

## Value

The updated `master_list` object with the project setup details.

## Examples

``` r
if (FALSE) { # \dontrun{
qcCheckR_setup_project(
  user_name, project_directory, mrm_template_list,
  QC_sample_label, sample_tags, mv_threshold, lod_threshold
)
} # }
```
