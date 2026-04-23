# PeakForgeR_setup_project

This function sets up the project by initialising the master list,
setting up project directories, and updating the script log.

## Usage

``` r
PeakForgeR_setup_project(
  user_name,
  project_directory,
  plateID,
  mrm_template_list,
  QC_sample_label
)
```

## Arguments

- user_name:

  string identifying user

- project_directory:

  Directory path for the project folder containing the wiff folder and
  wiff files.

- plateID:

  Plate ID for the current plate.

- mrm_template_list:

  List of MRM guides.

- QC_sample_label:

  Key for filtering QC samples from sample list.

## Value

The updated `master_list` object with the project setup details.

## Examples

``` r
if (FALSE) { # \dontrun{
PeakForgeR_setup_project("path/to/project_directory", "plateID",
mrm_template_list, "QC_sample_label")
} # }
```
