# set_project_details

This function sets project details in the master list.

## Usage

``` r
set_project_details(
  master_list,
  user_name,
  project_directory,
  plateID,
  QC_sample_label
)
```

## Arguments

- master_list:

  The master list object.

- user_name:

  string specifying user

- project_directory:

  Directory path for the project folder.

- plateID:

  Plate ID for the current plate.

- QC_sample_label:

  Key for filtering QC samples from sample list.

## Value

The updated master list object with project details.

## Examples
