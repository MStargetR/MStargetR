# Stop with QC Error

This function stops the script execution and prints a detailed error
message if no viable QC type is found. It includes information about the
global QC pass status and plate QC assessment.

## Usage

``` r
stop_with_qc_error(project_name, global_qc_pass, plate_qc_passed)
```

## Arguments

- project_name:

  A string containing the project name.

- global_qc_pass:

  A list containing the global QC pass status.

- plate_qc_passed:

  A list containing the plate QC assessment.

## Value

Stops the script execution with an error message.
