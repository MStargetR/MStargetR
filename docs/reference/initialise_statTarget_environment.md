# Initialise statTarget Environment

This function initialises the environment for `statTarget` batch
correction. It creates necessary directories, sets up the master data,
and flags failed QC injections.

## Usage

``` r
initialise_statTarget_environment(master_list)
```

## Arguments

- master_list:

  A list containing project details and data.

## Value

A list containing the project directory, master data, and metabolite
list for `statTarget`.
