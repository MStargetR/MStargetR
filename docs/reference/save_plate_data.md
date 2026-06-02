# save_plate_data

This function saves the master list data for a given plate to a qs2
file.

## Usage

``` r
save_plate_data(master_list, plate_idx)
```

## Arguments

- master_list:

  A list containing project details and data.

- plate_idx:

  The index of the plate to save the data for.

## Value

Saves the master list data to a `.qs2` file (load with
[`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html)) in the
specified project directory.

## Examples

``` r
if (FALSE) { # \dontrun{
save_plate_data(master_list, plate_idx)
} # }
```
