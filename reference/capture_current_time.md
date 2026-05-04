# Capture Current Time

This function captures the current time for the given section.

## Usage

``` r
capture_current_time(master_list, section_name, overwrite = FALSE)
```

## Arguments

- master_list:

  A list containing project details and script log information.

- section_name:

  A string representing the name of the current section.

- overwrite:

  Logical. If `FALSE` (the default) a warning is emitted when a
  timestamp already exists for `section_name`. Set to `TRUE` to
  overwrite silently.

## Value

updated master list
