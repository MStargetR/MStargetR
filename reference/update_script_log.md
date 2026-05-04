# Update Script Log

This function updates the script log in the `master_list` object by
capturing the current time, calculating the runtime for the current
section, and creating a message for the log.

## Usage

``` r
update_script_log(
  master_list,
  section_name,
  previous_section_name,
  next_section_name
)
```

## Arguments

- master_list:

  A list containing project details and script log information.

- section_name:

  A string representing the name of the current section.

- previous_section_name:

  A string representing the name of the previous section.

- next_section_name:

  A string representing the name of the next section.

## Value

The updated `master_list` object with the new log information.

## Examples

``` r
if (FALSE) { # \dontrun{
# Build a minimal master_list with start_time already recorded
master_list <- list(
  project_details = list(
    script_log = list(
      timestamps = list(start_time = Sys.time()),
      runtimes   = list(),
      messages   = list()
    )
  )
)
# Record the end of "section_1" and prepare the log entry for "section_2"
master_list <- update_script_log(master_list,
                                  section_name          = "section_1",
                                  previous_section_name = "start_time",
                                  next_section_name     = "section_2")
} # }
```
