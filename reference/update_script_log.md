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
master_list <- list(
                project_details = list(
                  script_log = list(
                    timestamps = list(
                      start_time = Sys.time()
                    ),
                    runtimes = list(),
                    messages = list()
                   )
                 )
               )

update_script_log(master_list, "section_1", "start_time", "section_2")
} # }
```
