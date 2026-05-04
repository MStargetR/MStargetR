# run_system_command

This function wraps the system command to run docker skyline allowing
for unit testing. It captures all output (stdout and stderr) and writes
it to a .txt file.

## Usage

``` r
run_system_command(
  PeakForgeR_command,
  output_file,
  expected_output_files = NULL
)
```

## Arguments

- PeakForgeR_command:

  Docker argument vector (character vector) from
  execute_PeakForgeR_command

- output_file:

  optional path to save the command output

- expected_output_files:

  Optional character vector of host-side file paths that Skyline is
  expected to have produced (e.g. the report CSV and sky file). When
  supplied, the function checks that every path exists and has non-zero
  size after the command returns, providing a safety net for Skyline
  crash signatures that change across Skyline builds.

## Examples

``` r
if (FALSE) { # \dontrun{
run_system_command(c("run", "--rm", "image", "wine", "SkylineCmd"), "output.txt")
} # }
```
