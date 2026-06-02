# run_system_command

This function wraps the container invocation that runs Skyline, allowing
for unit testing. It captures all output (stdout and stderr) and writes
it to a .txt file.

## Usage

``` r
run_system_command(
  PeakForgeR_command,
  output_file,
  expected_output_files = NULL,
  enable_HPC = getOption("MStargetR.enable_HPC", FALSE)
)
```

## Arguments

- PeakForgeR_command:

  Docker argument vector returned by
  [`execute_PeakForgeR_command()`](https://mstargetr.github.io/MStargetR/reference/execute_PeakForgeR_command.md).
  The vector carries the structured payload (`image_command`, `binds`)
  as attributes so the dispatcher can route the call to either Docker or
  Apptainer.

- output_file:

  optional path to save the command output

- expected_output_files:

  Optional character vector of host-side file paths that Skyline is
  expected to have produced (e.g. the report CSV and sky file). When
  supplied, the function checks that every path exists and has non-zero
  size after the command returns, providing a safety net for Skyline
  crash signatures that change across Skyline builds.

- enable_HPC:

  Logical. `FALSE` (default) -\> Docker. `TRUE` -\> Apptainer via the
  cached SIF.

## Examples

``` r
if (FALSE) { # \dontrun{
run_system_command(c("run", "--rm", "image", "wine", "SkylineCmd"), "output.txt")
} # }
```
