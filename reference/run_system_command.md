# run_system_command

This function wraps the system command to run docker skyline allowing
for unit testing. It captures all output (stdout and stderr) and writes
it to a .txt file.

## Usage

``` r
run_system_command(PeakForgeR_command, output_file)
```

## Arguments

- PeakForgeR_command:

  Docker argument vector (character vector) from
  execute_PeakForgeR_command

- output_file:

  optional path to save the command output

## Examples

``` r
if (FALSE) { # \dontrun{
run_system_command(c("run", "--rm", "image", "wine", "SkylineCmd"), "output.txt")
} # }
```
