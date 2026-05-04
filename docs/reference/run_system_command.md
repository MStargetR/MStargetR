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

  developed skyline docker command

- output_file:

  optional path to save the command output

## Examples

``` r
if (FALSE) { # \dontrun{
run_system_command("docker run skyline", "output.txt")
} # }
```
