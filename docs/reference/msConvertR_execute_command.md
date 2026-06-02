# msConvertR_execute_command

This function executes the command to convert files to mzML format.

## Usage

``` r
msConvertR_execute_command(
  commands,
  output_directory,
  plateIDs,
  enable_HPC = getOption("MStargetR.enable_HPC", FALSE)
)
```

## Arguments

- commands:

  List of lists, each containing `docker_args` (character vector passed
  to `system2`) and `saneID` (sanitized plate ID).

- output_directory:

  Character string. Root output directory where a `MStargetR_logs`
  sub-directory will be created.

- plateIDs:

  Character vector of sanitized plate IDs corresponding to each element
  of `commands`.

## Value

Named logical list; `TRUE` for each plate that converted successfully,
`FALSE` otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
msConvertR_execute_command(commands, output_directory, plateIDs)
} # }
```
