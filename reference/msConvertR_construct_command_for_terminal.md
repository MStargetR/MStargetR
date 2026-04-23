# msConvertR_construct_commands_for_terminal

This function constructs the command for terminal to convert files to
mzML format.

## Usage

``` r
msConvertR_construct_command_for_terminal(
  input_directory,
  output_directory,
  plateIDs
)
```

## Arguments

- input_directory:

  path to input directory containing vendor files

- output_directory:

  path to output directory.

- plateIDs:

  The names of vendor files to convert

## Value

The constructed command string.

## Examples

``` r
if (FALSE) { # \dontrun{
command <- msConvertR_construct_command_for_terminal(path/to/input/directory,
                                                     "path/to/output_directory")
} # }
```
