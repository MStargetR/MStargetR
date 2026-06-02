# msConvertR_construct_commands_for_terminal

This function constructs the command for terminal to convert files to
mzML format.

## Usage

``` r
msConvertR_construct_command_for_terminal(
  input_directory,
  output_directory,
  groups,
  enable_HPC = getOption("MStargetR.enable_HPC", FALSE)
)
```

## Arguments

- input_directory:

  path to input directory containing vendor files

- output_directory:

  path to output directory.

- groups:

  Plate membership table from
  [`derive_plate_groups()`](https://mstargetr.github.io/MStargetR/reference/derive_plate_groups.md);
  one command is produced per plate, with one msconvert invocation per
  member.

## Value

A list of per-plate command objects, each with an `invocations` list.
Carries an `active_plateIDs` attribute.

## Examples

``` r
if (FALSE) { # \dontrun{
command <- msConvertR_construct_command_for_terminal(path/to/input/directory,
                                                     "path/to/output_directory")
} # }
```
