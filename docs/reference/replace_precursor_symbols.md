# replace_precursor_symbols

This function replaces forward or backwards slashes in 'Precursor Name'
while preserving the original naming convention This is due to skyline
cmd being unable to handle the special character

## Usage

``` r
replace_precursor_symbols(mrm_template, columns = c("Precursor Name", "Note"))
```

## Arguments

- mrm_template:

  dataframe of transitions (mrm_template) for SkylineR or qcCheckR

## Value

Updated mrm_template with special characters replaced in 'Precursor
Name' and 'Note', while the original names are preserved for the columns
in original_col

## Scope of sanitisation

This function only replaces `/` and `\` with underscores to satisfy
Skyline's path parser. It does *not* provide general shell safety. If
any returned value is later interpolated into a shell command, callers
must use `system2(args = vector)` (never string interpolation) or pass
the value through
[`sanitize_identifier()`](https://mstargetr.github.io/MStargetR/reference/sanitize_identifier.md)
first.

## Examples

``` r
if (FALSE) { # \dontrun{
replace_precursor_symbols(mrm_template, columns = c("Precursor Name", "Note"))
} # }
```
