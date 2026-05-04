# Compare MRM Template Internal Standards Against Concentration Guide

Checks that every stable isotope-labelled (SIL) internal standard listed
in the `Note` column of an MRM transition template has a corresponding
entry in the concentration guide. Unmatched standards will cause
failures during response-concentration calculations in qcCheckR.

## Usage

``` r
compare_mrm_template_with_guide(mrm_template, concentration_guide)
```

## Arguments

- mrm_template:

  A data.frame of MRM transitions. Must contain a `Note` column whose
  non-NA values identify SIL internal standards.

- concentration_guide:

  A data.frame of SIL internal standard concentrations. Must contain a
  `SIL_name` column.

## Value

If all internal standards match, returns `NULL` invisibly and prints a
success message. Otherwise, returns a character vector of unmatched
`Note` values from the `mrm_template` that need to be corrected.

## Examples

``` r
if (FALSE) { # \dontrun{
compare_mrm_template_with_guide(mrm_template, concentration_guide)
} # }
```
