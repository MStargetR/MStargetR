# Check MRM Transition List for Unique Q1/Q3 Pairs

Validates that all precursor (Q1) and product (Q3) ion mass-to-charge
ratio pairs in an MRM transition list are unique. Duplicate Q1/Q3
combinations cause ambiguous peak assignments in downstream processing
by PeakForgeR and qcCheckR.

## Usage

``` r
transition_checkR(transition_df, tolerance = 0.001)
```

## Arguments

- transition_df:

  A data.frame containing MRM transitions. Must include the columns
  `"Precursor Mz"` (numeric), `"Product Mz"` (numeric), and
  `"Precursor Name"` (character).

- tolerance:

  Numeric. Pairs whose Precursor Mz and Product Mz both differ by less
  than this value (in Da) are considered duplicates. Defaults to
  `0.001`. Using a tolerance-based comparison avoids false negatives
  caused by R's banker's rounding at half-unit boundaries (e.g. 500.0005
  rounds down at precision 3 while 500.0015 rounds up).

## Value

If all transitions are unique, returns `NULL` invisibly and prints a
success message. If duplicates are found, returns a data.frame of the
non-unique transitions (first six columns) sorted by `Precursor Mz` and
prints a warning message.

## Examples

``` r
if (FALSE) { # \dontrun{
transition_checkR(transition_df)
transition_checkR(transition_df, tolerance = 0.005)
} # }
```
