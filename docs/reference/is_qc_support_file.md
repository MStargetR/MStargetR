# Test whether a filename belongs to an ANPC QC / support file

Wraps `MSTARGETR_QC_SUPPORT_PATTERN` so the predicate can be exercised
in isolation without constructing full paths.

## Usage

``` r
is_qc_support_file(fname)
```

## Arguments

- fname:

  Character vector of file names (basename or full path).

## Value

Logical vector; `TRUE` for each element that matches.
