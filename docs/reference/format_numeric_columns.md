# Format Numeric Columns for Export

Rounds numeric columns to 2 d.p. (\>=1) or 3 significant figures (\<1).
Columns whose names match "sample" are left untouched.

## Usage

``` r
format_numeric_columns(df)
```

## Arguments

- df:

  A data frame.

## Value

The data frame with numeric columns formatted.
