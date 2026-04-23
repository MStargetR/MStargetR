# Evaluate an expression with a temporarily changed working directory

Internal helper that mirrors
[`withr::with_dir`](https://withr.r-lib.org/reference/with_dir.html).
Used because `withr` is in DESCRIPTION Suggests (not Imports), so we
cannot depend on it at call sites. Guarantees the original working
directory is restored even if `expr` errors or the user interrupts.

## Usage

``` r
mstargetr_with_dir(new_dir, expr)
```

## Arguments

- new_dir:

  Target working directory.

- expr:

  Expression to evaluate.

## Value

The value of `expr`.
