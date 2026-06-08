# Evaluate an expression with a temporarily changed working directory

Internal helper that changes the working directory, evaluates `expr`,
and restores the original directory on exit. Equivalent to
[`withr::with_dir`](https://withr.r-lib.org/reference/with_dir.html) but
implemented without a `withr` dependency at this specific call site so
it can be used safely during package initialisation before withr is
attached.

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

## Details

Note on lazy-eval semantics: `expr` is a promise that is
[`force()`](https://rdrr.io/r/base/force.html)-d *after* the directory
has been changed (and the `on.exit` restore has been registered). The
caller's working directory at call-time is therefore the directory in
which `expr` is evaluated — this is intentional and matches the
behaviour of
[`withr::with_dir`](https://withr.r-lib.org/reference/with_dir.html).
Guarantees the original working directory is restored even if `expr`
errors or the user interrupts.
