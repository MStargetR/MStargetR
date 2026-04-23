# Sanitize an identifier for safe use in file paths

Removes or replaces characters that could cause path traversal (`..`,
`/`, `\\`). Allows alphanumeric, underscore, hyphen, dot, space,
parentheses, and at-sign only.

## Usage

``` r
sanitize_identifier(x, context = "identifier")
```

## Arguments

- x:

  Character string to sanitize.

- context:

  Description of what is being sanitized (for error messages).

## Value

Sanitized character string.

## Details

Note: this helper guards filesystem callers. External command safety
comes from passing arguments as a vector to
[`system2()`](https://rdrr.io/r/base/system2.html) (which never invokes
a shell), not from this sanitizer — spaces and parentheses are
deliberately preserved here so human-readable identifiers round-trip
through filenames.
