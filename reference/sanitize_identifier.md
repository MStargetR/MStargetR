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

The exact allowed character set after sanitization is:
`[A-Za-z0-9_.()@ -]` (alphanumeric, underscore, hyphen, dot,
parentheses, at-sign, and ASCII space). Any character outside this set
is replaced with an underscore. Path traversal sequences (`..`) and path
separators (`/`, `\`) are rejected outright rather than silently
stripped. Callers that pass the result to a Windows filesystem API
should be aware that Windows silently strips trailing spaces and that
parentheses may require quoting in some shell contexts; always use
`system2(args = vector)` rather than string interpolation when passing
sanitized values to external commands.
