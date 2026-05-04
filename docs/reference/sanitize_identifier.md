# Sanitize an identifier for safe use in file paths and system commands

Removes or replaces characters that could cause path traversal or
command injection. Allows alphanumeric, underscore, hyphen, dot, space,
and parentheses only.

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
