# Tee R console output to the per-plate log file

Evaluates `expr` while tee-ing stdout to the plate log file (the same
file that docker/container output already uses), so that R-side
[`message()`](https://rdrr.io/r/base/message.html),
[`warning()`](https://rdrr.io/r/base/warning.html),
[`print()`](https://rdrr.io/r/base/print.html), and
[`cat()`](https://rdrr.io/r/base/cat.html) calls from pipeline entry
points are persisted without being silenced on the console. Both the
console *and* the file receive every line.

## Usage

``` r
mst_with_logging(plateID, project_directory = getwd(), expr)
```

## Arguments

- plateID:

  A single character string identifying the plate (used to derive the
  log file name).

- project_directory:

  A single character string for the project directory. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

- expr:

  An R expression to evaluate. Its value is returned invisibly.

## Value

The value of `expr`, invisibly.

## Details

The log path mirrors the construction used in
[`log_error()`](https://mstargetr.github.io/MStargetR/reference/log_error.md):
`<project_directory>/MStargetR_logs/<plateID>_MStargetR_log.txt`.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- mst_with_logging("plateA", "/path/to/project", {
  message("Processing plate A")
  42L
})
} # }
```
