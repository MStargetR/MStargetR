# Assert that the requested container runtime is available.

Assert that the requested container runtime is available.

## Usage

``` r
assert_runtime_available(runtime)
```

## Arguments

- runtime:

  One of `"docker"` or `"apptainer"`. The latter also accepts
  `singularity` as a fallback.

## Value

Invisibly, the executable name (`"docker"`, `"apptainer"`, or
`"singularity"`). Stops with an actionable error if not found.
