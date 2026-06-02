# Default ANPC MRM Templates

Returns the default ANPC MRM template list (four versioned file paths
shipped with the package). Callers that need the ANPC default list
should call this function directly rather than relying on the
side-effect branch in
[`validate_mrm_template_list()`](https://mstargetr.github.io/MStargetR/reference/validate_mrm_template_list.md).

## Usage

``` r
default_mrm_templates()
```

## Value

A named list of file paths (v1–v4).
