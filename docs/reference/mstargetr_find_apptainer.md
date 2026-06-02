# Locate the Apptainer/Singularity executable, accepting either name.

Returns the executable name (`"apptainer"` or `"singularity"`) suitable
for passing to [`system2()`](https://rdrr.io/r/base/system2.html).
Returns `""` if neither is on `PATH`.

## Usage

``` r
mstargetr_find_apptainer()
```
