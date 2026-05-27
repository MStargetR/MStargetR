# Build the `--bind` / `-v` argument list for a list of binds.

Each bind entry is a list with elements `host`, `container`, and
optional `ro` (logical, default `FALSE`). The flag differs between
runtimes: Docker uses `-v host:container[:ro]`, Apptainer uses
`--bind host:container[:ro]`.

## Usage

``` r
mstargetr_bind_args(binds, runtime)
```
