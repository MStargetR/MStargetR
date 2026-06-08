# Normalise a plot input to a (static, interactive) pair

Accepts a ggplot, a plotly widget, or a `list(static, interactive)`.

## Usage

``` r
normalise_plot_pair(plot)
```

## Arguments

- plot:

  Plot input.

## Value

List with elements `static` (ggplot or NULL) and `interactive` (plotly
or NULL).
