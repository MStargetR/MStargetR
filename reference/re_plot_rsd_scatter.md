# RSD scatter: before vs after batch correction (advanced_plots)

One point per metabolite, x = pre-correction RSD, y = post-correction
RSD, coloured by class. y = x diagonal drawn for reference. Mirrors the
Shiny `results_rsd_scatter`.

## Usage

``` r
re_plot_rsd_scatter(qc_rsd_before, qc_rsd_after, class_map = NULL)
```

## Arguments

- qc_rsd_before, qc_rsd_after:

  Named numeric vectors.

- class_map:

  Optional named character; metabolite -\> class.

## Value

`list(static, interactive)`, or `NULL`.
