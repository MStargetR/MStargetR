# Generate PCA ggplot

This function generates a PCA plot using `ggplot2` for the PCA scores
stored in the `master_list`. It allows for coloring the points by a
specified variable (e.g., sample type or plate ID).

## Usage

``` r
generate_pca_ggplot(master_list, fill_var)
```

## Arguments

- master_list:

  A list containing project details and PCA scores.

- fill_var:

  The variable to color the points by (e.g., "sample_type_factor",
  "sample_plate_id").

## Value

A `ggplot` object representing the PCA scores.
