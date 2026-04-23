# Run PCA Model

This function runs a PCA model on the specified data source from the
`master_list`. It preprocesses the data by filtering out failed samples
and high RSD lipids if `preprocessed` is set to TRUE. It then performs
PCA using the `ropls` package and stores the model and scores in the
`master_list`.

## Usage

``` r
run_pca_model(master_list, source, preprocessed = FALSE)
```

## Arguments

- master_list:

  A list containing project details and data.

- source:

  The data source to run PCA on (e.g., "peakArea", "concentration",
  "concentration.statTarget").

- preprocessed:

  Logical indicating whether to preprocess the data (default is FALSE).

## Value

The updated `master_list` with PCA models and scores.
