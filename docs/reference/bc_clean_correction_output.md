# Clean statTarget Correction Output

Clean statTarget Correction Output

## Usage

``` r
bc_clean_correction_output(corrected_raw, metabolite_map, pheno)
```

## Arguments

- corrected_raw:

  Tibble of raw statTarget output.

- metabolite_map:

  Tibble mapping metabolite codes to original names.

- pheno:

  Tibble with sample ID to sample_name mapping.

## Value

Tibble with sample_name and corrected metabolite columns.
