# Prepare ProfileFile for statTarget

Prepare ProfileFile for statTarget

## Usage

``` r
bc_prepare_profile_file(data, metabolite_cols, pheno, st_dir)
```

## Arguments

- data:

  Data.frame with flagged sample types.

- metabolite_cols:

  Character vector of metabolite column names.

- pheno:

  Tibble returned by `bc_prepare_pheno_file`.

- st_dir:

  Path to statTarget working directory.

## Value

List with `metabolite_map` tibble.
