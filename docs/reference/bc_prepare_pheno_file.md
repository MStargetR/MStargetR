# Prepare PhenoFile for statTarget

Prepare PhenoFile for statTarget

## Usage

``` r
bc_prepare_pheno_file(data, qc_label, st_dir)
```

## Arguments

- data:

  Data.frame with flagged sample types.

- qc_label:

  Character identifying QC samples.

- st_dir:

  Path to statTarget working directory.

## Value

Tibble with pheno mapping columns: sample, sample_name, batch, class,
order.
