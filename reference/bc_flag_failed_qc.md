# Flag Failed QC Injections

Flag Failed QC Injections

## Usage

``` r
bc_flag_failed_qc(data, qc_label, metabolite_cols)
```

## Arguments

- data:

  A data.frame of sample data.

- qc_label:

  Character identifying QC samples.

- metabolite_cols:

  Character vector of metabolite column names.

## Value

List with `data` (modified) and `failed_samples` (character).
