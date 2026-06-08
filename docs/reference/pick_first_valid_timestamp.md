# pick_first_valid_timestamp

Returns the first non-empty `mzR_timestamp` from the imported mzR
entries, scanning files in the order given by `file_order`. Returns
`NA_character_` if no entry has a usable timestamp, in which case
`extract_acquisition_year` will warn and return `NA_integer_`.

## Usage

``` r
pick_first_valid_timestamp(mzR_entries, file_order)
```

## Arguments

- mzR_entries:

  Named list of mzR import entries (typically
  `master_list$data[[plate]]$mzR`).

- file_order:

  Character vector of mzML filenames in the order to scan.

## Value

Character timestamp, or `NA_character_` if none found.
