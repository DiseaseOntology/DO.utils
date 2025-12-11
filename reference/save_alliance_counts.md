# Writes Alliance Counts to File

Writes Alliance record counts to a .csv file with the version info of
the Alliance file it came from as a footer.

## Usage

``` r
save_alliance_counts(counts_tbl, file, ...)
```

## Arguments

- counts_tbl:

  record counts as `alliance_tbl` (e.g. output from
  [`count_alliance_records()`](https://allenbaron.github.io/DO.utils/reference/count_alliance_records.md))

- file:

  file to write to

- ...:

  arguments to pass on to
  [`readr::write_csv()`](https://readr.tidyverse.org/reference/write_delim.html)

## Value

Returns the `counts_tbl` with its version info footer invisibly.

## See also

Other Alliance functions:
[`count_alliance_records()`](https://allenbaron.github.io/DO.utils/reference/count_alliance_records.md),
[`download_alliance_tsv()`](https://allenbaron.github.io/DO.utils/reference/download_alliance_tsv.md),
[`read_alliance()`](https://allenbaron.github.io/DO.utils/reference/read_alliance.md)
