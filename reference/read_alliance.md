# Read Alliance .tsv.gz File

Reads in a .tsv or .tsv.gz file from the Alliance of Genome Resources as
a tibble. It is recommended that Alliance files be downloaded using
[`download_alliance_tsv()`](https://allenbaron.github.io/DO.utils/reference/download_alliance_tsv.md).

## Usage

``` r
read_alliance(alliance_tsv)
```

## Arguments

- alliance_tsv:

  path to Alliance .tsv or .tsv.gz file

## Value

A dataframe.

## See also

Other Alliance functions:
[`count_alliance_records()`](https://allenbaron.github.io/DO.utils/reference/count_alliance_records.md),
[`download_alliance_tsv()`](https://allenbaron.github.io/DO.utils/reference/download_alliance_tsv.md),
[`save_alliance_counts()`](https://allenbaron.github.io/DO.utils/reference/save_alliance_counts.md)
