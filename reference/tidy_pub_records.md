# Tidy Publication Records

Tidy publication record results into a tibble. Limited data is retained.
If full data retention is desired, consider using
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
instead (expect list columns and untidy column names).

## Usage

``` r
tidy_pub_records(x, ...)
```

## Arguments

- x:

  A publication record object, as produced by
  [`citedby_pubmed()`](https://allenbaron.github.io/DO.utils/reference/citedby_pubmed.md),
  [`citedby_scopus()`](https://allenbaron.github.io/DO.utils/reference/citedby_scopus.md)
  and
  [`pubmed_summary()`](https://allenbaron.github.io/DO.utils/reference/pubmed_summary.md).

- ...:

  Ignored; included for extensibility.
