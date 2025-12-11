# Tidy Google Analytics Tables (INTERNAL)

Tidies Google Analytics tables loaded with
[`read_ga()`](https://allenbaron.github.io/DO.utils/reference/read_ga.md).

## Usage

``` r
tidy_ga_tbl(ga_tbl, keep_total = FALSE)
```

## Arguments

- ga_tbl:

  A Google analytics table loaded with
  [`read_ga()`](https://allenbaron.github.io/DO.utils/reference/read_ga.md),
  that needs to be tidied.

- keep_total:

  Whether to keep the row with totals that appears at the bottom of the
  table, as a logical scalar (default: `FALSE`).
