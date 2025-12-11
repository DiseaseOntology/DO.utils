# Matches Carefully

Wrapper for [`base::match()`](https://rdrr.io/r/base/match.html) that
will NOT match `NA` values. Uses
[`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
to skip `NA` values.

## Usage

``` r
match_carefully(x, table, nomatch = NA_integer_, incomparables = NULL)
```

## Arguments

- x:

  vector or `NULL`: the values to be matched. [Long
  vectors](https://rdrr.io/r/base/LongVectors.html) are supported.

- table:

  vector or `NULL`: the values to be matched against. [Long
  vectors](https://rdrr.io/r/base/LongVectors.html) are not supported.

- nomatch:

  the value to be returned in the case when no match is found. Note that
  it is coerced to `integer`.

- incomparables:

  a vector of values that cannot be matched. Any value in `x` matching a
  value in this vector is assigned the `nomatch` value. For historical
  reasons, `FALSE` is equivalent to `NULL`.
