# Fuzzy (Approximate) String Matching

Wraps
[`stringdist::amatch()`](https://rdrr.io/pkg/stringdist/man/amatch.html)
to perform "fuzzy" (approximate) string matching while providing more
informative output. Instead of an integer vector of best match
positions, this function returns a tibble with the input, its
corresponding best match, and the approximate string distance.

## Usage

``` r
match_fz(x, table, method = "lcs", maxDist = 115, ...)
```

## Arguments

- x:

  elements to be approximately matched: will be coerced to `character`
  unless it is a list consisting of `integer` vectors.

- table:

  lookup table for matching. Will be coerced to `character` unless it is
  a list consting of `integer` vectors.

- method:

  Matching algorithm to use. See
  [`stringdist-metrics`](https://rdrr.io/pkg/stringdist/man/stringdist-metrics.html).

- maxDist:

  Elements in `x` will not be matched with elements of `table` if their
  distance is larger than `maxDist`. Note that the maximum distance
  between strings depends on the method: it should always be specified.

- ...:

  arguments passed on to
  [`stringdist::amatch()`](https://rdrr.io/pkg/stringdist/man/amatch.html)

## Value

A tibble with 3 columns:

- `x`

- `table_match`: the closest match of `x`

- `dist`: the distance between x and its closest match (given the method
  selected

## NOTES

Fuzzy string matching is *SLOW*. Expect this function to take \>1 min
for comparisons of more than 500 values for all methods.

For comparison of citation titles specifically, the "lcs" method is
faster than "osa" and seems to work better. Based on light
experimentation, a good setting for `maxDist` value for citation titles
is between 80-115.
