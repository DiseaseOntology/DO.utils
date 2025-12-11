# Convert `esummary` Object into Tibble

Converts an `esummary` object into a
[tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Usage

``` r
# S3 method for class 'esummary_list'
as_tibble(x, ...)

# S3 method for class 'esummary_list_nested'
as_tibble(x, ...)
```

## Arguments

- x:

  An `esummary` object (`esummary_list` or `esummary_list_nested`)

- ...:

  Ignored; included for extensibility.

## Value

An untidy, where `esummary_id` is the input identifier.
`esummary_list_nested` objects will have an additional `cites` column.

## Note

For single inputs to
[`rentrez::entrez_summary()`](https://docs.ropensci.org/rentrez/reference/entrez_summary.html),
`always_return_list` must be `TRUE`.

## See also

citedby_pubmed, pubmed_summary
