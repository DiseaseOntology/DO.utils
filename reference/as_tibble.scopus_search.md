# Convert `scopus_search` Object into Tibble

Converts a `scopus_search` object into a
[tibble](https://tibble.tidyverse.org/reference/tibble.html).

## Usage

``` r
# S3 method for class 'scopus_search'
as_tibble(x, ...)

# S3 method for class 'scopus_search_list'
as_tibble(x, ...)
```

## Arguments

- x:

  A `scopus_search` or `scopus_search_list` object.

- ...:

  Ignored; included for extensibility.

## Value

An untidy tibble. `scopus_search_list` objects will have an additional
`cites` column.

## See also

citedby_scopus
