# Get Cited By List from Scopus

List Scopus publications citing those specified by `title`, optionally
split `by_id`.

## Usage

``` r
citedby_scopus(
  title,
  by_id = FALSE,
  id = NULL,
  api_key = NULL,
  insttoken = NULL,
  no_result = "warning",
  view = "STANDARD",
  start = 0,
  count = NULL,
  max_count = 20000,
  headers = NULL,
  wait_time = 0,
  verbose = FALSE,
  ...
)
```

## Arguments

- title:

  Publication title(s) to be used in a Scopus Search API REFTITLE()
  query, as a character vector; see
  https://dev.elsevier.com/sc_search_tips.html.

- by_id:

  Whether to split "cited by" publications by the ID they cite, as a
  boolean (default: `TRUE`); `FALSE` returns a unified list of *unique*
  cited by publications *without* reference to the ID they cite.

- id:

  Unique IDs used to identify `title` input, as a character vector of
  the same length as `title`; *REQUIRED* if `by_id = TRUE`, otherwise
  ignored.

- api_key:

  API Key for Elsevier

- insttoken:

  Elsevier institutional token (*REQUIRED*). See
  [`vignette("obtain_use_records")`](https://allenbaron.github.io/DO.utils/articles/obtain_use_records.md)
  for more details.

- no_result:

  The type of condition that should be signaled when no Scopus results
  exist in a response; one of "error", "warning" (default), "message" or
  "none".

- view:

  type of view to give, see
  <https://api.elsevier.com/documentation/ScopusSearchAPI.wadl>

- start:

  where should the records start gathering

- count:

  number of records to retrieve (below 200 for STANDARD, below 25 for
  COMPLETE views, see <https://dev.elsevier.com/api_key_settings.html>).

- max_count:

  Maximum count of records to be returned.

- headers:

  Additional headers to be added by
  [`httr::add_headers()`](https://httr.r-lib.org/reference/add_headers.html).

- wait_time:

  The time in seconds to wait across consecutive requests of a single
  search (when records \> 25)

- verbose:

  Print diagnostic messages

- ...:

  Named arguments to be passed to
  [`httr::GET()`](https://httr.r-lib.org/reference/GET.html). Available
  arguments are listed in the [Scopus Search Documentation: API
  Specification](https://dev.elsevier.com/documentation/ScopusSearchAPI.wadl).

## Value

If `by_id = FALSE`, the list result from the Scopus Search API (as
produced by
[rscopus::scopus_search](https://rdrr.io/pkg/rscopus/man/scopus_search.html)).
If `by_id = TRUE`, an `id` named list of `scopus_search` result lists.

## See also

Other citedby_functions:
[`citedby_pubmed()`](https://allenbaron.github.io/DO.utils/reference/citedby_pubmed.md)
