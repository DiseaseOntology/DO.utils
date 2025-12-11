# Get PubMed Summary

Retrieves PubMed Summary information for specified publications. Acts as
a wrapper around
[rentrez::entrez_summary](https://docs.ropensci.org/rentrez/reference/entrez_summary.html)
to allow a larger number of IDs as input (using
[rentrez::entrez_post](https://docs.ropensci.org/rentrez/reference/entrez_post.html))
and for input as an ID list (in addition to an `id` vector or
`web_history` object).

## Usage

``` r
pubmed_summary(input, config = NULL, version = "2.0", retmode = "xml", ...)
```

## Arguments

- input:

  One of the following: 1) A vector with unique PubMed IDs, 2) a list of
  vectors with PubMed IDs (for example, output from
  `citedby_pmid(.... by_id = TRUE)` \>
  [`extract_pmid()`](https://allenbaron.github.io/DO.utils/reference/extract_pmid.md)),
  OR 3) a `web_history` object (see NCBI Entrez API documentation for
  information).

- config:

  vector configuration options passed to
  [`httr::GET`](https://httr.r-lib.org/reference/GET.html)

- version:

  Argument included here for flexibility, but best left with the
  defaults as set; see
  [`rentrez::entrez_summary()`](https://docs.ropensci.org/rentrez/reference/entrez_summary.html)
  for details.

- retmode:

  "xml" (default) or "json"; "xml" is preferred because of a higher
  response limit. This default is opposite the
  [`rentrez::entrez_summary()`](https://docs.ropensci.org/rentrez/reference/entrez_summary.html)
  default.

- ...:

  character Additional terms to add to the request, see NCBI
  documentation linked to in references for a complete list
