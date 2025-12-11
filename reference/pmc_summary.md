# Get PubMed Central Summary

Retrieves PubMed Central Summary information for specified publications.
Acts in exactly the same manner as
[`pubmed_summary()`](https://allenbaron.github.io/DO.utils/reference/pubmed_summary.md).
*Consult* *documentation there for more details.*

## Usage

``` r
pmc_summary(input, config = NULL, version = "2.0", retmode = "xml", ...)
```

## Arguments

- input:

  One of the following: 1. A vector with unique PubMed Central IDs. 2. A
  list of vectors with unique PubMed Central IDs. 3. A `web_history`
  object (see NCBI Entrez API documentation for information).

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
