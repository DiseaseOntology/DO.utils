# Search PubMed

Searches PubMed using Entrez Utilities API via
[`rentrez`](https://docs.ropensci.org/rentrez/reference/rentrez.html).

## Usage

``` r
search_pubmed(
  term,
  config = NULL,
  retmode = "xml",
  use_history = FALSE,
  retmax = NULL,
  ...
)
```

## Arguments

- term:

  the search term (character); see reference or
  [rentrez::entrez_search](https://docs.ropensci.org/rentrez/reference/entrez_search.html)
  for syntax details

- config:

  vector configuration options passed to httr::GET

- retmode:

  character, one of json (default) or xml. This will make no difference
  in most cases.

- use_history:

  logical. If TRUE return a web_history object for use in later calls to
  the NCBI

- retmax:

  maximum number of PubMed IDs to return (integer; default = 20, max =
  100,000); see reference under 'Optional Parameters – Retrieval'

- ...:

  character, additional terms to add to the request, see NCBI
  documentation linked to in references for a complete list

## References

https://www.ncbi.nlm.nih.gov/books/NBK25499/#*chapter4_ESearch*

## See also

rentrez::entrez_search
