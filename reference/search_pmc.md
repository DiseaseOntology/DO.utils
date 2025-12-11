# Search PubMed Central

Searches PubMed Central using Entrez Utilities API via
[`rentrez`](https://docs.ropensci.org/rentrez/reference/rentrez.html).

## Usage

``` r
search_pmc(
  term,
  config = NULL,
  retmode = "xml",
  use_history = FALSE,
  pmid = FALSE,
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

- pmid:

  whether to return PMID as well as PMCID (default = FALSE); when TRUE,
  PMIDs are obtained via
  [rcrossref::id_converter](https://docs.ropensci.org/rcrossref/reference/id_converter.html))

- retmax:

  maximum number of PubMed IDs to return (integer; default = 20, max =
  100,000); see reference under 'Optional Parameters – Retrieval'

- ...:

  character, additional terms to add to the request, see NCBI
  documentation linked to in references for a complete list
