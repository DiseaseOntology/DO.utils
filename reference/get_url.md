# Get URL (internal)

Get a URL used within this package. List all possible names organized by
type with `get_url("names")`.

## Usage

``` r
get_url(.name)
```

## Arguments

- .name:

  Internal name of a desired URL, as a string.

## Possible Names (organized by type)

- pub_urls: doi, github, orcid, pubmed, pmc

- xref_urls: ICD9CM, ICD10CM, KEGG, MESH, NCI, ORDO, MIM, OMIM,
  SNOMEDCT_US, UMLS_CUI

- web: DO_website

- data_urls: alliance_disease_tsv

- ns_prefix: ...includes common OBO Foundry and SPARQL prefixes; use
  `ns_prefix` to see the complete list.

## NOTE

`get_url()` provides prefixes for disease-ontology.org style link
support. The prefixes are primarily for cross-references (`xrefs`) and
are generally better for creating URLs to look up information about a
particular entity online but they *may* overlap with official namespaces
in
[ns_prefix](https://allenbaron.github.io/DO.utils/reference/ns_prefix.md),
which can also be accessed by `get_url()`. Where this occurs, the
prefixes can generally be distinguished by capitalization, with xref
prefixes in uppercase,
e.g. "MESH" = "https://meshb.nlm.nih.gov/record/ui?ui=", and the
official namespace in lowercase,
e.g. "mesh" = "https://id.nlm.nih.gov/mesh/". As this distinction cannot
always be guaranteed, care should be taken for any prefix related to
xrefs in the DO.
