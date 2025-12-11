# Curation Queue: DOIDs to split?

Create a curation queue of DOIDs that may need to be split into multiple
diseases based on the fact that they have multiple xrefs from the same
source.

## Usage

``` r
queue_xref_split(.DOrepo, src = "all")
```

## Arguments

- .DOrepo:

  The path to the local HumanDiseaseOntology repository or a
  `pyDOID.repo.DOrepo` object created by
  [`DOrepo()`](https://allenbaron.github.io/DO.utils/reference/DOrepo.md).

- src:

  The xref sources to include in the output identified by prefix as a
  character vector or "all" (default) to include everything.
