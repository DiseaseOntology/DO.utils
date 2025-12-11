# Publication Info

Datasets describing official publications of the Human Disease Ontology
project, grouped by ontology. Data includes publication identifiers from
various sources, along with publication titles and NLM-formatted
citations.

- DO_pubs: Human Disease Ontology publications only

- ST_pubs: Symptom Ontology & Pathogen Transmission Ontology
  publications

## Usage

``` r
DO_pubs

ST_pubs
```

## Format

Each data frame consists of 10 variables:

- internal_id:

  short-version identifier used by the DO team

- pmid:

  PubMed ID

- pmcid:

  PubMed Central ID

- doi:

  DOI

- scopus_eid:

  Scopus Electronic Identification, not the same as a Scopus ID

- lens_id:

  Lens.org ID

- semantic_scholar_id:

  Semantic Scholar Corpus ID

- first_author:

  Publication First Author

- title:

  Publication Title

- citation_nlm:

  Full NLM-formatted citation

`DO_pubs` includes 10 publications.

`ST_pubs` includes 1 publications.

## Source

Compiled by J. Allen Baron; last updated 2022-07-22.
