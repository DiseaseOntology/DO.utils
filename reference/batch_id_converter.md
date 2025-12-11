# Convert IDs in Batches

Batches inputs to convert any number of IDs using PubMed Centrals ID
converter API, via
[rcrossref::id_converter](https://docs.ropensci.org/rcrossref/reference/id_converter.html).
Input cannot have missing values and *should* be unique.

## Usage

``` r
batch_id_converter(x, type = NULL, ...)
```

## Arguments

- x:

  (character) One or more of: doi, pmid, pmcid, or manuscript id, see
  examples. required.

- type:

  (character) one of doi, pmid, pmcid, or manuscript id

- ...:

  Curl args passed on to
  [crul::verb-GET](https://docs.ropensci.org/crul/reference/verb-GET.html)
