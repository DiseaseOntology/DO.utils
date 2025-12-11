# Map Terms to Terms of Specified Namespace

Map `term` to terms of a specified namespace (e.g. "DOID") using
[PyOBO](https://github.com/pyobo/pyobo) (and by dependency other
[biopragmatics](https://biopragmatics.github.io/) and [INDRA
labs](https://indralab.github.io/) programs; e.g.
[GILDA](https://github.com/indralab/gilda),
[bioregistry](http://bioregistry.io/)). In INDRA labs terminology, GILDA
grounds (predicts matches of) `term` to `namespace` terms.

## Usage

``` r
pyobo_map(terms, namespace)
```

## Arguments

- terms:

  Term(s) to map, as a character vector.

- namespace:

  Namespace to map terms to (*according to bioregistry*), as a string.

## Value

List of results for each term. See
[`parse_mapping()`](https://allenbaron.github.io/DO.utils/reference/parse_mapping.md)
and dependencies for details on format of results.
