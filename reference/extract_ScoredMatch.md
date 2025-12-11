# Extract ScoredMatch Objects (INTERNAL)

Extracts mapping results from specialized `ScoredMatch` python objects
(as defined by GILDA) AT THE LEVEL of individual mappings.

## Usage

``` r
extract_ScoredMatch(py_ScoredMatch, prefix = NULL, prefix_sep = ":")
```

## Arguments

- py_ScoredMatch:

  A GILDA ScoredMatch (python) object.

- prefix:

  *Optional* prefix to add to namespace local unique identifiers (LUI;
  e.g. 4, the LUI for "disease" in DO), as a string; preferably to
  create a complete namespace ID (e.g. "DOID:4").

- prefix_sep:

  *Optional* separator placed between `prefix` and namespace LUIs, as a
  string. Ignored if `prefix = NULL`.
