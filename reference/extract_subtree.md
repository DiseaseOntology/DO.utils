# Extract Subtree

Extracts the classes and parents of a DO subtree from a `pyDOID.owl.xml`
object.

## Usage

``` r
extract_subtree(x, top_node, reload = FALSE)
```

## Arguments

- x:

  A 'pyDOID.owl.xml' object or the path to an OWL/RDF XML file that can
  be instantiated as such an object by
  [`owl_xml()`](https://allenbaron.github.io/DO.utils/reference/owl_xml.md).

- top_node:

  The top node of the tree, as a valid DOID (see
  [`is_valid_doid()`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)
  for valid input formats).

- reload:

  Force reload the file into memory, as `TRUE` or `FALSE` (default).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with the
columns: `id`, `label`, `parent_id`, and `parent_label`, with one row
for each unique combination for each subclass below and including
`top_node`.

## See also

[`format_subtree()`](https://allenbaron.github.io/DO.utils/reference/format_subtree.md)
to arrange data in a tree structure similar to ontology browsers.
