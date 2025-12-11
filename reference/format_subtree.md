# Format a Subtree

Format a subtree, produced by
[`extract_subtree()`](https://allenbaron.github.io/DO.utils/reference/extract_subtree.md),
as a text-based tree mirroring
[disease-ontology.org](https://disease-ontology.org/).

## Usage

``` r
format_subtree(subtree_df, top_node)
```

## Arguments

- subtree_df:

  A dataframe from
  [`extract_subtree()`](https://allenbaron.github.io/DO.utils/reference/extract_subtree.md).

- top_node:

  The top node of the tree, as a valid DOID (see
  [`is_valid_doid()`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)
  for valid input formats).

## See also

[`extract_subtree()`](https://allenbaron.github.io/DO.utils/reference/extract_subtree.md)

## Examples

``` r
if (FALSE) { # \dontrun{
    do_owl <- {path_to_doid.owl_here}
    subtree <- extract_subtree(do_owl, "DOID:3070")
    st_formatted <- format_subtree(subtree, "DOID:3070")
    st_formatted
} # }
```
