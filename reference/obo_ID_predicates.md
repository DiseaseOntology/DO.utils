# OBO ID predicates

These predicates are designed to identify and validate common ID formats
defined within OBO Foundry ontologies.

## Usage

``` r
is_valid_obo(x, allow = "standard", ns_type = "obo")

is_valid_doid(x, allow = "standard", ns_type = "obo")
```

## Arguments

- x:

  A set of IDs, as a character vector.

- allow:

  The OBO ID format(s) to consider valid; as a character vector. One or
  more of: `"curie"`, `"obo_curie"` (e.g. `obo:DOID_4`), `"uri"`,
  `"<uri>"` (URI in angle brackets), or `"ns.lui"` (i.e. an OBO CURIE
  without `obo:`).

  `"standard"` (default) is a generic grouping that represents
  syntactically correct formats (`"curie"`, `"obo_curie"`, `"uri"` and
  `"<uri>"`).

  `"ns.lui"` will only be included if explicitly specified.

- ns_type:

  The type of OBO namespaces to consider valid; as a character vector.
  One of: `"obo"` (default), `"ont"` (ontology primary namespaces only),
  or `"prop"` (ontology property namespaces only; may not be
  exhaustive).

## Details

- `is_valid_obo()` to determine if identifiers match an OBO Foundry ID.

- `is_valid_doid()` to determine if identifiers match a DOID/doid.

## Notes

These predicates *do not* attempt to confirm any ID actually exists in
an OBO Foundry ontology, but only test if the IDs are syntactically
formatted correctly (see [OBO Foundry ID
Policy](https://obofoundry.org/id-policy) and [OBO File
Specification](https://owlcollab.github.io/oboformat/doc/obo-syntax.html))
*AND* correspond to a known namespace of an OBO Foundry ontology (see
[obo_prefix](https://allenbaron.github.io/DO.utils/reference/ns_prefix.md)).

## See also

Other ID predicates:
[`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md),
[`is_uri()`](https://allenbaron.github.io/DO.utils/reference/is_uri.md)

Other predicates:
[`all_duplicated()`](https://allenbaron.github.io/DO.utils/reference/all_duplicated.md),
[`char_val_predicates`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md),
[`iff_all_vals()`](https://allenbaron.github.io/DO.utils/reference/iff_all_vals.md),
[`is_curie()`](https://allenbaron.github.io/DO.utils/reference/is_curie.md),
[`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md),
[`is_uri()`](https://allenbaron.github.io/DO.utils/reference/is_uri.md),
[`lgl_predicates`](https://allenbaron.github.io/DO.utils/reference/lgl_predicates.md),
[`num_val_predicates`](https://allenbaron.github.io/DO.utils/reference/num_val_predicates.md)

## Examples

``` r
# OBO formats
obo_id <- c(
    #### valid by default ####
    "http://purl.obolibrary.org/obo/DOID_0001816", # URI
    "<http://purl.obolibrary.org/obo/CL_0000066>", # angle-bracketed URI
    "DOID:4", # CURIE, standard version
    "obo:CL_0000066", # OBO CURIE
    #### valid OBO property ####
    "so:has_origin", # standard CURIE ~ OBO annotation property
    "obo:so#has_origin", # '#' separator ~ OBO annotation property
    ### conditionally valid -- only if "ns.lui" in `allow` ###
    "DOID_0040001", # namespace-separator-LUI (i.e. OBO CURIE w/'obo:' prefix removed)
    "so#has_origin", # namespace-lui separator must be '#' for properties
    #### invalid ####
    "0001816", # bare number without prefix
    "obo:DOID:14566", # namespace-lui separator must be '_' ('#' for properties)
    " obo:HP_0000001" # must have NO `[:space:]` characters
)

is_valid_obo(obo_id)
#>  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
is_valid_obo(obo_id, allow = c("standard", "ns.lui"))
#>  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE

# DOID formats
doid <- c(
    #### valid ####
    "http://purl.obolibrary.org/obo/DOID_0001816", # URI
    "DOID:4", # CURIE, standard version
    "obo:DOID_4", # OBO CURIE, less common
    # properties are recognized as valid (unless ns_type = 'ont')
    "doid:DO_IEDB_slim",
    "obo:doid#DO_IEDB_slim",
    # namespace-separator-LUI (valid if 'ns.lui' included in `allow`)
    "DOID_0040001",
    #### invalid ####
    "0001816", # bare number without prefix
    "obo:DOID_21 " # must have NO `[:space:]` characters
)

is_valid_doid(doid)
#> [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
is_valid_doid(doid, ns_type = "ont")
#> [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
is_valid_doid(doid, allow = c("standard", "ns.lui"), ns_type = "ont")
#> [1]  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE
```
