# CURIE (RDF ID) Predicate

This predicate is designed to validate CURIEs, compact URIs conforming
to the [W3C CURIE Syntax
1.0](https://www.w3.org/TR/2010/NOTE-curie-20101216/) standard (or
stricter as allowed by that standard).

## Usage

``` r
is_curie(x, def = "obo_generic")
```

## Arguments

- x:

  A set of CURIEs, as a character vector.

- def:

  The definition of CURIEs to test against; as a string. One of `"obo"`,
  `"obo_generic"` (default), `"w3c"`, or `"w3c_safe"` (see `Notes`
  section for details).

## Notes

The [W3C CURIE Syntax
1.0](https://www.w3.org/TR/2010/NOTE-curie-20101216/) standard defines
the most general CURIE syntax allowed (`def = "w3c"`; not a perfect
implementation, e.g. U+3000 non-breaking space is not accepted). Note
that by definition URIs will be identified as CURIEs. If it is desirable
to distinguish these use `def = "w3c_safe"` and wrap CURIEs in square
brackets (as defined in the standard). Alternatively, use the stricter
OBO Foundry-based standards (as stated in that standard, more strict
definitions for CURIEs can be defined).

`"obo"` corresponds to the official OBO Foundry definition of a CURIE as
stated in the [OBO Foundry ID
Policy](http://obofoundry.org/id-policy.md). This should work for *most*
class CURIEs from OBO Foundry ontologies but will not recognize
`obo:IDSPACE_LOCALID` formatted CURIEs or CURIEs with letters or symbols
in their local unique identifier. To allow these in the set while still
restricting to CURIE patterns found in OBO Foundry ontologies, use
`"obo_generic"`.

## See also

Other ID predicates:
[`is_uri()`](https://allenbaron.github.io/DO.utils/reference/is_uri.md),
[`obo_ID_predicates`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)

Other predicates:
[`all_duplicated()`](https://allenbaron.github.io/DO.utils/reference/all_duplicated.md),
[`char_val_predicates`](https://allenbaron.github.io/DO.utils/reference/char_val_predicates.md),
[`iff_all_vals()`](https://allenbaron.github.io/DO.utils/reference/iff_all_vals.md),
[`is_invariant()`](https://allenbaron.github.io/DO.utils/reference/is_invariant.md),
[`is_uri()`](https://allenbaron.github.io/DO.utils/reference/is_uri.md),
[`lgl_predicates`](https://allenbaron.github.io/DO.utils/reference/lgl_predicates.md),
[`num_val_predicates`](https://allenbaron.github.io/DO.utils/reference/num_val_predicates.md),
[`obo_ID_predicates`](https://allenbaron.github.io/DO.utils/reference/obo_ID_predicates.md)

## Examples

``` r
id <- c(
    #### pass all ####
    "DOID:0001816", "CL:0000066",
    #### pass "obo_generic" & "w3c" ####
    "obo:DOID_4", "obo:so#has_origin", # obo prefixed CURIEs
    "oboInOwl:hasDbXref", "skos:exactMatch", # obo object properties
    "alfred:LO362836C", # not OBO but conforms to `"obo_generic"` pattern
    #### pass only "w3c" ####
    "_4dn.biosource:4DNSR73BT2A2", "aceview.worm:aap-1",
    #### always fail ####
    "4dn.biosource:4DNSR73BT2A2", # starts with a number
    "0001816", # bare number without prefix
    " obo:HP_0000001", # must have NO `[:space:]` characters
    "http://purl.obolibrary.org/obo/DOID_0001816" # URI
)
```
