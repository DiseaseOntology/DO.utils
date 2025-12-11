# OBO ID predicates

These predicates are designed to identify and validate common ID formats
defined within OBO Foundry ontologies.

## Usage

``` r
is_valid_obo(x)

is_valid_doid(x)
```

## Arguments

- x:

  A set of IDs, as a character vector.

## Details

- `is_valid_obo()` to determine if identifiers match the OBO Foundry IRI
  pattern or are an obo:LUI CURIE.

- `is_valid_doid()` to determine if identifiers match DO's IRI or CURIE
  pattern.

## Notes

These predicates *do not* attempt to confirm any ID actually exists in
an OBO Foundry ontology, but only test if the IDs are syntactically
formatted correctly (see [OBO Foundry ID
Policy](https://obofoundry.org/id-policy) and [OBO File
Specification](https://owlcollab.github.io/oboformat/doc/obo-syntax.html)).

Not all OBO formats are valid DOID formats and vice versa.

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
    #### valid ####
    "http://purl.obolibrary.org/obo/DOID_0001816", # URI
    "<http://purl.obolibrary.org/obo/CL_0000066>", # bracketed_URI
    "obo:DOID_4", # CURIE, standard version
    "obo:so#has_origin", # '#' separator ~ OBO annotation properties
    #### invalid ####
    "0001816", # bare number without prefix
    "obo:DOID:14566", # namespace-lui separator must be '_' or '#'
    " obo:HP_0000001" # must have NO `[:space:]` characters
)

is_valid_obo(obo_id)
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE

# DOID formats
doid <- c(
    #### valid ####
    "http://purl.obolibrary.org/obo/DOID_0001816", # URI
    "DOID:4", # CURIE, standard version
    "obo:DOID_4", # OBO CURIE, less common
    "DOID_0040001", # basename (OBO prefix removed)
    #### invalid ####
    "0001816", # bare number without prefix
    "doid#DO_IEDB_slim", # namespace-lui separator must be '_'
    "obo:doid#DO_IEDB_slim",
    "obo:DOID_21 " # must have NO `[:space:]` characters
)

is_valid_doid(doid)
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
```
