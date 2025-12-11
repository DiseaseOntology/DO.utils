# Convert URI to CURIE

Converts URI(s) to CURIE(s).

## Usage

``` r
to_curie(x, strip_angle_brackets = TRUE)
```

## Arguments

- x:

  URI(s), as a character vector.

- strip_angle_brackets:

  Whether to remove all `<` and `>` from the input, as a boolean
  (default: `TRUE`). Where this might have undesirable consequences,
  e.g. *some* angle brackets need to be removed, perform the removal
  beforehand and use `strip_angle_brackets = FALSE`.

## Note

Performs no URI validation, relying on simple string matching of
namespace-prefix pairs of
[ns_prefix](https://allenbaron.github.io/DO.utils/reference/ns_prefix.md)
for conversion. Any values not matching one of these will be returned
without modification.

## See also

Other identifier converters:
[`to_uri()`](https://allenbaron.github.io/DO.utils/reference/to_uri.md)

## Examples

``` r
.uri <- c(
    "http://www.w3.org/2000/01/rdf-schema#comment",
    "http://purl.org/dc/elements/1.1/date",
    "http://purl.org/dc/terms/license",
    "http://www.w3.org/2002/07/owl#deprecated",
    "http://www.geneontology.org/formats/oboInOwl#id",
    "http://purl.obolibrary.org/obo/UBERON_0000002",
    "http://purl.obolibrary.org/obo/DOID_0001816",
    "http://purl.obolibrary.org/obo/doid#DO_AGR_slim",
    "<http://www.geneontology.org/formats/oboInOwl#hasDbXref>"
)
to_curie(.uri)
#> [1] "rdfs:comment"       "dc:date"            "terms:license"     
#> [4] "owl:deprecated"     "oboInOwl:id"        "UBERON:0000002"    
#> [7] "DOID:0001816"       "doid:DO_AGR_slim"   "oboInOwl:hasDbXref"

# uses 'obo' namespace when an OBO Foundry ontology namespace isn't available
to_curie(
    c("http://purl.obolibrary.org/obo/SO_0000110",
    "http://purl.obolibrary.org/obo/so#has_origin")
)
#> [1] "SO:0000110"    "so:has_origin"

#returns non-URI or unknown namespace prefixes unmodified
to_curie(
    c("http://purl.obolibrary.org/obo/SYMP_0000000",
    "not a URI", "https://disease-ontology.org/")
)
#> [1] "SYMP:0000000"                  "not a URI"                    
#> [3] "https://disease-ontology.org/"
```
