# Format OBO Foundry IDs

Convert valid OBO Foundry ontology IDs to a specified format. Input
*may* be tested to ensure it matches a valid OBO ID format but no
attempt is made to confirm IDs match actual terms in any OBO Foundry
ontology.

`format_doid` is a convenience function focused solely on DOIDs.

## Usage

``` r
format_obo(
  x,
  as = "curie",
  validate = TRUE,
  allow = "standard",
  ns_type = "obo"
)

format_doid(
  x,
  as = "curie",
  validate = TRUE,
  allow = "standard",
  ns_type = "obo"
)
```

## Arguments

- x:

  A set of IDs, as a character vector.

- as:

  The format to convert the OBO IDs to, as a string. One of: `"curie"`
  (default), `"obo_curie"` (e.g. `obo:DOID_4`, `obo:doid#DO_rare_slim`),
  `"uri"`, `"<uri>"` (URI surrounded by angle brackets), `"ns.lui"`
  (i.e. an OBO CURIE without `obo:`).

- validate:

  Whether to ensure only valid OBO IDs are included in `x`, `TRUE`
  (default) or `FALSE`. When `FALSE`, non-OBO ID input will be handled
  differently depending on `as` (see `Non-OBO CURIE/URIs` section).

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

## Note on `ns_type`

The `ns_type` argument affects both validation *AND* formatting.

## Non-OBO CURIE/URIs

Be extra cautious when using `format_obo()` with non-OBO Foundry
CURIE/URIs (i.e. `validate = FALSE`). In an effort to allow meaningful
pass-through in some situations, non-OBO CURIE/URIs are returned
unchanged, except when `as = "uri"` or `"<uri>"`, in which case the
original input is assumed to be URIs and is either stripped or
surrounded by angle brackets.

## Examples

``` r
x <- c(
    "http://purl.obolibrary.org/obo/DOID_0001816",
    "<http://purl.obolibrary.org/obo/CL_0000066>",
    "obo:SYMP_0000000",
    "obo:so#has_origin",
    "DOID:4"
)

format_obo(x, as = "curie")
#> [1] "DOID:0001816"  "CL:0000066"    "SYMP:0000000"  "so:has_origin"
#> [5] "DOID:4"       
format_obo(x, as = "uri")
#> [1] "http://purl.obolibrary.org/obo/DOID_0001816" 
#> [2] "http://purl.obolibrary.org/obo/CL_0000066"   
#> [3] "http://purl.obolibrary.org/obo/SYMP_0000000" 
#> [4] "http://purl.obolibrary.org/obo/so#has_origin"
#> [5] "http://purl.obolibrary.org/obo/DOID_4"       
format_obo(x, as = "<uri>")
#> [1] "<http://purl.obolibrary.org/obo/DOID_0001816>" 
#> [2] "<http://purl.obolibrary.org/obo/CL_0000066>"   
#> [3] "<http://purl.obolibrary.org/obo/SYMP_0000000>" 
#> [4] "<http://purl.obolibrary.org/obo/so#has_origin>"
#> [5] "<http://purl.obolibrary.org/obo/DOID_4>"       
format_obo(x, as = "obo_curie")
#> [1] "obo:DOID_0001816"  "obo:CL_0000066"    "obo:SYMP_0000000" 
#> [4] "obo:so#has_origin" "obo:DOID_4"       
format_obo(x, as = "ns.lui")
#> [1] "DOID_0001816"  "CL_0000066"    "SYMP_0000000"  "so#has_origin"
#> [5] "DOID_4"       

# ns.lui input can be validated, if explicitly specified
try(format_obo(c(x, "DOID_0001816"), allow = "standard"))
#> Error : Elements 6 of valid_obo are not true
format_obo(c(x, "DOID_0001816"), allow = c("standard", "ns.lui"))
#> [1] "DOID:0001816"  "CL:0000066"    "SYMP:0000000"  "so:has_origin"
#> [5] "DOID:4"        "DOID:0001816" 

# non-OBO IDs can be passed as input with caution, if desired
mixed_input <- c(
    x, "rdfs:label", "<http://xmlns.com/foaf/0.1/Person>",
    "random_text", "0050117", "obo:SYMP:0000000"
)
format_obo(mixed_input, validate = FALSE)
#>  [1] "DOID:0001816"                       "CL:0000066"                        
#>  [3] "SYMP:0000000"                       "so:has_origin"                     
#>  [5] "DOID:4"                             "rdfs:label"                        
#>  [7] "<http://xmlns.com/foaf/0.1/Person>" "random_text"                       
#>  [9] "0050117"                            "obo:SYMP:0000000"                  

# ns_type will influence output, even when validate = FALSE
# e.g. only obo:so#has_origin (property) is converted to a CURIE (so:has_origin)
format_obo(x, as = "curie", validate = FALSE, ns_type = "prop")
#> [1] "http://purl.obolibrary.org/obo/DOID_0001816"
#> [2] "<http://purl.obolibrary.org/obo/CL_0000066>"
#> [3] "obo:SYMP_0000000"                           
#> [4] "so:has_origin"                              
#> [5] "DOID:4"                                     

# format_doid() works the same
x <- c(
    "http://purl.obolibrary.org/obo/DOID_0001816",
    "DOID:4",
    "obo:DOID_14566",
    "<http://purl.obolibrary.org/obo/DOID_156>"
)

format_doid(x, as = "curie")
#> [1] "DOID:0001816" "DOID:4"       "DOID:14566"   "DOID:156"    

# ...but other OBO Foundry ontology IDs will error on validation
try(format_doid(c(x, "obo:CL_0000066")))
#> Error : Elements 5 of valid_doid are not true

# though they can be passed through if validation is turned off
mixed_input <- c(x, "obo:SYMP_0000000", "foaf:Person", "random_text", "0050117")
format_doid(mixed_input, validate = FALSE)
#> [1] "DOID:0001816"     "DOID:4"           "DOID:14566"       "DOID:156"        
#> [5] "obo:SYMP_0000000" "foaf:Person"      "random_text"      "0050117"         
```
