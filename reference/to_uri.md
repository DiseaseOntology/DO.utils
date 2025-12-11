# Convert CURIE to URI

Converts CURIE(s) to URI(s). Angle brackets will not be added.

## Usage

``` r
to_uri(x)
```

## Arguments

- x:

  CURIE(s), as a character vector.

## Note

Performs no CURIE validation but relies on a properly formatted CURIE
for conversion (e.g. prefix:reference, according to the [CURIE
standard](https://www.w3.org/TR/2010/NOTE-curie-20101216/#s_syntax)).
Any inputs not matching a prefix of
[ns_prefix](https://allenbaron.github.io/DO.utils/reference/ns_prefix.md)
will be returned without modification.

## See also

Other identifier converters:
[`to_curie()`](https://allenbaron.github.io/DO.utils/reference/to_curie.md)

## Examples

``` r
.curie <- c("rdfs:comment", "dc:date", "terms:license", "owl:deprecated",
            "oboInOwl:id", "UBERON:0000002", "DOID:0001816",
            "doid:DO_AGR_slim")
to_uri(.curie)
#> [1] "http://www.w3.org/2000/01/rdf-schema#comment"   
#> [2] "http://purl.org/dc/elements/1.1/date"           
#> [3] "http://purl.org/dc/terms/license"               
#> [4] "http://www.w3.org/2002/07/owl#deprecated"       
#> [5] "http://www.geneontology.org/formats/oboInOwl#id"
#> [6] "http://purl.obolibrary.org/obo/UBERON_0000002"  
#> [7] "http://purl.obolibrary.org/obo/DOID_0001816"    
#> [8] "http://purl.obolibrary.org/obo/doid#DO_AGR_slim"

#returns non-CURIE or unknown namespace prefixes unmodified
to_curie(c("SYMP:0000000", "not a CURIE", "bioregistry.collection:0000001"))
#> [1] "SYMP:0000000"                   "not a CURIE"                   
#> [3] "bioregistry.collection:0000001"
```
