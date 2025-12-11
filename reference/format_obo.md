# Format OBO Foundry IDs

Convert valid OBO Foundry ontology IDs to a specified format. Input
*may* be tested to ensure it matches a valid OBO ID format but no
attempt is made to confirm IDs match actual terms in any OBO Foundry
ontology.

## Usage

``` r
format_obo(x, as = "CURIE", validate_input = TRUE)
```

## Arguments

- x:

  A set of IDs, as a character vector.

- as:

  The format to convert the OBO IDs to, as a string. The following
  formats are possible options:

  - `"CURIE"` (default)

  - `"URI"`

  - `"bracketed_URI"`: e.g.
    `"<http://purl.obolibrary.org/obo/CL_0000066>"`

  - `"ns_lui"`: namespace with local unique identifier (preserves
    separator).

  - `"ns"`: namespace of ontology only

  As valid OBO formats, the first three formats may be modified
  repeatedly by `format_obo()`. The 'ns' formats, on the other hand, are
  not valid OBO formats and cannot be formatted again by `format_obo()`.

- validate_input:

  Whether to ensure only valid OBO IDs are included in `x`,`TRUE`
  (default) or `FALSE`. When `FALSE`, non-OBO ID input will *most
  likely* be returned unchanged.

## Caution

Be extra cautious when using `format_obo()` with
`validate_input = FALSE` as unexpected text conversion may occur.

## See also

Other format IDs:
[`format_doid()`](https://allenbaron.github.io/DO.utils/reference/format_doid.md)

## Examples

``` r
x <- c(
    "http://purl.obolibrary.org/obo/DOID_0001816",
    "<http://purl.obolibrary.org/obo/CL_0000066>",
    "obo:SYMP_0000000",
    "obo:so#has_origin"
)

# reversible
format_obo(x, as = "CURIE")
#> [1] "obo:DOID_0001816"  "obo:CL_0000066"    "obo:SYMP_0000000" 
#> [4] "obo:so#has_origin"
format_obo(x, as = "URI")
#> [1] "http://purl.obolibrary.org/obo/DOID_0001816" 
#> [2] "http://purl.obolibrary.org/obo/CL_0000066"   
#> [3] "http://purl.obolibrary.org/obo/SYMP_0000000" 
#> [4] "http://purl.obolibrary.org/obo/so#has_origin"
format_obo(x, as = "bracketed_URI")
#> [1] "<http://purl.obolibrary.org/obo/DOID_0001816>" 
#> [2] "<http://purl.obolibrary.org/obo/CL_0000066>"   
#> [3] "<http://purl.obolibrary.org/obo/SYMP_0000000>" 
#> [4] "<http://purl.obolibrary.org/obo/so#has_origin>"

# irreversible
format_obo(x, as = "ns_lui")
#> [1] "DOID_0001816"  "CL_0000066"    "SYMP_0000000"  "so#has_origin"
format_obo(x, as = "ns")
#> [1] "DOID" "CL"   "SYMP" "so#" 

# non-OBO IDs can be passed as input with caution, if desired
mixed_input <- c(x, "random_text", "0050117", "obo:SYMP:0000000")
format_obo(mixed_input, validate_input = FALSE)
#> [1] "obo:DOID_0001816"  "obo:CL_0000066"    "obo:SYMP_0000000" 
#> [4] "obo:so#has_origin" "random_text"       "0050117"          
#> [7] "obo:SYMP:0000000" 
```
