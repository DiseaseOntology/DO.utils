# Format DOIDs

Convert valid DOIDs and/or bare numbers to a specified DOID format.
Input *may* be tested to ensure it matches a valid DOID format but no
attempt is made to confirm bare numbers or DOIDs match actual diseases
in the ontology.

## Usage

``` r
format_doid(x, as = "CURIE", convert_bare = FALSE, validate_input = TRUE)
```

## Arguments

- x:

  A set of IDs, as a character vector.

- as:

  The format to convert the DOIDs to, as a string. All valid formats are
  possible options: "CURIE" (default), "URI", "obo_CURIE", "basename".

- convert_bare:

  Whether bare numbers should be converted to canonical DOIDs, `TRUE` or
  `FALSE` (default).

- validate_input:

  Whether to ensure only valid DOIDs are included in `x`, `TRUE`
  (default) or `FALSE`. When `FALSE`, non-DOID input will be returned
  unchanged.

## Caution

Be extra cautious when using `format_doid()` with
`validate_input = FALSE` as unexpected text conversion may occur.

## See also

Other format IDs:
[`format_obo()`](https://allenbaron.github.io/DO.utils/reference/format_obo.md)

## Examples

``` r
x <- c(
    "http://purl.obolibrary.org/obo/DOID_0001816",
    "DOID:4",
    "obo:DOID_14566",
    "DOID_0040001"
)

format_doid(x, as = "URI")
#> [1] "http://purl.obolibrary.org/obo/DOID_0001816"
#> [2] "http://purl.obolibrary.org/obo/DOID_4"      
#> [3] "http://purl.obolibrary.org/obo/DOID_14566"  
#> [4] "http://purl.obolibrary.org/obo/DOID_0040001"
format_doid(x, as = "CURIE")
#> [1] "DOID:0001816" "DOID:4"       "DOID:14566"   "DOID:0040001"
format_doid(x, as = "obo_CURIE")
#> [1] "obo:DOID_0001816" "obo:DOID_4"       "obo:DOID_14566"   "obo:DOID_0040001"
format_doid(x, as = "basename")
#> [1] "DOID_0001816" "DOID_4"       "DOID_14566"   "DOID_0040001"

# bare numbers can be converted to canonical DOIDs, if desired
w_bare <- c(x, "0050117")
format_doid(w_bare, convert_bare = TRUE)
#> [1] "DOID:0001816" "DOID:4"       "DOID:14566"   "DOID:0040001" "DOID:0050117"

# non-DOIDs can be passed as input, if desired
mixed_input <- c(x, "random_text", "obo:SYMP_0000000", "0050117")
format_doid(mixed_input, validate_input = FALSE)
#> [1] "DOID:0001816"     "DOID:4"           "DOID:14566"       "DOID:0040001"    
#> [5] "random_text"      "obo:SYMP_0000000" "0050117"         
format_doid(mixed_input, convert_bare = TRUE, validate_input = FALSE)
#> [1] "DOID:0001816"     "DOID:4"           "DOID:14566"       "DOID:0040001"    
#> [5] "random_text"      "obo:SYMP_0000000" "DOID:0050117"    
```
