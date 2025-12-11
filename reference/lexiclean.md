# Prepare Text for Matching

Process text to improve exact, quasi-exact and/or lexical text matching
(e.g. "lexical cleaning").

## Usage

``` r
lexiclean(x, mod = "all")
```

## Arguments

- x:

  A character vector.

- mod:

  Desired modifications, as a character string:

  - `"all"` : Use all modifications.

  - `"lc"` : Convert to lowercase.

  - `"roman"` : Convert all numbers to roman numerals.

  - `"rm_space"` : Remove all spaces, defined by `[:space:]`, see
    [`regex()`](https://rdrr.io/r/base/regex.html).

  - `"rm_punct"` : Remove all punctuation, defined by `[:punct:]`, see
    [`regex()`](https://rdrr.io/r/base/regex.html).

## Value

A character vector with specified modifications applied.

## Examples

``` r
x <- c("disease", "X-linked exudative vitreoretinopathy 2", "hepatitis D",
       "dilated cardiomyopathy 1FF", "chromosome 15q25 deletion syndrome",
       "spinocerebellar ataxia type 19/22", "Addison's disease",
       "disease type II", "B-lymphoblastic leukemia/lymphoma, BCR-ABL1–like")

lexiclean(x)
#> [1] "disease"                                  
#> [2] "xlinkedexudativevitreoretinopathyii"      
#> [3] "hepatitisd"                               
#> [4] "dilatedcardiomyopathyiff"                 
#> [5] "chromosomexvqxxvdeletionsyndrome"         
#> [6] "spinocerebellarataxiatypexixxxii"         
#> [7] "addisonsdisease"                          
#> [8] "diseasetypeii"                            
#> [9] "blymphoblasticleukemialymphomabcrablilike"
lexiclean(x, "roman")
#> [1] "disease"                                         
#> [2] "X-linked exudative vitreoretinopathy II"         
#> [3] "hepatitis D"                                     
#> [4] "dilated cardiomyopathy IFF"                      
#> [5] "chromosome XVqXXV deletion syndrome"             
#> [6] "spinocerebellar ataxia type XIX/XXII"            
#> [7] "Addison's disease"                               
#> [8] "disease type II"                                 
#> [9] "B-lymphoblastic leukemia/lymphoma, BCR-ABLI–like"
```
