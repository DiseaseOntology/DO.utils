# Parse OMIM Entry Names

Parses OMIM entry names — often listed in all-uppercase with an
"inverted filing" convention (primary disease term first, then
comma-separated qualifiers) and an optional semicolon-separated
abbreviation — into case- and order-normalized names and separate
abbreviations. Mixed-case input (e.g. from `genemap2.txt`) is also
accepted.

## Usage

``` r
parse_omim_name(
  x,
  col = NULL,
  eponyms = disease_eponyms,
  patterns = disease_cap_patterns
)
```

## Arguments

- x:

  A character vector of OMIM entry name strings (e.g.
  `"SPASTIC PARAPLEGIA 14, AUTOSOMAL RECESSIVE; SPG14"`), or a data
  frame with such a column.

- col:

  \[**data.frame only**\] The name of the column in `x` that contains
  OMIM entry name strings, passed as a string (e.g. `"entry"`).

- eponyms:

  A named character vector for proper noun (eponym) capitalization,
  where names are lowercase words and values are their capitalized
  replacements (e.g. `c("waardenburg" = "Waardenburg")`). Applied as
  whole-word, case-insensitive substitutions after lowercasing. Defaults
  to
  [disease_eponyms](https://diseaseontology.github.io/DO.utils/reference/disease_eponyms.md).
  Pass `NULL` to disable, or supply your own vector to override.

- patterns:

  A named character vector of phrase-level regex substitutions applied
  to the full lowercased name *after* `eponyms`, longest-first. Useful
  for context-sensitive capitalization (e.g.
  `c("short syndrome" = "SHORT syndrome")`). Defaults to
  [disease_cap_patterns](https://diseaseontology.github.io/DO.utils/reference/disease_cap_patterns.md).
  Pass `NULL` to disable.

## Value

- **Character vector input**: a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
  columns:

  - `name`: normalized name after rearrangement and capitalization
    fixes.

  - `abbreviation`: the semicolon-separated abbreviation, or `NA`.

- **Data frame input**: the input data frame with `name` and
  `abbreviation` appended (or replaced if already present).

## Details

Reverses OMIM\\s inverted filing convention by reclassifying
comma-separated qualifier tokens (inheritance terms, type codes, onset
adjectives, etc.) and reassembling them in natural-language order.
Rearrangement is only applied when at least one qualifier matches a
recognized forcing pattern; otherwise tokens are kept in original order
and lowercased. See the [algorithm
article](https://allenbaron.github.io/DO.utils/articles/parse-omim-name.html)
for full details on qualifier classification, capitalization rules, and
known limitations.

## See also

[disease_eponyms](https://diseaseontology.github.io/DO.utils/reference/disease_eponyms.md)
for the curated eponym replacement vector;
[disease_cap_patterns](https://diseaseontology.github.io/DO.utils/reference/disease_cap_patterns.md)
for the curated phrase pattern replacement vector; the [algorithm
article](https://allenbaron.github.io/DO.utils/articles/parse-omim-name.html)
for full details on how parsing and rearrangement work.

## Examples

``` r
parse_omim_name(c(
    "SCHIZOPHRENIA 12",
    "DYSTONIA 12; DYT12",
    "SPASTIC PARAPLEGIA 14, AUTOSOMAL RECESSIVE; SPG14",
    "OSTEOGENESIS IMPERFECTA, TYPE XI; OI11",
    "SCOLIOSIS, ISOLATED, SUSCEPTIBILITY TO, 1; IS1",
    "EPILEPSY, PROGRESSIVE MYOCLONIC, 4, WITH OR WITHOUT RENAL FAILURE; EPM4"
))
#> # A tibble: 6 × 2
#>   name                                                           abbreviation
#>   <chr>                                                          <chr>       
#> 1 schizophrenia 12                                               NA          
#> 2 dystonia 12                                                    DYT12       
#> 3 autosomal recessive spastic paraplegia 14                      SPG14       
#> 4 osteogenesis imperfecta type XI                                OI11        
#> 5 susceptibility to isolated scoliosis 1                         IS1         
#> 6 progressive myoclonic epilepsy 4 with or without renal failure EPM4        

# Data frame input
df <- data.frame(entry = c("HURIEZ SYNDROME; HRZ", "SCHWANNOMATOSIS, VESTIBULAR; SWNV"))
parse_omim_name(df, col = "entry")
#>                               entry                       name abbreviation
#> 1              HURIEZ SYNDROME; HRZ            Huriez syndrome          HRZ
#> 2 SCHWANNOMATOSIS, VESTIBULAR; SWNV vestibular schwannomatosis         SWNV

# Proper noun correction via custom eponyms (overrides disease_eponyms)
parse_omim_name(
    "PERIPHERAL DEMYELINATING NEUROPATHY, CENTRAL DYSMYELINATION, WAARDENBURG SYNDROME, AND HIRSCHSPRUNG DISEASE; PCWH",
    eponyms = c("waardenburg" = "Waardenburg", "hirschsprung" = "Hirschsprung")
)
#> # A tibble: 1 × 2
#>   name                                                              abbreviation
#>   <chr>                                                             <chr>       
#> 1 peripheral demyelinating neuropathy, central dysmyelination, Waa… PCWH        

# Context-sensitive capitalization via phrase patterns (overrides eponyms)
parse_omim_name(
    "SHORT SYNDROME; SHORTSYN",
    patterns = c("short syndrome" = "SHORT syndrome")
)
#> # A tibble: 1 × 2
#>   name           abbreviation
#>   <chr>          <chr>       
#> 1 SHORT syndrome SHORTSYN    
```
