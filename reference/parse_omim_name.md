# Parse OMIM Entry Names

Parses OMIM entry names into case- and order-normalized names,
preserving abbreviations, if present.

## Usage

``` r
parse_omim_name(
  x,
  eponyms = disease_eponyms,
  patterns = disease_cap_patterns,
  qualifiers = omim_qualifiers
)
```

## Arguments

- x:

  A character vector of OMIM entry name strings.

- eponyms:

  A named character vector for proper noun (eponym) capitalization,
  where names are lowercase words and values are their capitalized
  replacements, e.g. `c("waardenburg" = "Waardenburg")`, or `NULL` to
  disable (default:
  [disease_eponyms](https://diseaseontology.github.io/DO.utils/reference/disease_eponyms.md)).

- patterns:

  A named character vector of phrase-level regex substitutions applied
  to the full lowercased name *after* `eponyms`, longest-first. Useful
  for context-sensitive capitalization, e.g.
  `c("short syndrome" = "SHORT syndrome")`, or `NULL` to disable
  (default:
  [disease_cap_patterns](https://diseaseontology.github.io/DO.utils/reference/disease_cap_patterns.md)).

- qualifiers:

  A character vector of uppercase OMIM qualifier tokens that force name
  rearrangement, in addition to the hardcoded structural patterns
  (numbers, `TYPE`, and definitive inheritance terms). Use `NULL` to
  disable adjective qualifier matching (default:
  [omim_qualifiers](https://diseaseontology.github.io/DO.utils/reference/omim_qualifiers.md)).

## Value

A character vector of normalized OMIM entry names with abbreviations
preserved.

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
for the curated phrase pattern replacement vector;
[omim_qualifiers](https://diseaseontology.github.io/DO.utils/reference/omim_qualifiers.md)
for the curated adjective qualifier vector; the [algorithm
article](https://allenbaron.github.io/DO.utils/articles/parse-omim-name.html)
for full details on how parsing and rearrangement work.

## Examples

``` r
omim_names <- c(
    "SCHIZOPHRENIA 12",
    "DYSTONIA 12; DYT12",
    "SPASTIC PARAPLEGIA 14, AUTOSOMAL RECESSIVE; SPG14",
    "OSTEOGENESIS IMPERFECTA, TYPE XI; OI11",
    "SCOLIOSIS, ISOLATED, SUSCEPTIBILITY TO, 1; IS1",
    "EPILEPSY, PROGRESSIVE MYOCLONIC, 4, WITH OR WITHOUT RENAL FAILURE; EPM4"
)

parse_omim_name(omim_names)
#> [1] "schizophrenia 12"                                                    
#> [2] "dystonia 12; DYT12"                                                  
#> [3] "autosomal recessive spastic paraplegia 14; SPG14"                    
#> [4] "osteogenesis imperfecta type XI; OI11"                               
#> [5] "susceptibility to isolated scoliosis 1; IS1"                         
#> [6] "progressive myoclonic epilepsy 4 with or without renal failure; EPM4"

# Proper noun correction via custom eponyms (overrides disease_eponyms)
parse_omim_name(
    "PERIPHERAL DEMYELINATING NEUROPATHY, CENTRAL DYSMYELINATION, WAARDENBURG SYNDROME, AND HIRSCHSPRUNG DISEASE; PCWH",
    eponyms = c("waardenburg" = "Waardenburg", "hirschsprung" = "Hirschsprung")
)
#> [1] "peripheral demyelinating neuropathy, central dysmyelination, Waardenburg syndrome, and Hirschsprung disease; PCWH"

# Context-sensitive capitalization via phrase patterns (overrides eponyms)
parse_omim_name(
    "SHORT SYNDROME; SHORTSYN",
    patterns = c("short syndrome" = "SHORT syndrome")
)
#> [1] "SHORT syndrome; SHORTSYN"
```
