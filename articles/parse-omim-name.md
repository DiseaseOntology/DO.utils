# How parse_omim_name() Works

[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md)
converts OMIM entry names from their raw all-uppercase, inverted-filing
format into normalized lowercase names with proper noun capitalization.
This article explains how the algorithm works, when rearrangement is and
isn’t applied, what capitalization fixes are automatic, and what the
current limitations are.

------------------------------------------------------------------------

## OMIM’s inverted filing convention

OMIM lists entry names with the primary disease term first, followed by
comma-separated qualifier terms that in natural language would precede
it:

    SPASTIC PARAPLEGIA 14, AUTOSOMAL RECESSIVE

The natural form is: *autosomal recessive spastic paraplegia 14*.
[`parse_omim_name()`](https://diseaseontology.github.io/DO.utils/reference/parse_omim_name.md)
reverses this inversion when recognized qualifiers are present.

------------------------------------------------------------------------

## Qualifier classification

Comma-separated tokens after the primary term are classified as follows:

### Pre-qualifiers — moved *before* the primary term

Adjective tokens such as `AUTOSOMAL RECESSIVE`, `CONGENITAL`, or
`FAMILIAL` are placed before the primary term. When multiple
pre-qualifiers are present they are reversed in order, so the last
listed appears first — matching OMIM’s preferred-name conventions.
Definitive inheritance terms (`AUTOSOMAL DOMINANT`, `X-LINKED`,
`Y-LINKED`, `MITOCHONDRIAL`) always take priority and appear first, even
when listed after other qualifiers.

``` r
parse_omim_name("SPASTIC PARAPLEGIA 14, AUTOSOMAL RECESSIVE; SPG14")
#> # A tibble: 1 × 2
#>   name                                      abbreviation
#>   <chr>                                     <chr>       
#> 1 autosomal recessive spastic paraplegia 14 SPG14
parse_omim_name("DEAFNESS, AUTOSOMAL DOMINANT, CONGENITAL; DFNA")
#> # A tibble: 1 × 2
#>   name                                   abbreviation
#>   <chr>                                  <chr>       
#> 1 autosomal dominant congenital deafness DFNA
```

### Post-qualifiers — attached *after* the primary term

- **Numeric type codes** (`1`, `4`, `23`) are appended with a space.
- **`TYPE ...` tokens** are appended with a space.
- **Alphanumeric subtype codes** (e.g. `7A`) are hyphen-joined to the
  preceding word (e.g. `SYNDROME, 7A` → `syndrome-7A`).

``` r
parse_omim_name("OSTEOGENESIS IMPERFECTA, TYPE XI; OI11")
#> # A tibble: 1 × 2
#>   name                            abbreviation
#>   <chr>                           <chr>       
#> 1 osteogenesis imperfecta type XI OI11
parse_omim_name("SCOLIOSIS, ISOLATED, SUSCEPTIBILITY TO, 1; IS1")
#> # A tibble: 1 × 2
#>   name                                   abbreviation
#>   <chr>                                  <chr>       
#> 1 susceptibility to isolated scoliosis 1 IS1
```

### Trailing phrases — appended *last*

Tokens beginning with `WITH`, `DUE TO`, `AND`, or `OR` are treated as
descriptive trailing phrases and appended after the primary term (and
any post-qualifiers).

``` r
parse_omim_name("EPILEPSY, PROGRESSIVE MYOCLONIC, 4, WITH OR WITHOUT RENAL FAILURE; EPM4")
#> # A tibble: 1 × 2
#>   name                                                           abbreviation
#>   <chr>                                                          <chr>       
#> 1 progressive myoclonic epilepsy 4 with or without renal failure EPM4
```

### `SUSCEPTIBILITY TO`

The token `SUSCEPTIBILITY TO` is always prepended before the entire
assembled name.

``` r
parse_omim_name("DIABETES MELLITUS, SUSCEPTIBILITY TO")
#> # A tibble: 1 × 2
#>   name                                abbreviation
#>   <chr>                               <chr>       
#> 1 susceptibility to diabetes mellitus NA
```

------------------------------------------------------------------------

## When rearrangement is applied

Rearrangement is only triggered when at least one qualifier matches a
**forcing pattern**:

- A pure number or alphanumeric subtype code (e.g. `1`, `7A`)
- A `TYPE` or `MULTIPLE TYPES` qualifier
- A definitive inheritance term (`AUTOSOMAL DOMINANT`, `X-LINKED`, etc.)
- A core set of strong adjective/onset qualifiers: `BILATERAL`,
  `CHILDHOOD-ONSET`, `CONGENITAL`, `EARLY-ONSET`, `FAMILIAL`, `FOCAL`,
  `GENERALIZED`, `HEREDITARY`, `HYPOMYELINATING`, `ISOLATED`,
  `JUVENILE`, `LATE-ONSET`, `NEONATAL`, `POSTSYNAPTIC`, `PRESYNAPTIC`,
  `PROGRESSIVE`, `SUSCEPTIBILITY TO`, `UNILATERAL`, `VESTIBULAR`

When **no** forcing pattern is present (e.g. multi-feature descriptor
lists), the tokens are kept in original comma order and only lowercased:

``` r
# No forcing pattern → no rearrangement
parse_omim_name("SPASTIC TETRAPLEGIA, THIN CORPUS CALLOSUM, AND PROGRESSIVE MICROCEPHALY")
#> # A tibble: 1 × 2
#>   name                                                              abbreviation
#>   <chr>                                                             <chr>       
#> 1 spastic tetraplegia, thin corpus callosum, and progressive micro… NA
```

To extend the forcing list for edge cases, open an issue or submit a PR
editing `omim_has_forcing()` in `R/parse.R`.

------------------------------------------------------------------------

## Capitalization fixes applied automatically

After rearrangement and lowercasing, `fix_disease_caps()` applies these
rules:

- **Roman numerals** following `type` are uppercased (e.g. `type xi` →
  `type XI`).
- **Alphanumeric subtype codes** are uppercased (e.g. `7a` → `7A`, `3a`
  → `3A`).
- **Immunoglobulin abbreviations**: `IgA`, `IgD`, `IgE`, `IgG`, `IgM`,
  `IgY`.
- **Word-level eponym substitutions** from the `eponyms` argument
  (default: \[disease_eponyms\]).
- **Phrase-level regex substitutions** from the `patterns` argument
  (default: \[disease_cap_patterns\]), applied longest-first and
  overriding any conflicting eponym substitutions.

------------------------------------------------------------------------

## Known limitations

- **Unrecognized proper nouns** remain lowercase. Add missing eponyms to
  a custom `eponyms` vector, or submit a PR to extend
  \[disease_eponyms\].

- **Context-sensitive capitalization** (e.g. `SHORT` as an acronym vs
  `short` as an adjective) cannot be resolved by word-level `eponyms`.
  Use `patterns` with a phrase-level regex:

  ``` r
  parse_omim_name(
      "SHORT SYNDROME; SHORTSYN",
      patterns = c("short syndrome" = "SHORT syndrome")
  )
  #> # A tibble: 1 × 2
  #>   name           abbreviation
  #>   <chr>          <chr>       
  #> 1 SHORT syndrome SHORTSYN
  ```

- **Context-specific word substitutions** (e.g. `WITH` →
  `associated with`) cannot be inferred and require manual
  post-processing.

- **Qualifiers outside the forcing list** may not trigger rearrangement.
  The list covers the most common OMIM qualifier types; edge cases exist
  (submit an issue to extend it).
